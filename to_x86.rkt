#lang racket

(require "utilities.rkt")
(require "interp.rkt")

(define primitive-set
  (set '+ '- 'read))

(define instruction-set
  (set 'addq 'negq 'movq 'subq))

(define (align n alignment)
  (cond [(eq? 0 (modulo n alignment)) n]
        [else
         (+ n (- alignment (modulo n alignment)))]))
         
(define uniquify
  (lambda (env)
    (lambda (e)
      (match e
        [(? symbol?) (cdr (assq e env))]
        [(? integer?) e]
        [`(let ([,x ,(app (uniquify env) new-e)]) ,body)
         (let ([new-x (gensym x)])
           `(let ([,new-x ,new-e])
              ,((uniquify (cons `(,x . ,new-x) env)) body)))]
        [`(program ,e) `(program ,((uniquify env) e))]
        [`(,op ,es ...) #:when (set-member? primitive-set op)
                        `(,op ,@(map (lambda (e) ((uniquify env) e)) es))]
        [else (error "Uniquify could not match " e)]))))

(define flatten
  (lambda (need-temp)
    (lambda (e)
      (match e
        [(? symbol?) (values e '() '())]
        [(? integer?) (values e '() '())]
        [`(let ([,x ,e]) ,body)
         (let-values ([(new-e e-stms e-vars) ((flatten #f) e)]
                      [(new-body body-stms body-vars) ((flatten need-temp) body)])
           (values new-body
                   (append e-stms `((assign ,x ,new-e)) body-stms)
                   (cons x (append e-vars body-vars))))]
        [`(,op ,(app (flatten #t) new-es es-stms* es-vars*) ...)
         #:when (set-member? primitive-set op)         
         (let ([prim-exp `(,op ,@new-es)]
               [es-stms (append* es-stms*)]
               [es-vars (append* es-vars*)])
           (case need-temp
             [(#f) (values prim-exp es-stms es-vars)]
             [(#t) (let ([temp (gensym 'temp)])
                     (values temp 
                             (append es-stms `((assign ,temp ,prim-exp)))
                             (cons temp es-vars)))]))]
        [`(program ,e) 
         (let-values ([(e-exp e-stms e-vars) ((flatten #t) e)])
           `(program ,e-vars ,@(append e-stms `((return ,e-exp)))))]
        [else (error "Flatten could not match " e)]))))

(define bin-op->instr
  (lambda (op)
    (match op
      ['+ 'addq]
      [else
       (error "bin-op->instr could not match " op)])))

(define unary-op->instr
  (lambda (op)
    (match op
      ['- 'negq]
      [else
       (error "unary-op->instr could not match " op)])))


(define select-instructions
  (lambda (e)
    (match e
      [(? symbol?) `(var ,e)]
      [(? integer?) `(int ,e)]
      [`(reg ,r) e]
      [`(return ,e) (select-instructions `(assign (reg rax) ,e))]
      [`(assign ,lhs (read))
      `((callq read_int) (movq (reg rax) ,(select-instructions lhs)))]
      [`(assign ,lhs (,op ,e1 ,e2))
       (let ([new-lhs (select-instructions lhs)]
             [new-e1 (select-instructions e1)]
             [new-e2 (select-instructions e2)]
             [instr (bin-op->instr op)])
         (cond [(equal? new-lhs new-e1) `((,instr ,new-e2 ,new-lhs))]
               [(equal? new-lhs new-e2) `((,instr ,new-e1 ,new-lhs))]
               [else
                `((movq ,new-e1 ,new-lhs) (,instr ,new-e2 ,new-lhs))]))]
      [`(assign ,lhs (,op ,e))
       (let ([new-lhs (select-instructions lhs)]
             [new-e (select-instructions e)]
             [instr (unary-op->instr op)])
         (if (equal? new-lhs new-e)
             `((,instr new-lhs))
             `((movq ,new-e ,new-lhs) (,instr ,new-lhs))))]
      [`(assign ,lhs ,rhs) #:when (symbol? rhs)
                           (let ([new-lhs (select-instructions lhs)])
                             (cond [(equal? new-lhs `(var ,rhs)) '()]
                                   [else `((movq (var ,rhs) ,new-lhs))]))]
      [`(assign ,lhs ,rhs) #:when (integer? rhs)
                           (let ([new-lhs (select-instructions lhs)])
                             `((movq (int ,rhs) ,new-lhs)))]
      [`(program ,vars ,stms ...)
       (let ([new-stms (map (lambda (s) (select-instructions s)) stms)])
         `(program ,vars ,@(append* new-stms)))]
      [else (error "R0/instruction selection, unmatch " e)])))

(define assign-homes
  (lambda (hash)
    (lambda (e)
      (match e
        [`(program (,vars ...) ,stms ...)
         (define word-size 8)
         (define first-offset 8)
         (define (make-stack-loc n)
           `(deref rbp ,(- (+ first-offset
                             (* word-size n)))))
         (define hash-tbl
           (make-hash (map cons vars
                           (map make-stack-loc
                                (stream->list (in-range 0 (length vars)))))))
         (define stack-space
           (align (* (length vars) word-size)
                  16))
         
         `(program ,stack-space
                   ,@(map (assign-homes hash-tbl) stms))]
        [`(int ,i) e]
        [`(reg ,r) e]
        [`(callq read_int) e]
        [`(var ,x) (hash-ref hash x)]
        [`(,instr ,es ...)
         #:when (set-member? instruction-set instr)
         `(,instr ,@(map (assign-homes hash) es))]
        [else
         (error "assign-homes could not match " e)]))))


(define patch-instructions
 (lambda (e)

  (define in-memory
   (lambda (x)
    (match x
     [`(deref rbp ,n) #t]
     [else #f])))

  (match e
   [`(program ,frame-size ,instr ...)
    `(program ,frame-size ,@(map patch-instructions instr))]
   [`(,instr (deref rbp ,offset1) (deref rbp ,offset2))
    #:when (set-member? instruction-set instr)
    `((movq (deref rbp ,offset1) (reg rax)) (,instr (reg rax) (deref rbp ,offset2)))]
   [else e])))
  


(define compile 
  (lambda (e)
    (let* ([uniq ((uniquify '()) e)]
           [flat ((flatten #t) uniq)]
           [instrs (select-instructions flat)]
           [homes ((assign-homes void) instrs)]
           [patched (patch-instructions homes)])
      patched)))

(define passes
 (list
  `("uniquify"              ,(uniquify '())          ,interp-scheme)
  `("flatten"               ,(flatten #f)            ,interp-C)
  `("instruction selection" ,select-instructions     ,interp-x86)
  `("assign homes"          ,(assign-homes (void))   ,interp-x86)
  `("insert spill code"     ,patch-instructions      ,interp-x86)
 ))


(define compiler-list
  ;; Name           Typechecker               Compiler-Passes      Initial interpreter  Valid suites
  `(("int_exp"      #f                        ,passes               ,interp-scheme       "s0" ,(range 1 28))
    ;("reg_int_exp"  #f                        ,reg-int-exp-passes  ,interp-scheme       (0))
    ;("conditionals" ,conditionals-typechecker ,conditionals-passes ,interp-scheme       (0 1))
    ;("vectors"      ,vectors-typechecker      ,vectors-passes      ,interp-scheme       (0 1 2))
    ;("functions"    ,functions-typechecker    ,functions-passes    ,interp-scheme       (0 1 2 3))
    ;("lambda"       ,lambda-typechecker       ,lambda-passes       ,interp-scheme       (0 1 2 3 4))
    ;("any"          ,R6-typechecker           ,R6-passes           ,interp-scheme       (0 1 2 3 4 6))
    ;("dynamic"      #f                        ,R7-passes           ,(interp-r7 '())     (7))
    ))

(compile 
 '(program (let ([a 42])
            (let ([b a])
              b))))

(for ([test compiler-list])
 (apply interp-tests test))


