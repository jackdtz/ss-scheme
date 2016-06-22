#lang racket

(require racket/set racket/stream)
(require "utilities.rkt")
(require "interp.rkt")

(provide int-exp-passes compile-R0)


(define compile-R0
 (class object%
  (super-new)

  (define primitive-set
   (set '+ '- 'read))

  (define/public uniquify
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

  (define/public flatten
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

   
   (define/public select-instructions
     (lambda (e)
       (match e
         [(? symbol?) `(var ,e)]
         [(? integer?) `(int ,e)]
         [`(return ,e) (select-instructions `((assign (reg rax) ,e)))]
         [`(assign ,lhs (read))
          `((callq read_int) (movq (reg rax) ,(select-instructions lhs)))]
         [`(assign ,lhs ,rhs) #:when (symbol? rhs)
                              (let ([new-lhs (select-instructions lhs)])
                                (cond [(equal? new-lhs `(var ,rhs)) '()]
                                      [else `((movq (var ,rhs) ,new-lhs))]))]
         [`(assign ,lhs ,rhs) #:when (integer? rhs)
                              (let ([new-lhs (select-instructions lhs)])
                                `((movq (int ,rhs) ,new-lhs)))]
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
         [`(program ,vars ,stms ...)
          (let ([new-stms (map (lambda (s) (select-instructions s)) stms)])
            `(program ,vars ,@(append* new-stms)))]
         [else (error "R0/instruction selection, unmatch " e)])))

  
  
  ))

;; Passes
(define int-exp-passes
  (let ([compiler (new compile-R0)]
        [interp (new interp-R0)])
    (list 
    `("uniquify" ,(send compiler uniquify '())
      ,interp-scheme)
     `("flatten" ,(send compiler flatten #f)
       ,interp-C)
    ; `("instruction selection" ,(send compiler select-instructions)
    ;   ,interp-x86)
    ; `("assign homes" ,(send compiler assign-homes (void))
    ;   ,interp-x86)
    ; `("insert spill code" ,(send compiler patch-instructions)
    ;   ,interp-x86)
    ; `("print x86" ,(send compiler print-x86) #f)
    )))


(define compiler (new compile-R0))

(define uniquify (send compiler uniquify '()))
(define flatten (send compiler flatten #t ))
(define inst-sel (send compiler select-instructions))
(define ass-homes (send compiler assign-homes (void)))
(define patch (send compiler patch-instructions))
(define x86 (send compiler print-x86))
  
(x86 (patch (ass-homes (inst-sel (flatten (uniquify '(program (+ 33 19))))))))



       


