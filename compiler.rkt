#lang racket

(require data/heap)
(require "utilities/utilities.rkt")
(require "utilities/interp.rkt")

(define primitive-set
  (set '+ '- 'read))

(define instruction-set
  (set 'addq 'negq 'movq 'subq))

(define caller-save (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10))

(define general-registers (vector 'rbx 'rcx 'rdx 'rsi 'rdi
                                  'r8 'r9 'r10 'r12 
                                  'r13 'r14))

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

(define commutative?
  (lambda (op)
    (match op
      ['+ #t]
      [else #f])))


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
               [(and (commutative? op) (integer? e1) (symbol? e2))
                `((movq ,new-e2 ,new-lhs) (,instr ,new-e1 ,new-lhs))]
               [else
                `((movq ,new-e1 ,new-lhs) (,instr ,new-e2 ,new-lhs))]))]
      [`(assign ,lhs (,op ,e))
       (let ([new-lhs (select-instructions lhs)]
             [new-e (select-instructions e)]
             [instr (unary-op->instr op)])
         (if (equal? new-lhs new-e)
             `((,instr new-lhs))
             `((movq ,new-e ,new-lhs) (,instr ,new-lhs))))]
      [`(assign ,lhs ,rhs)
       #:when (symbol? rhs)
       (let ([new-lhs (select-instructions lhs)])
         (cond [(equal? new-lhs `(var ,rhs)) '()]
               [else `((movq (var ,rhs) ,new-lhs))]))]
      [`(assign ,lhs ,rhs)
       #:when (integer? rhs)
       (let ([new-lhs (select-instructions lhs)])
         `((movq (int ,rhs) ,new-lhs)))]
      [`(program ,vars ,stms ...)
       (let ([new-stms (map (lambda (s) (select-instructions s)) stms)])
         `(program ,vars ,@(append* new-stms)))]
      [else (error "R0/instruction selection, unmatch " e)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                      ;
;                                                      ;
;                 Register Allocation                  ;
;                                                      ;
;                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define free-var
  (lambda (e)
    (match e
      [`(var ,x) (set x)]
      [`(reg ,r) (set r)]
      [`(int ,i) (set)]
      [else (error "unhandled case " e)])))

(define write-vars
  (lambda (ast)
    (match ast
      [`(,instr ,src ,dst) #:when (set-member? instruction-set instr)
       (free-var dst)]
      [`(negq ,x) (free-var x)]
      [`(callq ,f) caller-save]
      [else (error "write-vars could not match " ast)])))

(define read-vars
  (lambda (ast)
    (match ast
      [`(movq ,src ,dst) (free-var src)]
      [(or `(addq ,src ,dst) `(subq ,src ,dst) `(imul ,src ,dst))
       (set-union (free-var src) (free-var dst))]
      [`(negq ,x) (free-var x)]
      [else
       (error "read-vars could not match " ast)])))


(define liveness
 (lambda (origin-live-after)
  (lambda (old-instrs)

   (define loop 
    (lambda (instrs live-after all-lives instrs-col)
      (cond [(null? instrs) (values instrs-col all-lives)]
            [else
              (let-values ([(new-instr new-live-after) ((uncover-live live-after) (car instrs))])
                (loop (cdr instrs) 
                      new-live-after 
                      (cons new-live-after all-lives) 
                      (cons new-instr instrs-col)))])))
   
   (loop (reverse old-instrs) origin-live-after (list origin-live-after) '()))))

(define uncover-live
  (lambda (live-after) 
   (lambda (e)
    (match e
     [`(program ,vars ,instrs ...) 
      (let-values ([(new-instrs new-live-after) ((liveness (set)) instrs)])
        `(program (,vars ,(cdr new-live-after)) ,new-instrs))]
     [else
      (values e (set-union (set-subtract live-after
                                         (write-vars e))
                           (read-vars e)))]))))


(define build-interference
  (lambda (live-after graph)
    (lambda (ast)
      (match ast
        
       [`(program (,vars ,lives) ,instrs)
        (let ([graph (make-graph vars)])
          (let ([new-instrs
                 (for/list ([inst instrs] [live-after lives])
                           ((build-interference live-after graph) inst))])
            `(program (,vars ,graph) ,@new-instrs)))]
        
        [`(movq ,src ,dst) 
         (begin
           (for ([v live-after])
                (for ([d (free-var dst)]
                      #:when (not (or (equal? d v) (equal? `(var ,v) src))))
                     (add-edge graph d v)))
           ast)]
        
        [`(callq ,label) 
         (begin 
           (for ([v live-after])
                (for ([r caller-save] #:when (not (equal? v r)))
                     (add-edge graph v r)))
           ast)]
        
        [else
         (begin
           (for ([v live-after])
                (for ([d (write-vars ast)] #:when (not (equal? v d)))
                     (add-edge graph v d)))
           ast)]))))



(define annotate
  (lambda (graph)
    (hash-for-each
     graph              
     (lambda (k v) (hash-set! graph k `(,v ,(set)))))
    graph))

(define lowest-missing-num
  (lambda (lst)
    (cond [(null? lst) 0]
          [(null? (cdr lst)) (if (= 0 (car lst)) 1 0)]
          [else
           (let ([diff 
                  (set-subtract (apply set (range 0 (+ 2 (apply max lst))))
                                (list->set lst))])
             (car (sort (set->list diff) <)))])))
           
          


(define node-adjs  (lambda (x) (cadr x)))

(define node-saturations (lambda (x) (caddr x)))

(define node-key car)

(define heap->hash
  (lambda (h)
    (make-hash (vector->list (heap->vector h)))))

(define heap-contains?
  (lambda (heap key)
    (define hash (make-hash (vector->list (heap->vector heap))))

    (hash-has-key? hash key)))

(define hash->heap
  (lambda (h)
    (define node-heap
      (make-heap (lambda (n1 n2)
                   (>= (set-count (node-saturations n1))
                       (set-count (node-saturations n2))))))
    (heap-add-all! node-heap (hash->list h))
    node-heap))


(define update-heap-saturation!
  (lambda (heap key new-color)
    (let* ([hash (heap->hash heap)]
           [adjs (car (hash-ref hash key))])
      (set-for-each adjs
                    (lambda (var)
                      (let ([adj (car (hash-ref hash var))]
                            [sat (cadr (hash-ref hash var))])
                        (hash-set! hash var `(,adj ,(set-add sat new-color))))))
      (hash->heap hash))))
  
      
    


(define (color-graph graph)
  (lambda (vars)

    (define update-adjs-saturations!
      (lambda (node-heap adjs col)
        (set-for-each adjs
                      (lambda (var)
                        (let ([adj (car (hash-ref graph var))]
                              [sat (cadr (hash-ref graph var))])
                          (hash-set! graph var `(,adj ,(set-add sat col)))
                          (cond [(heap-contains? node-heap var)
                                 (begin
                                   (heap-remove! node-heap `(,var . (,adj ,sat)))
                                   (heap-add! node-heap `(,var . (,adj ,(set-add sat col)))))]))))))
        

    (let loop ([node-heap (hash->heap graph)]
               [map-col (make-hash)])
      (if (= 0 (heap-count node-heap))
          map-col
          (let* ([node (heap-min node-heap)]
                 [char (node-key node)]
                 [adjs (node-adjs node)]
                 [saturations (node-saturations node)])
            (let ([color (lowest-missing-num (set->list saturations))])
              (hash-set! map-col char color)
              (heap-remove-min! node-heap)
              (update-adjs-saturations! node-heap adjs color)
              
              (loop node-heap map-col)))))))
                         

(define reg-spill
  (lambda (color-map)
    
    (let ([reg-len (vector-length general-registers)]
          [word-size 8]
          [number-of-spill 0])
      (values
       (make-hash
        (map
         (lambda (col-map)
           `(,(car col-map)
             ,@(cond [(< (cdr col-map) reg-len)
                      `(reg ,(vector-ref general-registers (cdr col-map)))]
                     [else
                      ;update stack-size
                      (cond [(> (cdr col-map) number-of-spill)
                             (set! number-of-spill (cdr col-map))])
                          
                      `(deref rbp ,(- (* word-size
                                         (+ 1 (- (cdr col-map) reg-len)))))])))
         (hash->list color-map)))
       (* word-size number-of-spill)))))
    
    
      
      

; (program (vars graph) instrs) -> (program (vars) colored-instrs)
; color-map : var -> register / stack location
(define allocate-registers
  (lambda (color-map)
    (lambda (ast)
      (match ast
        [`(program (,vars ,graph) ,instrs ...)
         (let* ([annot-graph (annotate graph)]
                [color-map ((color-graph annot-graph) vars)])
           (let-values ([(reg-map stk-size) (reg-spill color-map)])
             `(program ,stk-size ,@(map (lambda (instr) ((allocate-registers reg-map) instr)) instrs))))]
        [`(var ,x) (hash-ref color-map x)]
        [`(int ,i) `(int ,i)]
        [`(reg ,r) `(reg ,r)]
        [`(callq ,f) `(call ,f)]
        [`(,instr ,src ,dst)
         #:when (set-member? instruction-set instr)
         `(,instr ,((allocate-registers color-map) src)
                  ,((allocate-registers color-map) dst))]
        [`(,instr ,dst)
          #:when (set-member? instruction-set instr)
         `(,instr ,((allocate-registers color-map) dst))]
        [else (error "allocate-registers could not match " ast)]))))
                             
    




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

  (define in-memory?
   (lambda (x)
    (match x
     [`(deref rbp ,n) #t]
     [else #f])))

  (match e
    [`(program ,frame-size ,instr ...)
    `(program ,frame-size ,@(append* (map patch-instructions instr)))]
    [`(movq ,src ,dst)
     (cond [(equal? src dst) '()]
           [(and (in-memory? src) (in-memory? dst))
            `((movq ,src (reg rax)) (movq (reg rax) ,dst))]
           [else `((movq ,src ,dst))])]    
    [`(,instr ,src ,dst)
     #:when (set-member? instruction-set instr)
     (cond [(and (in-memory? src) (in-memory? dst))
            `((movq ,src (reg rax)) (,instr (reg rax) ,dst))]
           [else `((,instr ,src ,dst))])]
    
    [else `(,e)])))
  
  
(define print-x86
  (lambda (e)
    (match e
      [`(deref ,reg ,r) (format "~a(%~a)" r reg)]
      [`(int ,n) (format "$~a" n)]
      [`(reg ,r) (format "%~a" r)]
      [`(callq ,f)
       (format "\tcallq\t~a\n" (label-name (symbol->string f)))]
      [`(,instr ,src ,dst)
       #:when (set-member? instruction-set instr)
       (format "\t~a\t~a, ~a\n" instr 
                                (print-x86 src)
                                (print-x86 dst))]
      [`(,instr ,dst)
       #:when (set-member? instruction-set instr)
       (format "\t~a\t~a\n" instr (print-x86 dst))]
      
      [`(program ,stack-space ,instrs ...)
       (string-append
         (format "\t.globl ~a\n" (label-name "main"))
         (format "~a:\n" (label-name "main"))
         (format "\tpushq\t%rbp\n")
         (format "\tmovq\t%rsp, %rbp\n")
         (format "\tsubq\t$~a, %rsp\n" stack-space)
         "\n"
         (string-append* (map print-x86 instrs))
         "\n"
         (format "\tmovq\t%rax, %rdi\n")
         (format "\tcallq\t~a\n" (label-name "print_int"))
         (format "\tmovq\t$0, %rax\n")
         (format "\taddq\t$~a, %rsp\n" stack-space)
         (format "\tpopq\t%rbp\n")
         (format "\tretq\n"))]
      
      [else (error "print-x86 unmatched " e)])))


(define compile 
  (lambda (e)
    (let* ([uniq ((uniquify '()) e)]
           [flat ((flatten #t) uniq)]
           [instrs (select-instructions flat)]
           [liveness ((uncover-live (void)) instrs)]
           [graph ((build-interference (void) (void)) liveness)]
           [reg-alloc ((allocate-registers (void)) graph)]
           [patched (patch-instructions reg-alloc)]
           [x86 (print-x86 patched)])
      (define out (open-output-file #:exists 'replace 
                                    "assembly/output.s"))
      (display x86 out)
      (close-output-port out)
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
 '(program
   (let ([v 1])
     (let ([w 46])
       (let ([x (+ v 7)])
         (let ([y (+ 4 x)])
           (let ([z (+ x w)])
             (+ z (- y)))))))))



;(compile '(program (+ 13 22)))

(for ([test compiler-list])
 (apply interp-tests test))


