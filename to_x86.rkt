#lang racket

(define primitive-set
(set '+ '- 'read))

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
  (lambda (e)
    (letrec ([stack-index 0]
             [new-index (lambda () (begin (set! stack-index (- stack-index 8)) stack-index))]
             [helper 
              (lambda (e env)
                (match e
                  [`(program ,vars ,stms ...)
                   (let-values ([(new-stms new-env) 
                     `(program ,vars ,@new-stms))]
                  [`(int ,i) (values e env)]
                  [`(reg ,r) (values e env)]
                  [`(var ,x) (if (assq x env)
                                 (values (cdr (assq x env)) env)
                                 (let ([mem-def `(deref rbp ,(new-index))])
                                  (values mem-def (cons `(,x ,mem-def) env))))]
                  [`(,un-op ,dst) `(,un-op ,(helper dst))]
                  [`(,bin-op ,src ,dst) 
                   (let*-values ([(new-src new-env) (helper src env)]
                                 [(new-dst new-env*) (helper dst new-env)])
                     (values `(,bin-op ,new-src ,new-dst) new-env*))]))])
      (let-values ([(stms env) (helper e '())])
        stms))))

(define compile 
  (lambda (e)
    (let* ([uniq ((uniquify '()) e)]
           [flat ((flatten #t) uniq)]
           [instrs (select-instructions flat)]
           [homes (assign-homes instrs)])
      homes)))



; '(program (temp313 temp312) 
;     (movq (int 10) (var temp312)) 
;     (negq (var temp312)) 
;     (movq (int 52) (var temp313)) 
;     (addq (var temp312) (var temp313)) 
;     (movq (var temp313) (reg rax)))

; '(program (temp378 temp377) 
;           (movq (int 10) (deref rbp -8)) 
;           (negq (deref rbp -16)) 
;           (movq (int 52) (deref rbp -24)) 
;           (addq (deref rbp -32) (deref rbp -40)) 
;           (movq (deref rbp -48) (reg rax)))


(compile '(program (+ 52 (- 10))))

; (foldl + 0 '(1 2 3 4 5))