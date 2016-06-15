#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                          ;
;                                                          ;
;                                                          ;
;                       Utilities                          ;
;                                                          ;
;                                                          ;
;                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define nested?
  (lambda (x)
    (match x
      [(? atom?) #f]
      [else #t])))

(define replace
  (lambda (source old new)
    (cond [(null? source) '()]
          [(not (atom? (car source)))
           (cons (replace (car source) old new) (replace (cdr source) old new))]
          [(equal? (car source) old)
           (cons new (replace (cdr source) old new))]
          [else
           (cons (car source) (replace (cdr source) old new))])))
           
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                          ;
;                                                          ;
;                                                          ;
;                       Main code                          ;
;                                                          ;
;                                                          ;
;                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define uniquify
  (lambda (alist)
    (lambda (e)
      (letrec ([lookup (lambda (table key)
                         (cond [(null? table) (error "key is not defined in table")]
                               [(equal? key (caar table)) (cdr (car table))]
                               [else (lookup (cdr table) key)]))])
      (match e
        [(? symbol?) (lookup alist e)]
        [(? integer?) e]
        [`(let ([,x ,e] ...) ,body) 
         (let* ([new-x (map gensym x)]
                [new-alist (append (map (lambda (t1 t2) (cons t1 t2)) x new-x))]
                [new-e (map (lambda (ee) ((uniquify new-alist) ee)) e)]
                [new-vars (map (lambda (x e) (list x e)) new-x new-e)])
           `(let ,new-vars ,((uniquify new-alist) body)))]
        [`(program ,e) `(program ,((uniquify alist) e))]
        [`(,op ,es ...) `(,op ,@(map (lambda (e) ((uniquify alist) e)) es))])))))

(define flatten
 (lambda (prog)
   (letrec ([x 0]
            [newtemp (lambda ()
                       (begin (set! x (+ x 1))
                              (string->symbol (string-append "temp." (number->string x)))))]
            [first-pass (lambda (prog)                                             
                          (match prog
                            [(? symbol?) (values '() prog)]
                            [(? integer?) (let ([ret-temp (newtemp)]) (values `((assign ,ret-temp ,prog)) ret-temp))]
                            [`(- ,e) (let ([ret-temp (newtemp)]) (values `((assign ,ret-temp ,prog)) ret-temp))]
                            [`(+ ,e1 ,e2) (cond [(and (not (nested? e1)) (not (nested? e2)))
                                                 (let ([ret-temp (newtemp)])
                                                   (values `((assign ,ret-temp ,prog)) ret-temp))]
                                                [(and (nested? e1) (not (nested? e2)))
                                                 (let-values ([(stms temp) (first-pass e1)])
                                                   (let ([ret-temp (newtemp)])
                                                     (values `(,@stms (assign ,ret-temp (+ ,temp ,e2))) ret-temp)))]
                                                [(and (not (nested? e1)) (nested? e2))
                                                 (let-values ([(stms temp) (first-pass e2)])
                                                   (let ([ret-temp (newtemp)])
                                                     (values `(,@stms (assign ,ret-temp (+ ,e1 ,temp))) ret-temp)))]
                                                 [(and (nested? e1) (nested? e2))
                                                  (let-values ([(stms1 temp1) (first-pass e1)]
                                                               [(stms2 temp2) (first-pass e2)])
                                                    (let ([ret-temp (newtemp)])
                                                      (values `(,@stms1 ,@stms2 (assign ,ret-temp (+ ,temp1 ,temp2))) ret-temp)))])]
                            [`(let ([,var ,exp]) ,body)
                             (let ([stms-temp (if (not (nested? exp))
                                                    `((assign ,var ,exp))                                                   
                                                    (let-values ([(stms ret-ftemp) (first-pass exp)])
                                                      (replace stms ret-ftemp var)))])
                               (let-values ([(stms* ret-temp*) (first-pass body)])
                               (values `(,@stms-temp ,@stms*) ret-temp*)))]))]                    
            [collect-vars (lambda (stm-list)
                            (foldr (lambda (stm col)
                                     (match stm
                                       [`(assign ,var ,exp) (if (memq var col)
                                                                col
                                                                (cons var col))]
                                       [`(return ,e) col]
                                       [else (error prog)]))
                                   '()
                                   stm-list))])
     (match prog
       [`(program ,e)
        (let-values ([(stms ret-temp) (first-pass e)])
          (let ([vars (collect-vars stms)])
            `(prog ,vars (,@stms (return ,ret-temp)))))]
       [else
        (error "should not happend")]))))

 (define select-instructions
   (lambda (prog)
     (letrec ([iterate (lambda (stms instr-select)
                         (if (null? stms)
                           '()
                           (cons (instr-select (car stms)) (iterate (cdr stms) instr-select))))]
              [instr-select (lambda (stm)
                              (match stm
                                     [`(assign ,e1 (+ ,n1 ,n2)) (cond [(and (integer? n1) (integer? n2))
                                                                        `((movq (int ,n1) (var ,e1)) (addq (int ,n2) (var ,e1)))]
                                                                      [(and (integer? n1) (symbol? n2) 
                                                                          `((addq (int ,n1) (var ,n2))))]
                                                                      [(and (symbol? n1) (integer? n2))
                                                                        `((addq (int ,n2) (var ,n1)))]
                                                                      [(and (symbol? n1) (symbol? n2)) 
                                                                        `((addq (var ,n1) (var ,n2)))])]
                                     [`(assign ,e1 (read)) `((callq read_int) (movq (reg rax) (var ,e1)))]
                                     [`(assign ,e1 ,e2) `((movq (var ,e2) (var ,e1)))]
                                     [`(return ,e) `((movq (reg rax) (var ,e)))]))])
     (match prog
       [`(prog ,vars ,stms) `(prog ,vars ,(iterate stms instr-select))]
       [else (error "Unkown prog in select-instruction")]))))

       
(define compile
  (lambda (prog)
    (select-instructions (flatten ((uniquify '()) prog)))))

(define compile1
  (lambda (prog)
    (flatten ((uniquify '()) prog))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                          ;
;                                                          ;
;                                                          ;
;                         Test                             ;
;                                                          ;
;                                                          ;
;                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
(compile '(program
           (let ([a 42])
             (let ([b a])
               b))))

(compile '(program
           (let ([x 32])
             (+ (let ([x 10]) x) x))))

(compile1 '(program
           (let ([x 32])
             (+ (let ([x 10]) x) x))))

(compile '(program
  (let ([x (let ([x 4])
             (+ x 1))])
    (+ x 2))))



