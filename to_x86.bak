#lang racket

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define map2
  (lambda (lst1 lst2 proc)
    (if (not (= (length lst1)
                (length lst2)))
        (error "map2 unmatch list length")
        (if (null? lst1)
            '()
            (cons (proc (car lst1) (car lst2)) (map2 (cdr lst1) (cdr lst2) proc))))))
         

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



(define nested?
  (lambda (x)
    (match x
      [(? atom?) #f]
      [else #t])))

(define flatten
 (lambda (prog)
   (letrec ([x 0]
            [newtemp (lambda ()
                       (begin (set! x (+ x 1))
                              (string->symbol (string-append "temp." (number->string x)))))]
            [first-pass (lambda (prog)                                             
                          (match prog
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
                             (let ([stms-temp (match exp
                                                (if (not (nested? exp))
                                                    `((assign ,var ,exp))                                                   
                                                    (let-values ([(stms ret-ftemp) (first-pass exp)])
                                                      stms)))])
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
     (let-values ([(stms ret-temp) (first-pass prog)])
       (let ([vars (collect-vars stms)])
         `(prog ,vars (,@stms (return ,ret-temp))))))))

       
       
(flatten `(let ([x 11])
           (+ x 41)))

((uniquify '()) '(program
                  (let ([x 32])
                    (+ (let ([x 10]) x) x))))

((uniquify '()) '(program
                  (let ([x (let ([x 4])
                             (+ x 1))])
                     (+ x 2))))

((uniquify '()) '(program
                  (let ([y 3]
                        [x (let ([x 4])
                             (+ x 1))]
                        
                        [z 9])
                    (+ (+ x 2) x))))

