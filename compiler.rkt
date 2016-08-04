#lang racket

(require data/heap)
(require "utilities/utilities.rkt")
(require "utilities/interp.rkt")
(require "utilities/runtime-config.rkt")

(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities for the whole compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define para-registers
  (list 'rdi 'rsi 'rdx 'rcx 'r8 'r9))

(define cmp-instructions
  (set 'eq? '< '<= '> '>=))

(define logic-instructions
  (set 'and 'or 'not))

(define primitive-set
  (set-union
   (set '+ '- 'read       
        'and 'or 'not)
   cmp-instructions
   logic-instructions))

(define vec-primitive-set
  (set 'vector 'vector-ref
       'vector-set!))

(define instruction-set
  (set 'addq 'negq 'movq 'subq 'movzbq 'cmpq 'xorq 'andq 'leaq))

(define prettify-fun-type
  (lambda (type)
    (if (is-function? type)
        type
        `(,@(car type) -> ,(cdr type)))))
  
(define member-of? hash-has-key?)
  
(define look-up
  (lambda (env key)
    (if (hash-has-key? env key)
        (hash-ref env key)
        (error "no value found for key" env key))))

(define add-env
  (lambda (env key val)
    (if (hash-has-key? env key)
        (error "duplicate key in env" key env)
        (hash-set env key val))))

(define type-check-lookup
  (lambda (env key)
    (if (hash-has-key? env key)
        (hash-ref env key)
        '())))

(define is-function?
  (lambda (x)
    (match x
      [`(,args ... -> ,ret) #t]
      [else #f])))


(define type-check-add-env
  (lambda (env key val)
    (if (hash-has-key? env key)
        (hash-set env key (cons val (hash-ref env key)))
        (hash-set env key (list val)))))

(define type-check-add-f-env
  (lambda (env fname type)
    (if (hash-has-key? env fname)
        (error "duplicated function in program: " fname)
        (hash-set env fname type))))

(define type-integer? (lambda (x) (equal? x 'Integer)))
(define type-boolean? (lambda (x) (equal? x 'Boolean)))
(define same-type? (lambda (x y) (equal? x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; type-check ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this pass checks if there is any type error in the program.
; it also annotate each node with it's type information
;
; 3           -------> (has-type 3 Integer)
; (and #t #f) -------> (has-type (and (has-type #t Boolean) (has-type #f Boolean)) Boolean)

(define type-check
  (lambda (env fenv)
    (lambda (ast)
      (define recur (type-check env fenv))

      (match ast
        [`(program ,(and fun-defs
                         `(define (,fun-names [,vars : ,arg-types] ...) : ,ret-types ,fbody))  ... ,body)
         
         (define types (map (lambda (func-arg-types ret-type)
                              `(,@func-arg-types -> ,ret-type))
                            arg-types ret-types))
      
         (define fenv* (foldl (lambda (fname type init)
                               (type-check-add-f-env init fname type))
                             (hash) fun-names types))
         (define env* (hash))
         (let-values ([(body-e body-type) ((type-check env* fenv*) body)])
           `(program (type ,body-type) ,@(map (lambda (def) ((type-check env* fenv*) def)) fun-defs) ,body-e))]
        
        ['(read) (values `(has-type (read) Integer) 'Integer)]
        [(? fixnum?) (values `(has-type ,ast Integer) 'Integer)]
        [(? boolean?) (values `(has-type ,ast Boolean) 'Boolean)]

        [(? symbol?)
         (let ([val (type-check-lookup env ast)])
           (cond [(not (null? val)) 
                  (define ty (car val))
                  (values `(has-type ,ast ,ty) ty)]
                 [else
                  (let ([f-type (type-check-lookup fenv ast)])
                    (unless (not (null? f-type))
                      (error (format "undefined variable ~a " ast)))
                    (values `(has-type ,ast ,f-type) f-type))]))]

        ; compare operations
        [`(,logic-op ,e1 ,e2)
         #:when (set-member? logic-instructions logic-op)
         (define-values (e1* e1-ty) (recur e1))
         (define-values (e2* e2-ty) (recur e2)) 
         (match* (e1-ty e2-ty)
           [('Boolean 'Boolean)
            (values `(has-type (,logic-op ,e1* ,e2*) Boolean) 'Boolean)]
            [(_ _) 
             (error
              (format "type-check: ~a expects two operands: ~a(type:~a) and ~a(type:~a) to have the same type 'Boolean (ast: ~a"
                      logic-op e1* e1-ty e2* e2-ty ast))])]
        
        [`(let ([,var ,e]) ,body)
         (define-values (var-e var-type) (recur e))
         
         (define env*
           (if (is-function? var-type)
               env
               (type-check-add-env env var var-type)))
         
         (define fenv*
           (if (is-function? var-type)
               (type-check-add-f-env fenv var var-type)
               fenv))
         
         (let-values ([(body-e body-ty) ((type-check env* fenv*) body)])
           (values `(has-type (let ([,var ,var-e]) ,body-e) ,body-ty)
                   body-ty))]
        
        [`(not ,(app recur e type))
         (match type
           ['Boolean (values `(has-type (not ,e) Boolean) 'Boolean)]
           [else (error "type-check: 'not expects a Boolean" ast type)])]
        
        [`(- ,(app recur e type))
         (match type
           ['Integer (values `(has-type (- ,e) Integer) 'Integer)]
           [else (error "type-check: '- expects an Integer" ast type)])]
        
        [`(+ ,(app recur e1 e1-ty)
             ,(app recur e2 e2-ty))
         (match* (e1-ty e2-ty)
           [('Integer 'Integer) (values `(has-type (+ ,e1 ,e2) Integer) 'Integer)]
           [('Integer _)
            (error "type-check: '+ expects e2 to be an Integer" ast e2)]
           [(_ 'Integer)
            (error "type-check: '+ expects e1 to be an Integer" ast e1)]
           [(_ _) 
            (error "type-check: '+ expects e1, e2 to be Integer" ast e1 e2)])]

        [`(eq? ,(app recur e1* e1-ty)
               ,(app recur e2* e2-ty))
         (match* (e1-ty e2-ty)
           [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
            (values `(has-type (eq? ,e1* ,e2*) Boolean) 'Boolean)]
           [(_ _)
            (unless (equal? e1-ty e2-ty)
              (error "type-check: eq? expect two operands have the same type" ast e1* e2*))
            (values `(has-type (eq? ,e1* ,e2*) Boolean) 'Boolean)])]
        [`(,cmp-op ,e1 ,e2)
         #:when (set-member? cmp-instructions cmp-op)
         (define-values (e1* e1-ty) (recur e1))
         (define-values (e2* e2-ty) (recur e2)) 
         (match* (e1-ty e2-ty)
           [('Integer 'Integer)
            (values `(has-type (,cmp-op ,e1* ,e2*) Boolean) 'Boolean)]
            [(_ _) 
             (error
              (format "type-check: ~a expects two operands: ~a(type:~a) and ~a(type:~a) to have the same type Integer (ast: ~a"
                      cmp-op e1* e1-ty e2* e2-ty ast))])]
        
        [`(if ,cmp ,t ,f) 
         (let-values ([(e-cmp type-cmp) (recur cmp)]
                      [(e-t type-t) (recur t)]
                      [(e-f type-f) (recur f)])
           (match type-cmp
             ['Boolean (match (equal? type-t type-f)
                         [#t (values `(has-type (if ,e-cmp ,e-t ,e-f) ,type-t) type-t)]
                         [else
                          (error (format "type-check: (thn:~a) and (els:~a) from ~a have different type ~a and ~a" 
                                  t f ast type-t type-f))])]
             [else
              (error (format "type-check: condition expression ~a from ~a expects a boolean type, but get type ~a" 
                                  cmp ast type-cmp))]))]
        
        ; Vector
        [`(void) (values `(has-type (void) Void) 'Void)]
        [`(vector ,(app recur e* t*)...)
         (let ([t `(Vector ,@t*)])
           (values `(has-type (vector ,@e*) ,t) t))]  
        [`(vector-ref ,(app recur e ty) ,ind)
         (match ty
           [`(Vector ,ts ...)
            (unless (and (exact-nonnegative-integer? ind)
                         (< ind (length ts)))
              (error 'type-check "invalid index ~a[~a]" ast ind))
            (let ([ret-ty (list-ref ts ind)])
              (values `(has-type (vector-ref ,e (has-type ,ind Integer)) ,ret-ty)
                      ret-ty))]
           [else
            (error "expected a vector type in vector-ref, not" ty)])]
        [`(vector-set! ,(app recur vec-e vec-ty) ,ind ,(app recur val-e val-ty))
         (match vec-ty
           [`(Vector ,ts ...)
            (unless (and (exact-nonnegative-integer? ind)
                         (< ind (length ts)))
              (error 'type-check "invalid index ~a[~a]" ast ind))
            (define expected-type (list-ref ts ind))
            (unless (equal? expected-type val-ty)
              (error (format "type mismatch in ~a, expect type ~a, but get ~a " ast expected-type val-ty)))
            (values `(has-type (vector-set! ,vec-e
                                            (has-type ,ind Integer)
                                            ,val-e)
                               Void)
                    'Void)]
           [else
            (display (format "vector: ~a" vec-ty))
            (error (format "expected a vector type in vector-set!, not ~a in ~a" vec-ty ast))])]
        
        ; function
        [`(define (,fun-name [,vars : ,types] ...) : ,ret-type ,body)
         
         (define updated-env-fenv
           (foldl (lambda (var type env-fenv)

                    (define env (car env-fenv))
                    (define fenv (cdr env-fenv))
                    (match type
                      [`(,args ... -> ,ret) (cons env (type-check-add-f-env fenv var type))]
                      [else (cons (type-check-add-env env var type) fenv)]))
                  (cons env fenv) vars types))

         (define env* (car updated-env-fenv))
         (define fenv* (cdr updated-env-fenv))
         
         (define-values (body-e body-type) ((type-check env* fenv*) body))
         (unless (equal? ret-type body-type)
           (error (format "body type: ~a and return type: ~a mismatch for function ~a\n current env: ~a \n current fenv: ~a"
                          body-type ret-type fun-name env fenv)))
         (define args (map (lambda (var type) `(,var : ,type)) vars types))
         `(define (,fun-name ,@args) : ,ret-type ,body-e)]
        
        [`(,fun-name ,pargs ...) 
         #:when (not (set-member? primitive-set fun-name))
         (define-values (fun-e fun-type) (recur fun-name))
         
         (unless (not (null? fun-type))
           (error (format "funtion ~a is not defined.\n ast: ~a\n env: ~a\n fenv: ~a" fun-name ast env fenv)))

         (match fun-type
           [`(,arg-types ... -> ,ret-type)
            (define-values (parg-e parg-types) (map2 (lambda (parg) ((type-check env fenv) parg)) pargs))
            
            (for/list ([arg-type arg-types]
                       [ptype parg-types])
                      (unless (equal? arg-type ptype)
                        (error (format "unmatch argument type in ~a, expect ~a but got ~a" fun-name arg-type ptype))))
            (values `(has-type (,fun-e ,@parg-e) ,ret-type) ret-type)]
           [else
            (error "function type error" ast)])]
        ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;; uniquify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this pass make sure that each let, function name, function parameter 
; uses a unique variable name

; (let ([x 3])              (let ([x.1 3])
;   (+ x 1))        =>        (+ x.1 1))

(define uniquify
  (lambda (env)
    (lambda (e)
      (define recur (uniquify env))
      
      (match e
        [`(has-type ,es ,t) `(has-type ,(recur es) ,t)]
        [(? symbol?) (cdr (assq e env))]
        [(? integer?) e]
        [(? boolean?) e]
        ['(void) '(void)]
        [`(if ,cnd ,thn ,els)
         `(if ,(recur cnd)
              ,(recur thn)
              ,(recur els))]
        [`(let ([,x ,(app recur new-e)]) ,body)
         (let ([new-x (gensym x)])
           `(let ([,new-x ,new-e])
              ,((uniquify (cons `(,x . ,new-x) env)) body)))]
        
        ; main program
        [`(program (type ,t) ,(and fun-defs `(define (,fun-names ,args ...) : ,ret ,fbody)) ... ,e)
         (define f-temps (map (lambda (f) (cons f (gensym f))) fun-names))
         (define new-env
           (foldl (lambda (f-temp env)
                    (cons f-temp env))
                 env f-temps))
         `(program (type ,t) ,@(map (lambda (def) ((uniquify new-env) def)) fun-defs)
                   ,((uniquify new-env) e))]
        
        [`(define (,fun-name (,vars : ,types) ...) : ,ret-type ,fbody)
         (define var-temps (map (lambda (var) (gensym var)) vars))
         (define new-env
           (foldl (lambda (var new-temp env)
                    (cons `(,var . ,new-temp) env))
                  env vars var-temps))
         (define new-arg-types (map (lambda (temp type) `(,temp : ,type)) var-temps types))
        `(define (,(recur fun-name) ,@new-arg-types) : ,ret-type ,((uniquify new-env) fbody))]
         
        [`(,op ,es ...) #:when (or (set-member? primitive-set op)
                                   (set-member? vec-primitive-set op))
                        `(,op ,@(map (lambda (e) (recur e)) es))]
        [`(,fname ,es ...)
         `(,(recur fname) ,@(map (lambda (e) (recur e)) es))]
        [else (error "Uniquify could not match " e)]))))


(define reveal-functions
  (lambda (funs)
    (lambda (e)
      (define recur (reveal-functions funs))
      (match e
        [`(program (type ,t) ,(and fun-defs `(define (,fun-names ,args ...) : ,rets ,fbody)) ... ,body)
         (define all-fun-types (map (lambda (fun-name ret-type) (cons fun-name ret-type)) fun-names rets))
         (define fun-env (foldl (lambda (fun-type init)
                                  (add-env init (car fun-type) (cdr fun-type)))
                                (hash) all-fun-types))
         `(program (type ,t) ,@(map (reveal-functions fun-env)  fun-defs) ,((reveal-functions fun-env) body))]
        [(? symbol?) (if (member-of? funs e) `(function-ref ,e) e)]
        [(? integer?) e]
        [(? boolean?) e]
        [`(void) e]
        [`(has-type ,e ,t) `(has-type ,(recur e) ,t)]
        [`(if ,(app recur cnd) 
              ,(app recur thn)
              ,(app recur els))
         `(if ,cnd ,thn ,els)]
        [`(and ,(app recur e1)
               ,(app recur e2))
         `(and ,e1 ,e2)]
        [`(,op ,es ...) #:when (or (set-member? vec-primitive-set op)
                                   (set-member? primitive-set op))
                        (define new-es (map recur es))
                        `(,op ,@new-es)]
        [`(,(and fname `(has-type ,e ,t)) ,es ...)
         `(app ,(recur fname) ,@(map recur es))]
        [`(let ([,x ,(app recur rhs)]) 
            ,(app recur body))
         `(let ([,x ,rhs]) ,body)]
        [`(define (,fun-name ,args ...) : ,ret-type ,fbody)
         `(define (,fun-name ,@args) : ,ret-type ,(recur fbody))]
        [else
         (error "in reveal-functions, unmatched" e)]))))
  

(define expose-allocation
  (lambda (e)
    (match e

      [`(program (type ,ty) ,fun-defs ... ,body)
       `(program (type ,ty) ,@(map expose-allocation fun-defs)  ,(expose-allocation body))]
      [`(function-ref ,name) e]
      [`(define (,fun-name ,args ...) : ,ret-type ,fbody)
       `(define (,fun-name ,@args) : ,ret-type ,(expose-allocation fbody))]
      [`(app ,es ...)
       `(app ,@(map expose-allocation es))]      
      [`(has-type (vector ,(app expose-allocation e*) ...) ,vec-type)
       (define len (length e*))
       (define size (* 8 (+ len 1)))
       (define vec (gensym 'alloc.))
       (define x* (map (lambda (e) (gensym 'vec-elt.)) e*))
       (define init-vec (foldr
                         (lambda (loc elt init)
                           (define v (gensym 'initrec.))
                           `(let ([,v (has-type (vector-set! ,vec ,loc ,elt) Void)])
                              ,init))
                         vec (range len) x*))
       (define voidy (gensym 'collectret.))
       (define alloc-init-vec
         `(has-type
           (let ([,voidy
                  (has-type
                   (if (has-type
                        (< (has-type
                            (+ (has-type (global-value free_ptr) Integer)
                               (has-type ,size Integer))
                            Integer)
                           (has-type (global-value fromspace_end) Integer))
                        Boolean)
                       (has-type (void) Void)
                       (has-type (collect ,size) Void))
                   Void)])
             (has-type
              (let ([,vec (has-type (allocate ,len ,vec-type) ,vec-type)])
                ,init-vec)
              ,vec-type))
           ,vec-type))
       (foldr
        (lambda (x e init)
          `(has-type (let ([,x ,e])
                       ,init)
                     ,vec-type))
        alloc-init-vec x* e*)]
      [`(has-type ,(app expose-allocation e) ,t)
       `(has-type ,e ,t)]
      [(? symbol?) e]
      [(? integer?) e]
      [(? boolean?) e]
      [`(void) e]
      [`(if ,(app expose-allocation cnd) 
            ,(app expose-allocation thn)
            ,(app expose-allocation els))
       `(if ,cnd ,thn ,els)]
      [`(and ,(app expose-allocation e1)
             ,(app expose-allocation e2))
       `(and ,e1 ,e2)]
      [`(,op ,es ...) #:when (or (set-member? vec-primitive-set op)
                                 (set-member? primitive-set op))
                      (define new-es (map expose-allocation es))
                      `(,op ,@new-es)]
      
      [`(let ([,x ,(app expose-allocation rhs)]) 
          ,(app expose-allocation body))
       `(let ([,x ,rhs]) ,body)]
      
      [else
       (error "in expose-allocation, unmatched" e)])))
                      
(define flatten
  (lambda (need-temp)
    (lambda (e)
      
      (define (flatten-if if-type thn-temp thn-stms els-temp els-stms vars)
        (lambda (cnd)
          (match cnd
            [`(has-type ,cnd ,t)
             (match cnd
               [#t (values thn-temp thn-stms vars)]
               [#f (values els-temp els-stms vars)]
               [`(not ,e) ((flatten-if if-type els-temp els-stms thn-temp thn-stms vars) e)]
               [`(and ,e1 ,e2)
                ((flatten-if if-type thn-temp thn-stms els-temp els-stms vars)
                 `(has-type (if (has-type (not ,e1) Boolean)
                                (has-type #f Boolean)
                                ,e2)
                            Boolean))]
               [`(,cmp ,e1 ,e2) #:when (set-member? cmp-instructions cmp) 
                                (let-values ([(new-e1 e1-stms e1-vars) ((flatten #t) e1)]
                                             [(new-e2 e2-stms e2-vars) ((flatten #t) e2)])
                                  (let ([if-temp (gensym 'if.)])
                                    (values if-temp
                                            `(,@e1-stms
                                              ,@e2-stms
                                              (if (,cmp ,new-e1 ,new-e2)
                                                  (,@thn-stms (assign ,if-temp ,thn-temp))
                                                  (,@els-stms (assign ,if-temp ,els-temp))))
                                            `((,if-temp . ,if-type) ,@(append e1-vars e2-vars vars)))))]
               [`(let ([,x (has-type ,e ,e-type)]) ,body) 
                (let*-values ([(new-e e-stms e-vars) ((flatten #f) e)]
                              [(new-body body-stms body-vars)
                               ((flatten-if if-type thn-temp thn-stms els-temp els-stms vars) body)])
                  (values new-body
                          `(,@e-stms 
                              (assign ,x ,new-e) 
                              ,@body-stms)
                          (cons `(,x . ,e-type) (append e-vars body-vars vars))))]
               [else
                (let-values ([(new-cnd cnd-stms cnd-vars) ((flatten #t) `(has-type ,cnd ,t))])
                  (let ([if-temp (gensym 'if.)])
                      (values if-temp
                              `(,@cnd-stms
                                (if (eq? #t ,new-cnd)
                                    (,@thn-stms (assign ,if-temp ,thn-temp))
                                    (,@els-stms (assign ,if-temp ,els-temp))))
                              (cons `(,if-temp . ,if-type) (append cnd-vars vars)))))])]
            [else (error "flatten if could not match " cnd)])))
      
      (match e
        [(? symbol?) (values e '() '())]
        [(? integer?) (values e '() '())]
        [(? boolean?) (values e '() '())]
        [`(void) (values '(void) '() '())]
        [`(collect ,size) (values '(void) `((collect ,size)) '())]
        [`(global-value ,name)
         (define temp (gensym 'global.))
         (values temp
                 `((assign ,temp (global-value ,name)))
                 `((,temp . Integer)))]
        [`(allocate ,len ,type)
         (cond [need-temp (define temp (gensym 'vector.))
                          (values temp
                                  `((assign ,temp (allocate ,len ,type)))
                                  `((,temp . ,type)))]
               [else
                (values e '() '())])]
        [`(has-type (function-ref ,fname) ,ty)
         (define ret (gensym 'temp.))
         (values ret
                 `((assign ,ret (function-ref ,fname)))
                 `((,ret . ,ty)))]
        [`(has-type (app ,fname ,args ...) ,ty)
         (define-values (fname-e fname-stms fname-vars) ((flatten #t) fname))
         (define-values (args-es args-stms args-vars) (map3 (flatten #t) args))
         (define function-apply `(app ,fname-e ,@args-es))
         (match need-temp
           [#f (values function-apply (append* fname-stms args-stms) (append* fname-vars args-vars))]
           [#t
            (define ret (gensym 'temp.))
            (values ret
                    `(,@(append* fname-stms args-stms) (assign ,ret (app ,fname-e ,@args-es)))
                    (cons `(,ret . ,ty) (append* fname-vars args-vars)))])]
        [`(define (,fun-name ,args ...) : ,ret ,fbody)
         (define-values (fbody-e fbody-stms fbody-vars) ((flatten #t) fbody))
         `(define (,fun-name ,@args) : ,ret ,fbody-vars ,@fbody-stms (return ,fbody-e))]
        [`(program (type ,t) ,fun-defs ... ,e)
         (let-values ([(e-exp e-stms e-vars) ((flatten #t) e)])
           (define fun-def-flat (map (lambda (def) ((flatten #t) def)) fun-defs))
           `(program ,e-vars (type ,t) (defines ,@fun-def-flat) ,@(append e-stms `((return ,e-exp)))))]        
        [`(has-type (if ,cnd ,thn ,els) ,if-type)
         (let-values ([(new-thn thn-stms thn-vars) ((flatten #t) thn)]
                      [(new-els els-stms els-vars) ((flatten #t) els)])
           ((flatten-if if-type new-thn thn-stms new-els els-stms (append thn-vars els-vars)) cnd))]
        
        [`(let ([,x (has-type ,e ,e-type)]) ,body)
         (let-values ([(new-e e-stms e-vars) ((flatten #f) `(has-type ,e ,e-type))]
                      [(new-body body-stms body-vars) ((flatten need-temp) body)])
           (values new-body
                   (append e-stms `((assign ,x ,new-e)) body-stms)
                   (cons `(,x . ,e-type) (append e-vars body-vars))))]
        [`(has-type (,op ,es ...) ,t)
         #:when (or (set-member? primitive-set op) (set-member? vec-primitive-set op))
         (define-values (new-es* es-stms* es-vars*) (map3 (flatten #t) es))
         (let ([prim-exp `(,op ,@new-es*)]
               [es-stms (append* es-stms*)]
               [es-vars (append* es-vars*)])               
           (case need-temp
             [(#f) (values prim-exp es-stms es-vars)]
             [(#t) (let ([temp (gensym 'temp.)])
                     (values temp 
                             (append es-stms `((assign ,temp ,prim-exp)))
                             (cons `(,temp . ,t) es-vars)))]))]
        [`(has-type ,e ,t)
         ((flatten need-temp) e)]
        [else (error "flatten could not match " e)]))))

(define bin-op->instr
  (lambda (op)
    (match op
      ['+ 'addq]
      ['and 'andq]
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

(define cmp-op->instr
  (lambda (op)
    (match op
      ['eq? 'e]
      ['< 'l]
      ['<= 'le]
      ['> 'g]
      ['>= 'ge]
      [else
       (error "cmp-op->instr could not match " op)])))


(define select-instructions
  (lambda (e)
    (match e
      [(? symbol?) `(var ,e)]
      [(? integer?) `(int ,e)]
      [#t `(int 1)]
      [#f `(int 0)]
      [`(not ,e) `(xorq ,e 1)]
      [`(reg ,r) e]
      [`(return ,e) (select-instructions `(assign (reg rax) ,e))]

      ; -------------------------------------- function --------------------------------------
      [`(assign ,lhs (function-ref ,fun-name))
       `((leaq (function-ref ,fun-name) ,(select-instructions lhs)))]

      ; function definition
      [`(define (,fun-name (,args : ,types) ...) : ,ret (,vars ...) ,stms ...) 
       (define move-from-reg-func (lambda (reg arg) `(movq (reg ,reg) (var ,arg))))
       (define move-from-regs-instrs
         (if (<= (length args) 6)
             (map move-from-reg-func
                  (take para-registers (length args))
                  args)
             (map move-from-reg-func para-registers (take args 6))))

       (define move-from-stk-func (lambda (offset arg) `(movq (deref rbp ,offset) (var ,arg))))
       (define move-from-stks-instrs
         (if (<= (length args) 6)
             '()
             (map move-from-stk-func
                  (range 16 (* 8 (+ 2 (- (length args) 6))) 8)
                  (take-right args (- (length args) 6)))))
       
       (define all-func-calls 
         (filter (lambda (stm)
                   (match stm
                     [`(assign ,lhs (app ,f ,es ...)) #t]
                     [else #f]))
                 stms))

       (define max-num-stk-args
         (let ([lst
                (map (lambda (stm)
                       (match stm
                         [`(assign ,lhs (app ,f ,es ...))
                          (if (<= (length es) 6)
                              0
                              (- (length es) 6))]
                         [else
                          (error "max-stk-args could not match " stm)]))
                     all-func-calls)])
         (if (null? lst) 0 (apply max lst))))

       (define instrs
         `(,@move-from-regs-instrs
           ,@move-from-stks-instrs
           ,@(append* (map select-instructions stms))
           )
         )

       (define args-locals
         (map (lambda (arg type) (cons arg type)) args types))
       
       `(define (,fun-name) ,(length args)
          (,(append vars args-locals) ,max-num-stk-args)
          ,@instrs)]
                                 
      ; function application
      [`(assign ,lhs (app ,fun-name ,es ...))

       (define move-to-reg-func (lambda (arg reg) `(movq ,arg (reg ,reg))))
       (define move-to-regs
         (if (<= (length es) 6)
             (map move-to-reg-func
                  (map select-instructions es)
                  (take para-registers (length es)))
             (map move-to-reg-func
                  (map select-instructions (take es 6))
                  para-registers)))

       (define move-to-stk-func (lambda (arg offset) `(movq ,arg (stack-arg ,offset))))
       (define move-to-stks
         (if (<= (length es) 6)
             '()
             (let ([num-of-stk-needed (- (length es) 6)])
               (map move-to-stk-func
                    (map select-instructions (take-right es num-of-stk-needed))
                    (range 0 (* 8 num-of-stk-needed) 8)))))

       `(,@move-to-regs
         ,@move-to-stks
         (indirect-callq (var ,fun-name))
         (movq (reg rax) ,(select-instructions lhs)))]
                   
      ; -------------------------------------- vector -------------------------------------- 
      [`(void) `(int 0)]
      [`(collect ,size)
       `((movq (reg ,rootstack-reg) (reg rdi))
         (movq ,(select-instructions size) (reg rsi))
         (callq collect))]
      [`(assign ,lhs (allocate ,len (Vector ,ts ...)))
       (define new-lhs (select-instructions lhs))
       (define not-forwading-bit 0)
       (define length-bits (arithmetic-shift len 1))
       (define ptr-mask-bits
         (arithmetic-shift 
           (let loop ([lst (reverse ts)]
                      [bits 0])
             (cond [(null? lst) bits]
                   [else
                    (let ([is-vector-bit (match (car lst)
                                          [`(Vector ,tys ...) 1]
                                          [else 0])])
                      (loop (cdr lst)
                            (bitwise-ior (arithmetic-shift bits 1) is-vector-bit)))]))
           7))
       (define tag (bitwise-ior ptr-mask-bits length-bits not-forwading-bit))
       `((movq (global-value free_ptr) ,new-lhs)
         (addq (int ,(* 8 (+ len 1))) (global-value free_ptr))
         (movq ,new-lhs (reg r11))
         (movq (int ,tag) (deref r11 0)))]
      [`(assign ,lhs (vector-ref ,vec ,i))
       `((movq ,(select-instructions vec) (reg r11))
         (movq (deref r11 ,(* 8 (+ 1 i))) ,(select-instructions lhs)))]
      [`(assign ,lhs (vector-set! ,vec ,i ,elt))
       `((movq ,(select-instructions vec) (reg r11))
         (movq ,(select-instructions elt) (deref r11 ,(* 8 (+ 1 i))))
         (movq (int 0) ,(select-instructions lhs)))]
      
      [`(,cmp ,e1 ,e2)
       #:when (set-member? cmp-instructions cmp)
       `(,cmp ,(select-instructions e1) ,(select-instructions e2))]
      [`(assign ,lhs (read))
      `((callq read_int) (movq (reg rax) ,(select-instructions lhs)))]
      [`(assign ,lhs (void))
       `((movq (int 0) ,(select-instructions lhs)))]
      [`(assign ,lhs (global-value ,name))
       `((movq (global-value ,name) ,(select-instructions lhs)))]
      [`(assign ,lhs (,cmp-op ,e1 ,e2))
       #:when (set-member? cmp-instructions cmp-op)
       (let ([new-lhs (select-instructions lhs)]
             [new-e1 (select-instructions e1)]
             [new-e2 (select-instructions e2)]
             [set-op (cmp-op->instr cmp-op)])
         `((cmpq ,new-e1 ,new-e2)
           (set ,set-op (byte-reg al))
           (movzbq (byte-reg al) ,new-lhs)))]
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
      [`(assign ,lhs ,rhs)
       (let ([new-lhs (select-instructions lhs)]
             [new-rhs (select-instructions rhs)])
         `((movq ,new-rhs ,new-lhs)))]
      [`(if ,cnd ,thn ,els)
       `((if ,(select-instructions cnd)
            ,(append* (map select-instructions thn))
            ,(append* (map select-instructions els))))]

      ; ---------------------------- main -------------------------------
      [`(program (,vars ...) (type ,t) (defines ,fun-defs ...) ,stms ...)
       (let ([fun-def-stms (map select-instructions fun-defs)]
             [new-stms (map select-instructions stms)])
         `(program ,vars (type ,t) (defines ,@fun-def-stms) ,@(append* new-stms)))]
      [else (error "R0/instruction selection, unmatch " e)])))


;;;;;;;;;;;;;;;;;;;;;;;;; liveness analysis ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define free-var
  (lambda (e)
    (match e
      [`(var ,x) (set x)]
      [`(reg ,r) (set r)]
      [`(int ,i) (set)]
      [`(stack-arg ,offset) (set)]
      [`(function-ref ,f) (set)]
      [`(global-value ,name) (set)]
      [`(deref ,reg ,offset) (set reg)]
      [`(byte-reg ,r) (set (byte-reg->full-reg r))]
      [else (error "free-var: unhandled case " e)])))

(define write-vars
  (lambda (ast)
    (match ast
      [`(,op ,src ,dst) #:when (set-member? instruction-set op)
                        (free-var dst)]
      [`(negq ,x) (free-var x)]
      [`(callq ,f) caller-save]
      [`(set ,cc ,arg) (free-var arg)]
      [`(cmpq ,e1 ,e2) (set)]
      [`(indirect-callq ,fname) caller-save]
      [`(leaq ,from ,to) (free-var to)]
      [else (error "write-vars could not match " ast)])))

(define read-vars
  (lambda (ast)
    (match ast
      [(or `(movq ,src ,dst) `(movzbq ,src ,dst)) (free-var src)]
      [(or `(addq ,src ,dst) `(subq ,src ,dst) `(imul ,src ,dst) `(xorq ,src ,dst))
       (set-union (free-var src) (free-var dst))]
      [`(cmpq ,e1 ,e2) (set-union (free-var e1) (free-var e2))]
      [`(negq ,x) (free-var x)]
      [`(set ,cc ,arg) (set)]
      [`(callq ,f) caller-save]
      [`(leaq ,from ,to) (free-var from)]
      [`(indirect-callq ,fname) (free-var fname)]
      [`(andq ,e1 ,e2) (set-union (free-var e1) (free-var e2))]
      [else
       (error "read-vars could not match " ast)])))

(define liveness
 (lambda (origin-live-after)
  (lambda (old-instrs is-if?)

   (define loop 
    (lambda (instrs live-after all-lives instrs-col)
      (cond [(null? instrs) (values instrs-col all-lives)]
            [else
              (let-values ([(new-instr new-live-after) ((uncover-live live-after) (car instrs))])
                (loop (cdr instrs) 
                      new-live-after 
                      (cons new-live-after all-lives) 
                      (cons new-instr instrs-col)))])))

    (loop (reverse old-instrs) origin-live-after `(,origin-live-after) '()))))

(define uncover-live
  (lambda (live-after) 
   (lambda (e)
    (match e
     [`(program ,vars (type ,t) (defines ,fun-defs ...) ,instrs ...)
      (let-values ([(new-instrs new-live-after) ((liveness (set)) instrs #f)])
        (define new-fun-defs (map (uncover-live (set)) fun-defs))
        `(program (,vars ,(cdr new-live-after)) (type ,t) (defines ,@new-fun-defs) ,@new-instrs))]
      [`(if (,cmp-op ,e1 ,e2) ,thn ,els)
       (let-values ([(new-thn thns-before) ((liveness live-after) thn #t)]
                    [(new-els elss-before) ((liveness live-after) els #t)]) 
         (values `(if (,cmp-op ,e1 ,e2) ,new-thn ,(cdr thns-before) ,new-els ,(cdr elss-before))
                 (set-union (car thns-before)
                            (car elss-before)
                            (free-var e1)
                            (free-var e2))))]
      [`(define (,fname) ,num-params (,vars-types ,max-stack) ,instrs ...)
       (define-values (new-instrs new-live-after) ((liveness (set)) instrs #f))
       `(define (,fname) ,num-params ((,vars-types ,max-stack) ,(cdr new-live-after)) ,@new-instrs)]                            
      [else
       (values e (set-union (set-subtract live-after
                                          (write-vars e))
                            (read-vars e)))]))))

(define build-interference
  (lambda (live-after graph mgraph)
    
    (define var? (lambda (x) (equal? (car x) 'var)))
    (define get-var (lambda (x) (cadr x)))
    
    (lambda (ast)
      (match ast
        
       [`(program (,vars-types ,lives) (type ,t) (defines ,fun-defs ...) ,instrs ...)       
        (define new-fun-defs (map (build-interference live-after graph mgraph) fun-defs))
        (let* ([vars* (map (lambda (var-type) (car var-type)) vars-types)]
               [graph (make-graph vars*)]
               [mgraph (make-graph vars*)])
          (let ([new-instrs
                 (for/list ([inst instrs] [live-after lives])
                           ((build-interference live-after graph mgraph) inst))])
            `(program (,vars-types ,graph ,mgraph) (type ,t) (defines ,@new-fun-defs) ,@new-instrs)))]
       
       [`(define (,fname) ,num-params ((,vars-types ,max-stack) ,lives) ,instrs ...)
        (let* ([vars* (map (lambda (var-type) (car var-type)) vars-types)]
               [graph (make-graph vars*)]
               [mgraph (make-graph vars*)])          
          (let ([new-instrs
                 (for/list ([inst instrs] [live-after lives])
                           ((build-interference live-after graph mgraph) inst))])
            `(define (,fname) ,num-params (,vars-types ,max-stack ,graph ,mgraph) ,@new-instrs)))]
        
        [(or `(movq ,src ,dst) `(movzbq ,src ,dst))
         (begin
           (for ([v live-after])
                (for ([d (free-var dst)]
                      #:when (not (or (equal? d v) (equal? `(var ,v) src))))
                     (add-edge graph d v)))
           (cond [(and (var? src) (var? dst)) (add-edge mgraph (get-var src)
                                                               (get-var dst))])
           ast)]  
        [`(callq ,label) 
         (begin 
           (for ([v live-after])
                (for ([r caller-save] #:when (not (equal? v r)))
                     (add-edge graph v r)))
           ast)]
        [`(if ,cnd ,thn ,thn-after ,els ,els-after)
          (let ([new-thn (map (lambda (instr live-after) ((build-interference live-after graph mgraph) instr )) thn thn-after)]
                [new-els (map (lambda (instr live-after) ((build-interference live-after graph mgraph) instr )) els els-after)])
           `(if ,cnd ,new-thn ,new-els))]

        [else
         (begin
           (for ([v live-after])
                (for ([d (write-vars ast)] #:when (not (equal? v d)))
                     (add-edge graph v d)))
           ast)]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; register allocation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; add saturation for each node
(define annotate
  (lambda (graph)
    (hash-for-each
     graph              
     (lambda (var adj-nodes)
       (let ([pre-saturated (list->set
                             (map (lambda (reg) (register->color reg))
                                  (filter (lambda (var) (set-member? registers var)) (set->list adj-nodes))))])
       (hash-set! graph var `(,adj-nodes ,pre-saturated)))))
    graph))

(define choose-color
  (lambda (var interfered mgraph col-map satu)
    (let* ([move-related-vars (if (hash-has-key? mgraph var)
                                  (look-up mgraph var)
                                  (set))]
           [non-interfered (set-subtract move-related-vars
                                        interfered)]
           [non-interfered-alloc (filter (lambda (var) (hash-has-key? col-map var))
                                         (set->list non-interfered))])
      (cond [(not (null? non-interfered-alloc)) (look-up col-map (car non-interfered-alloc))]
            [(null? satu) 0]
            [(null? (cdr satu)) (if (= 0 (car satu)) 1 0)]
            [else
             (let ([diff 
                    (set-subtract (apply set (range 0 (+ 2 (apply max satu))))
                                  (list->set satu))])
               (car (sort (set->list diff) <)))]))))
           
          
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
           [adjs (car (look-up hash key))])
      (set-for-each adjs
                    (lambda (var)
                      (let ([adj (car (look-up hash var))]
                            [sat (cadr (look-up hash var))])
                        (hash-set! hash var `(,adj ,(set-add sat new-color))))))
      (hash->heap hash))))
  
(define (color-graph graph mgraph)
  (lambda (vars)

    (define update-adjs-saturations!
      (lambda (node-heap adjs col)
        (set-for-each adjs
                      (lambda (var)
                        (let ([adj (car (look-up graph var))]
                              [sat (cadr (look-up graph var))])
                          (hash-set! graph var `(,adj ,(set-add sat col)))
                          (cond [(heap-contains? node-heap var)
                                 (begin
                                   (heap-remove! node-heap `(,var . (,adj ,sat)))
                                   (heap-add! node-heap `(,var . (,adj ,(set-add sat col)))))]))))))
    (define init-heap-graph
      (hash->heap (make-hash (filter (lambda (node) (not (set-member? registers (car node)))) (hash->list graph)))))
        
    (let loop ([node-heap init-heap-graph]
               [map-col (make-hash)])
      (if (= 0 (heap-count node-heap))
          map-col
          (let* ([node (heap-min node-heap)]
                 [char (node-key node)]
                 [adjs (node-adjs node)]
                 [saturations (node-saturations node)])
            (let ([color (choose-color char adjs mgraph map-col (set->list saturations))])
              (hash-set! map-col char color)
              (heap-remove-min! node-heap)
              (update-adjs-saturations! node-heap adjs color)
              
              (loop node-heap map-col)))))))
                         
(define reg-spill
  (lambda (color-map)
    (let* ([word-size 8]
           [reg-len (vector-length general-registers)]
           [spilled-color (map (lambda (v) (cdr v))
                               (filter (lambda (v) (>= (cdr v) reg-len))
                                       (hash->list color-map)))]
           [number-of-spill (set-count (apply set spilled-color))])
      (values
       (make-hash
        (map
         (lambda (col-map)
           `(,(car col-map)
             ,@(cond [(< (cdr col-map) reg-len)
                      `(reg ,(vector-ref general-registers (cdr col-map)))]
                     [else
                      `(deref rbp ,(- (* word-size
                                         (+ 1 (- (cdr col-map) reg-len)))))])))
         (hash->list color-map)))
       (align (* word-size number-of-spill) 16)))))
        
(define assign-homes
  (lambda (reg-map)
    (lambda (e)
      (define recur (assign-homes reg-map))
      (match e
        [`(global-value ,name) e]
        [`(deref ,reg ,s) e]
        [`(stack-arg ,offset) e]
        [`(var ,x) (look-up reg-map x)]
        [`(int ,i) `(int ,i)]
        [`(reg ,r) `(reg ,r)]
        [`(byte-reg ,r) `(byte-reg ,r)]
        [`(callq ,f) `(callq ,f)]
        [`(set ,cc ,arg) `(set ,cc ,(recur arg))]
        [`(,cmp-op ,e1 ,e2) #:when (set-member? cmp-instructions cmp-op)
         `(,cmp-op ,(recur e1) ,(recur e2))]
        [`(,instr ,src ,dst)
         #:when (set-member? instruction-set instr)
         `(,instr ,(recur src)
                  ,(recur dst))]
        [`(,instr ,dst)
          #:when (set-member? instruction-set instr)
         `(,instr ,(recur dst))]
        [`(eq? ,e1 ,e2) `(eq? ,(recur e1) ,(recur e2))]
        [`(if ,cnd ,thn ,els)
         `(if ,(recur cnd)
              ,(map recur thn)
              ,(map recur els))]
        [`(function-ref ,name) `(function-ref ,name)]
        [`(indirect-callq ,f) `(indirect-callq ,(recur f))]
        [else (error "assign-homes could not match " e)]))))      

(define allocate-registers
  (lambda (ast)  
    (define (helper vars-types graph mgraph)
      (let* ([annot-graph (annotate graph)]
             [color-map ((color-graph annot-graph mgraph)
                         (map (lambda (var-type) (car var-type)) vars-types))])
        (let-values ([(reg-map stk-size) (reg-spill color-map)])
          (define root-size (* 8 
                               (length 
                                 (filter (lambda (var-type)
                                                   (match (cdr var-type)
                                                     [`(Vector ,ts ...) #t]
                                                     [else #f]))
                                         vars-types))))
          (values root-size stk-size reg-map))))     
    (match ast
      [`(program (,vars-types ,graph ,mgraph) (type ,t) (defines ,fun-defs ...) ,instrs ...)
       
       (define-values (root-size stk-size reg-map) (helper vars-types graph mgraph))
       `(program (,stk-size ,root-size) (type ,t) 
                 (defines ,@(map allocate-registers fun-defs))
                 ,@(map (assign-homes reg-map) instrs))]
      
      [`(define (,fname) ,num-params (,vars-types ,max-stack ,graph ,mgraph) ,instrs ...)
       (define-values (root-size stk-size reg-map) (helper vars-types graph mgraph))
       (define stack-size (align (+ (* stk-size 8)
                                    (* max-stack 8))
                                 16))
       `(define (,fname) ,num-params (,root-size ,stack-size) ,@(map (assign-homes reg-map) instrs))]
      [else (error "allocate-registers could not match " ast)])))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lower-conditionals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             
(define lower-conditionals
  (lambda (ast)
    (match ast
      [`(program (,stk-size ,root-size) (type ,t) ,instrs ...)
       `(program (,stk-size ,root-size) (type ,t) ,@(append* (map lower-conditionals instrs)))]
      [`(if (,cmp-op ,e1 ,e2) ,thns ,elss)
       (let ([thenlabel (gensym 'then.)]
             [elselabel (gensym 'else.)]
             [endlabel (gensym 'end.)])
       `((cmpq ,e2 ,e1)
         (jmp-if ,(cmp-op->instr cmp-op) ,thenlabel)
         ,@(append* (map lower-conditionals elss))
         (jmp ,endlabel)
         (label ,thenlabel)
         ,@(append* (map lower-conditionals thns))
         (label ,endlabel)))]
      [else `(,ast)])))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; patch-instructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The purpose of this pass is to make sure that each instruction adheres to
; the restrictions regarding which arguments can be memory references. For
; most instructions, the rule is that at most one argument may be a memory
; reference.

; for example: 
; (movq (deref rbp -8) (deref rbp -16))
;             
;  will become
;
;  (movq (deref rbp -8) (reg rax))
;  (movq (reg rax) (deref rbp -16))

(define patch-instructions
 (lambda (e)

  (define in-memory?
   (lambda (x)
    (match x
      [`(global-value ,name) #t]
      [`(deref rbp ,n) #t]
      [`(stack-arg ,offset) #t]
      [`(function-ref ,name) #t]
     [else #f])))

  (match e
    [`(program (,stk-size ,root-size) (type ,t) ,instr ...)
    `(program (,stk-size ,root-size) (type ,t) ,@(append* (map patch-instructions instr)))]
    [`(cmpq (int ,e1) (int ,e2)) 
     `((movq (int ,e2) (reg rax))
       (cmpq (int ,e1) (reg rax)))]
    [`(cmpq (reg ,r) (int ,i))
     `((cmpq (int ,i) (reg ,r)))]
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
      [`(global-value ,label)
       (format "~a(%rip)" (label-name (symbol->string label)))]
      [`(deref ,reg ,r) (format "~a(%~a)" r reg)]
      [`(int ,n) (format "$~a" n)]
      [(or `(reg ,r) `(byte-reg ,r)) (format "%~a" r)]
      [`(set ,cc ,arg) (format "\tset~a\t~a\n" cc (print-x86 arg))]
      [`(jmp-if ,cc ,label) (format "\tj~a\t~a\n" cc (symbol->string label))]
      [`(jmp ,label) (format "\tjmp\t~a\n" (symbol->string label))]
      [`(label ,label) (format "\n~a:\n" (symbol->string label))]
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
      
      [`(program (,stk-size ,root-size) (type ,t) ,instrs ...)
       
       (define initialize-heaps
         (string-append
           (format "\tmovq $~a, %rdi\n" root-size)
           (format "\tmovq $~a, %rsi\n" (heap-size))
           (format "\tcallq ~a\n" (label-name "initialize"))
           (format "\tmovq ~a, %~a\n"
                   (print-x86 '(global-value rootstack_begin))
                   rootstack-reg)))
       
       (define initialize-roots
         (string-append*
           (for/list ([i (range (/ root-size 8))])
             (string-append
               (format "\tmovq $0, (~a)\n" rootstack-reg)
               (format "\taddq $~a, ~a\n" 8 rootstack-reg)))))

       (string-append
         (format "\t.globl ~a\n" (label-name "main"))
         (format "~a:\n" (label-name "main"))
         (format "\tpushq\t%rbp\n")
         (format "\tmovq\t%rsp, %rbp\n")
         (string-append* (map
                          (lambda (reg)
                            (format "\tpushq\t%~a\n" reg))
                          (set->list callee-save)))
         (format "\tsubq\t$~a, %rsp\n" stk-size)
         initialize-heaps
         initialize-roots
         "\n"
         (string-append* (map print-x86 instrs))
         "\n"
         (format "\tmovq\t%rax, %rdi\n")
         (format "\tcallq\t~a\n" (label-name "print_int"))
         (format "\tmovq\t$0, %rax\n")
         (format "\taddq\t$~a, %rsp\n" stk-size)
         (string-append* (map
                          (lambda (reg)
                            (format "\tpopq\t%~a\n" reg))
                          (reverse (set->list callee-save))))
         (format "\tpopq\t%rbp\n")
         (format "\tretq\n"))]
      
      [else (error "print-x86 unmatched " e)])))

(define run
  (lambda (e)    
    (define log
      (lambda (e)
        (pretty-display e)
        (newline)))    
   (let* (
           [checked ((type-check '() '()) e)]
           [uniq ((uniquify '()) checked)]           
           [revealed ((reveal-functions (void)) uniq)]
           [expo (expose-allocation revealed)]
           [flat ((flatten #t) expo)]
           ;[instrs (select-instructions flat)]
           ;[liveness ((uncover-live (void)) instrs)]
           ;[graph ((build-interference (void) (void) (void)) liveness)]
           ;[allocs (allocate-registers graph)]
           ; [lower-if (lower-conditionals allocs)]
           ; [patched (patch-instructions lower-if)]
           ; [x86 (print-x86 patched)]
           )
     (log checked)
     (log uniq)
     (log revealed)
     (log expo)  
     (log flat)
     ;(log instrs)
     ;(log liveness)
     ;(log graph)
     ;(log allocs)
     ; (log lower-if)
     ; (log patched)
      ; (log x86)
     
      1 
    )))

#;(run 
   '(program
  (define (minus [n : Integer] [m : Integer]) : Integer
  (+ n (- m)))

(define (zero [x : Integer]) : (Vector)
  (if (eq? x 0)
      (vector)
      (zero (+ (vector-ref (vector x) 0) (- 1)))))

(vector-ref (vector (zero 1) (zero 2) 42) 2)
 ))

(define interp (new interp-R3))
(define interp-F (send interp interp-F '()))
        
(define test-passes
    (list
     `("uniquify"                ,(uniquify '())                                   ,interp-scheme)
     `("reveal-functions"        ,(reveal-functions '())                           ,interp-F)
     `("expose allocation"       ,expose-allocation                                ,interp-F)
     `("flatten"                 ,(flatten #f)                                     ,interp-C)
     `("instruction selection"   ,select-instructions                              ,interp-x86)
     `("liveness analysis"       ,(uncover-live (void))                            ,interp-x86)
     `("build interference"    ,(build-interference (void) (void) (void))          ,interp-x86)
     ; `("allocate register"     ,allocate-registers                                 ,interp-x86) 
     ; ; `("lower-conditionals"    ,lower-conditionals                               ,interp-x86)
     ; `("patch-instructions"    ,patch-instructions                                ,interp-x86)
     ; `("x86"                   ,print-x86                                          #f)
     ))

(define suite-list
  `((0 . ,(range 1 28))
    (1 . ,(range 1 37))
    (2 . ,(range 1 21))
    (3 . ,(range 1 20))
    (4 . ,(range 0 8))
    (6 . ,(range 0 10))
    (7 . ,(range 0 9))
    ))

(define compiler-list
  ;; Name           Typechecker               Compiler-Passes      Initial interpreter   Test-name    Valid suites
  `(("conditionals"  ,(type-check (void) (void))    ,test-passes          ,interp-scheme       "s3"         ,(cdr (assq 3 suite-list)))
    
    ))

(begin
  (for ([test compiler-list])
   (apply interp-tests test))
  (pretty-display "all passed"))
