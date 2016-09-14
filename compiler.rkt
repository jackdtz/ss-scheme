#lang racket

(require data/heap)
(require "utilities/utilities.rkt")
(require "utilities/interp.rkt")
(require "utilities/runtime-config.rkt")

(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bebug flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define debug-flag #f)
(define log-file "test.s")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities for the whole compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define replace-dash
  (lambda (f-name)
    (string->symbol (string-replace (symbol->string f-name) "-" "_"))))

(define (write-to-file path ss [title ""])
  (define out (open-output-file path #:exists 'replace))
  (pretty-display title out)
  (pretty-display ss out)
  (pretty-display (newline) out)
  (close-output-port out))

(define pretty-display-color-map
  (lambda (color-map [title ""])
    (define new-lst
      (hash-map
        color-map
        (lambda (var color) `(,(symbol->string var) . ,color))))
    (write-to-file log-file (sort new-lst #:key car string<?) title)))

(define pretty-display-graph
  (lambda (graph [title ""])
    (define new-graph
      (hash-map
        graph
        (lambda (var adj-nodes)
          `(,(symbol->string var) . ,(list (sort (map (lambda (elt) (symbol->string elt)) 
                               (set->list adj-nodes)) 
                          string<?))))))
    (define new-graph* (sort new-graph #:key car string<?))
    (write-to-file log-file new-graph* title)))

(define pretty-display-inter-graph
  (lambda (graph [title ""])
    (define new-graph
      (hash-map
        graph
        (lambda (var sets)
          (define adj-nodes (car sets))
          `(,(symbol->string var) . ,(list (sort (map (lambda (elt) (symbol->string elt)) 
                               (set->list adj-nodes)) 
                          string<?))))))
    (define new-graph* (sort new-graph #:key car string<?))
    (write-to-file log-file new-graph* title)))
    
(define number-callee-saves (length (set->list callee-save)))
    
(define function-first-offset (+ 8 (* 8 number-callee-saves)))

(define para-registers
  (list 'rdi 'rsi 'rdx 'rcx 'r8 'r9))

(define cmp-instructions
  (set 'eq? '< '<= '> '>=))

(define arith-instructions
  (set '+ '-))

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
  (set 'addq 'negq 'movq 'subq 'movzbq 'cmpq 'xorq 'andq))

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



(define is-vector?
  (lambda (x)
    (match x
      [`(Vector ,es ...) #t]
      [else #f])))

(define root-type?
  (lambda (x)
    (or (is-function? x)
        (is-vector? x))))


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
      
  (define update-env-fenv
    (lambda (vars types env fenv)
      (define updated-env-fenv
             (foldl (lambda (var type env-fenv)
                      (define env (car env-fenv))
                      (define fenv (cdr env-fenv))
                      (match type
                        [`(,args ... -> ,ret) (cons env (type-check-add-f-env fenv var type))]
                        [else (cons (type-check-add-env env var type) fenv)]))
                    (cons env fenv) vars types))
      (values (car updated-env-fenv) (cdr updated-env-fenv))))

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
            ; (display (format "vector: ~a" vec-ty))
            (error (format "expected a vector type in vector-set!, not ~a in ~a" vec-ty ast))])]


        ; lambda
        [`(lambda: ([,vars : ,types] ...) : ,ret-type ,body)
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
           (error (format "body type: ~a and return type: ~a mismatch for lambda function ~a\n current env: ~a \n current fenv: ~a"
                          body-type ret-type ast env fenv)))
         (define args (map (lambda (var type) `(,var : ,type)) vars types))
         (define lambda-type `(,@types -> ,ret-type))
         (values `(has-type (lambda: (,@args) : ,ret-type ,body-e) ,lambda-type)
                 lambda-type)]
        
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

      (define get-args*-env*
        (lambda (vars types env)
          (define var-temps (map (lambda (var) (gensym var)) vars))
          (define new-env
           (foldl (lambda (var new-temp env)
                    (cons `(,var . ,new-temp) env))
                    env vars var-temps))
         (define new-arg-types (map (lambda (temp type) `(,temp : ,type)) var-temps types))
         (values new-arg-types new-env)))
      
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
        [`(lambda: ([,vars : ,types] ...) : ,ret-type ,body)
         (define-values (new-arg-types new-env) (get-args*-env* vars types env))
         `(lambda: ,new-arg-types : ,ret-type ,((uniquify new-env) body))] 
        [`(define (,fun-name (,vars : ,types) ...) : ,ret-type ,fbody)
         (define-values (new-arg-types new-env) (get-args*-env* vars types env))
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
        [`(lambda: ,args : ,ret-type ,body)
         `(lambda: ,args : ,ret-type ,(recur body))]
        [else
         (error "in reveal-functions, unmatched" e)]))))

(define convert-to-closure
  (lambda (e)

    (define (collect-vars ast)
      (match ast
        [`(has-type ,e ,t)
         (if (symbol? e) (hash e ast) (collect-vars e))]
        [(or (? integer?) (? boolean?)) (hash)]
        [`(function-ref ,f) (hash)]
        [`(let ([,x ,e]) ,body)
         (hash-union (collect-vars e) (hash-remove (collect-vars body) x))]
        [`(if ,cnd ,thn, els)
         (hash-union (collect-vars cnd) (collect-vars thn) (collect-vars els))]
        [`(lambda: ([,xs : ,Ts] ...) : ,rT ,body)
         (define (rm x h) (hash-remove h x))
         (foldl rm (collect-vars body) xs)]
        [`(app ,es ...)
         (apply hash-union (map collect-vars es))]
        [`(,op ,es ...)
         (apply hash-union (map collect-vars es))]
        [else (error 'free-variables "unmatched ~a" e)]))

    (define (rm-from-hash var hash)
      (hash-remove hash var))

    (match e

      [`(program (type ,t) ,fun-defs ... ,body)
       (define-values (new-fun-defs fun-lambda) (map2 convert-to-closure fun-defs))
       (define-values (new-body lambda-body) (convert-to-closure body))

       `(program (type ,t) ,@new-fun-defs ,@(append* fun-lambda) ,@lambda-body ,new-body)]


      [`(has-type (function-ref ,f) ,t) 
       (values `(has-type (vector (has-type (function-ref ,f) _)) (Vector _)) '())]
      [(or (? boolean?) (? integer?) (? symbol?)) (values e '())]
      [`(read) (values e '())]
      [`(void) (values e '())]
      [`(if ,cnd ,thn ,els)
      (define-values (new-cnd cnd-lambda) (convert-to-closure cnd))
      (define-values (new-thn thn-lambda) (convert-to-closure thn))
      (define-values (new-els els-lambda) (convert-to-closure els))
      (values `(if ,new-cnd ,new-thn ,new-els)
              `(,@cnd-lambda ,@thn-lambda ,@els-lambda))]


      [`(has-type (app (has-type ,f ,f-type) ,es ...) ,t)
      (define-values (new-f f-fun) (convert-to-closure `(has-type ,f ,f-type)))
      (define-values (new-es es-funs) (map2 convert-to-closure es))
      (define temp (gensym 'app.))
      (match new-f
        [`(has-type ,e^ ,t^)
         (values 
           `(has-type
              (let ([,temp ,new-f])
                (has-type 
                  (app 
                    (has-type (vector-ref (has-type ,temp ,t^) (has-type 0 Integer)) _)
                    (has-type ,temp ,t^)
                    ,@new-es)
                  ,t))
                ,t)
           (append* `(,f-fun ,@es-funs)))]
         [else (error "This should not happend " new-f f-fun e)])]

      [`(has-type (lambda: ([,vars : ,types] ...) : ,ret-type ,body) ,t)
       (define all-vars (collect-vars body))
       (define typed-free-vars (map cdr (hash->list (foldl rm-from-hash all-vars vars))))

       (define free-vars (map cadr typed-free-vars))
       (define free-var-types (map caddr typed-free-vars))

       (define-values (new-body lambda-body) (convert-to-closure body))
       (define clos (gensym 'fvs.))

       (define let-bindings
        (foldr (lambda (fv type index init)
                  `(has-type
                      (let ([,fv (has-type (vector-ref (has-type ,clos _) (has-type ,index Integer)) ,type)])
                        ,init)
                      ,ret-type))
              new-body free-vars free-var-types (range 1 (add1 (length typed-free-vars)))))

       (define fname (gensym 'lambda.))
       (define args (map (lambda (var type) `(,var : ,type)) vars types))


       (define top-level-def
         `(define (,fname [,clos : _] ,@args) : ,ret-type
            ,let-bindings))

       (values `(has-type (vector (has-type (function-ref ,fname) _) ,@typed-free-vars) (Vector _ ,@free-var-types)) 
               (cons top-level-def lambda-body))]

      [`(has-type ,e ,t)
       (define-values (new-e lambda-e) (convert-to-closure e))
       (values `(has-type ,new-e ,t) lambda-e)]

      [`(define (,fname ,args ...) : ,ret-type ,body)
       (define clos (gensym 'fvs.))
       (define-values (new-body body-lambda) (convert-to-closure body))
       (values `(define (,fname [,clos : _] ,@args) : ,ret-type ,new-body)
               body-lambda)]

      [`(,op ,e)
       #:when (set-member? primitive-set op)
       (define-values (new-e e-lambda) (convert-to-closure e))
       (values `(,op ,new-e) e-lambda)] 

      [`(,op ,e1 ,e2)
       #:when (set-member? primitive-set op)
       (define-values (new-e1 e1-lambda) (convert-to-closure e1))
       (define-values (new-e2 e2-lambda) (convert-to-closure e2))
       (values `(,op ,new-e1 ,new-e2)
               `(,@e1-lambda ,@e2-lambda))]

      [`(let ([,x ,e]) ,body)
      (define-values (new-body body-lambda) (convert-to-closure body))
      (define-values (new-e e-lambda) (convert-to-closure e))
      (values `(let ([,x ,new-e]) ,new-body)
              `(,@body-lambda ,@e-lambda))]

      [`(vector-ref ,vec ,ind)
      (define-values (new-vec vec-lambda) (convert-to-closure vec))
      (define-values (new-ind ind-lambda) (convert-to-closure ind))
      (values `(vector-ref ,new-vec ,new-ind)
              `(,@vec-lambda ,@ind-lambda))]

      [`(vector ,es ...)
      (define-values (new-es es-lambda) (map2 convert-to-closure es))
      (values `(vector ,@new-es) (append* es-lambda))]

      [`(vector-set! ,vec ,ind ,val)
      (define-values (new-vec vec-lambda) (convert-to-closure vec))
      (define-values (new-ind ind-lambda) (convert-to-closure ind))
      (define-values (new-val val-lambda) (convert-to-closure val))

      (values `(vector-set! ,new-vec ,new-ind ,new-val)
              `(,@vec-lambda ,@ind-lambda ,@val-lambda))]


      [else (error "convert-to-closure could not match " e)]



       )))


  

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
       (define vec (gensym 'alloc-addr.))
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
       (define not-forwading-bit 1)
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
       (define move-stm (if (= i 0) `((movq ,(select-instructions vec) (reg r11))) '()))
       `(,@move-stm
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
      [`(callq ,f) (set)]
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
      [`(define (,fname) ,num-params (,vars-types ,max-stack-args) ,instrs ...)
       (define-values (new-instrs new-live-after) ((liveness (set)) instrs #f))
       `(define (,fname) ,num-params ((,vars-types ,max-stack-args) ,(cdr new-live-after)) ,@new-instrs)]                            
      [else
       (values e (set-union (set-subtract live-after
                                          (write-vars e))
                            (read-vars e)))]))))

(define build-interference
  (lambda (live-after graph mgraph vars-types)
    
    (define var? (lambda (x) (equal? (car x) 'var)))
    (define get-var (lambda (x) (cadr x)))
    (lambda (ast)
      (match ast
        
       [`(program (,vars-types ,lives) (type ,t) (defines ,fun-defs ...) ,instrs ...)  
        (cond [debug-flag (write-to-file log-file ast "liveness:\n")]) 
        (define new-fun-defs (map (build-interference live-after graph mgraph vars-types) fun-defs))
        (let* ([vars* (map (lambda (var-type) (car var-type)) vars-types)]
               [graph (make-graph vars*)]
               [mgraph (make-graph vars*)])
          (let ([new-instrs
                 (for/list ([inst instrs] [live-after lives])
                           ((build-interference live-after graph mgraph vars-types) inst))])
            `(program (,vars-types ,graph ,mgraph) (type ,t) (defines ,@new-fun-defs) ,@new-instrs)))]
       
       [`(define (,fname) ,num-params ((,vars-types ,max-stack-args) ,lives) ,instrs ...)
        (let* ([vars* (map (lambda (var-type) (car var-type)) vars-types)]
               [graph (make-graph vars*)]
               [mgraph (make-graph vars*)])          
          (let ([new-instrs
                 (for/list ([inst instrs] [live-after lives])
                           ((build-interference live-after graph mgraph vars-types) inst))])
            `(define (,fname) ,num-params (,vars-types ,max-stack-args ,graph ,mgraph) ,@new-instrs)))]
        
        [(or `(movq ,src ,dst) `(movzbq ,src ,dst))
         (begin
           (for ([v live-after])
                (for ([d (free-var dst)]
                      #:when (not (or (equal? d v) (equal? `(var ,v) src))))
                     (add-edge graph d v)))
           ; (cond [(and (var? src) (var? dst)) (add-edge mgraph (get-var src)
           ;                                                     (get-var dst))])
           ast)]  
        [`(callq ,label) 
         (begin 
           ; caller-save registers are alived before function call
           ; so all registers that are lived at this point are interfered with caller-save registers
           (for ([v live-after])
                (for ([r caller-save] #:when (not (equal? v r)))
                     (add-edge graph v r)))
           ; TODO: write comment for the following code block
           ; Why does function variable intefer with callee-save regs?
           (for ([v live-after])
             (cond [(and (not (set-member? registers v))
                         (root-type? (cdr (assq v vars-types))))
                    (for ([u callee-save])
                      (add-edge graph u v))]))
           ast)]
        [`(if ,cnd ,thn ,thn-after ,els ,els-after)
          (let ([new-thn (map (lambda (instr live-after) ((build-interference live-after graph mgraph vars-types) instr )) thn thn-after)]
                [new-els (map (lambda (instr live-after) ((build-interference live-after graph mgraph vars-types) instr )) els els-after)])
           `(if ,cnd ,new-thn ,new-els))]

        [else
         (begin
           (for ([v live-after])
                (for ([d (write-vars ast)] #:when (not (equal? v d)))
                     (add-edge graph v d)))
           ast)]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; register allocation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Inside the graph, annotate each node with its pre-saturation set. 
; Saturation is the set of registers that a node(var) has conflict with.
; Since some variable or temp vars are adjacent to register in the graph,
; we need to extract those registers from the adjacent set and that is the pre-saturation set
(define annotate
  (lambda (graph)
    (hash-for-each
     graph              
     (lambda (var adj-nodes)
       (define all-regs (filter (lambda (var) (set-member? registers var)) (set->list adj-nodes)))
       (define regs->colors (map (lambda (reg) (register->color reg)) all-regs))
       (define pre-saturated (list->set regs->colors))
       (hash-set! graph var `(,adj-nodes ,pre-saturated))))
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
               (if (set-empty? diff)
                   0
                   (car (sort (set->list diff) <))))]))))
           
          
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
  (lambda (vars-types color-map)
    (define word-size 8)
    (define reg-len (vector-length general-registers))
    (define num-of-stack-spill 0)
    (define num-of-root-spill 0)
    (define homes 
      (make-hash
        (map
          (lambda (var-type)
            (define var (car var-type))
            (define type (cdr var-type))
            (define var-color-index (look-up color-map var))
            (define home
              (cond [(< var-color-index reg-len) 
                     `(reg ,(vector-ref general-registers var-color-index))]
                    [(root-type? type)
                     (define i num-of-root-spill)
                     (set! num-of-root-spill (add1 i))
                     `(deref ,rootstack-reg
                             ,(- (* (add1 i) word-size)))]
                    [else
                     (define i num-of-stack-spill)
                     (set! num-of-stack-spill (add1 i))
                     `(deref rbp ,(- (+ function-first-offset
                                        (* i word-size))))]))
            
            `(,var . ,home))
        vars-types)))
    (values homes
            (align (* word-size num-of-stack-spill) 16)
            (align (* word-size num-of-root-spill) 16))))
    
(define assign-homes
  (lambda (homes)
    (lambda (e)
      (define recur (assign-homes homes))
      (match e
        [`(global-value ,name) e]
        [`(deref ,reg ,s) e]
        [`(function-ref ,f) e]
        [`(indirect-callq ,f) `(indirect-callq ,(recur f))]
        [`(stack-arg ,offset) e]
        [`(var ,x) (look-up homes x)]
        [`(int ,i) `(int ,i)]
        [`(reg ,r) `(reg ,r)]
        [`(byte-reg ,r) `(byte-reg ,r)]
        [`(callq ,f) `(callq ,f)]
        [`(set ,cc ,arg) `(set ,cc ,(recur arg))]
        [`(leaq ,src ,dst) `(leaq ,(recur src) ,(recur dst))]
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
        [else (error "assign-homes could not match " e)]))))      


(define allocate-registers
  (lambda (ast)
    
    (define (helper graph mgraph vars-types)
      (define annotated-graph (annotate graph))
      (define color-map ((color-graph annotated-graph mgraph)
                         (map (lambda (var) (car var)) vars-types)))
      (define-values (homes stack-size root-size) (reg-spill vars-types color-map))
      (values homes stack-size root-size color-map))
    
    (match ast
      [`(program (,vars-types ,graph ,mgraph) (type ,t) (defines ,fun-defs ...) ,instrs ...)
       (define-values (homes stack-size root-size color-map) (helper graph mgraph vars-types))
       ;; debug
       (cond [debug-flag (pretty-display-color-map color-map "color-map:")
                         (pretty-display-color-map homes "homes:")
                         (pretty-display-inter-graph graph "interference:")])

       `(program (,stack-size ,root-size) (type ,t)
                 (defines ,@(map allocate-registers fun-defs))
                 ,@(map (assign-homes homes) instrs))]
      [`(define (,fname) ,num-params (,vars-types ,max-stack-args ,graph ,mgraph) ,instrs ...)
       (define-values (homes stack-size root-size color-map) (helper graph mgraph vars-types))


       
       (define adjust-stack-size
         (align (+ (* max-stack-args 8) stack-size) 16))
       `(define (,fname) ,num-params (,adjust-stack-size ,root-size) ,@(map (assign-homes homes) instrs))]
       
      [else (error "allocate-registers could not match " ast)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lower-conditionals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             
(define lower-conditionals
  (lambda (ast)
    
    (define append-number
      (lambda (symbol int)
        (string->symbol 
          (string-append (symbol->string symbol)
                         (number->string int)))))
    
    (match ast
      [`(program (,stk-size ,root-size) (type ,t) (defines ,fun-defs ...) ,instrs ...)
       `(program (,stk-size ,root-size) (type ,t) (defines ,@(map lower-conditionals fun-defs)) 
                 ,@(append* (map lower-conditionals instrs)))]
      [`(define (,fname) ,num-params (,stack-size ,root-size) ,instrs ...)
       `(define (,fname) ,num-params (,stack-size ,root-size) ,@(append* (map lower-conditionals instrs)))]
      [`(if (,cmp-op ,e1 ,e2) ,thns ,elss)
       (define random-int (random 100000000))
       (let ([thenlabel (append-number 'then. random-int)]
             [elselabel (append-number 'else. random-int)]
             [endlabel (append-number 'end. random-int)])
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
      [`(deref ,reg ,n) #t]
      [`(stack-arg ,offset) #t]
      [`(function-ref ,name) #t]
     [else #f])))
     
  (match e
    [`(program (,stk-size ,root-size) (type ,t) (defines ,fun-defs ...) ,instr ...)
    `(program (,stk-size ,root-size) (type ,t) (defines ,@(map patch-instructions fun-defs)) 
                                               ,@(append* (map patch-instructions instr)))]
    [`(define (,fname) ,num-params (,stack-size ,root-size) ,instrs ...)
     `(define (,fname) ,num-params (,stack-size ,root-size) ,@(append* (map patch-instructions instrs)))]
    [`(cmpq (int ,e1) (int ,e2)) 
     `((movq (int ,e2) (reg rax))
       (cmpq (int ,e1) (reg rax)))]
    [`(cmpq (reg ,r) (int ,i))
     `((cmpq (int ,i) (reg ,r)))]
    [`(leaq ,src ,dst)
     (cond [(in-memory? dst)
            `((leaq ,src (reg rax))
              (movq (reg rax) ,dst))]
           [else `(,e)])]
    [(or `(addq (int 0) ,dst) `(subq (int 0) ,dst)) '()]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;; print-x86 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define print-x86
  (lambda (e)

    (define (updata-stack op)
      (lambda (stack-size)
        (if (= 0 stack-size)
            ""
            (match op
              ['addq (format "\taddq\t$~a, %rsp\n" stack-size)]
              ['subq (format "\tsubq\t$~a, %rsp\n" stack-size)]
              [else (error "update-stack could not match " op)]))))
    (define subtract-stack (updata-stack 'subq))
    (define add-stack (updata-stack 'addq))
    
    (match e
      [`(global-value ,label)
       (format "~a(%rip)" (label-name (symbol->string (replace-dash label))))]
      [`(deref ,reg ,r) (format "~a(%~a)" r reg)]
      [`(function-ref ,label) (format "~a(%rip)" (replace-dash label))]
      [`(stack-arg ,offset) (format "~a(%rsp)\n" offset)]
      [`(indirect-callq ,f) (format "\tcallq\t*~a\n" (print-x86 f))]
      [`(int ,n) (format "$~a" n)]
      [(or `(reg ,r) `(byte-reg ,r)) (format "%~a" r)]
      [`(set ,cc ,arg) (format "\tset~a\t~a\n" cc (print-x86 arg))]
      [`(jmp-if ,cc ,label) (format "\tj~a\t~a\n" cc (symbol->string label))]
      [`(jmp ,label) (format "\tjmp\t~a\n" (symbol->string label))]
      [`(label ,label) (format "\n~a:\n" (symbol->string label))]
      [`(leaq ,src ,dst) (format "\tleaq\t~a,\t~a\n" (print-x86 src) (print-x86 dst))]
      [`(callq ,f)
       (format "\tcallq\t~a\n" (label-name (symbol->string f)))]
      [`(movq (int 0) ,(and dst `(reg ,r)))
       (format "\txorq\t~a, ~a\n" (print-x86 dst) (print-x86 dst))]
      [`(,instr ,src ,dst)
       #:when (set-member? instruction-set instr)
       (format "\t~a\t~a, ~a\n" instr 
                                (print-x86 src)
                                (print-x86 dst))]
      [`(,instr ,dst)
       #:when (set-member? instruction-set instr)
       (format "\t~a\t~a\n" instr (print-x86 dst))]
      
      [`(define (,fname) ,num-params (,stack-size ,root-size) ,instrs ...)
       (define callee-regs (set->list callee-save))
       (define save-callee-regs
         (string-append*
           (for/list ([reg callee-regs])
             (format "\tpushq\t%~a\n" reg))))
       (define restore-callee-regs
         (string-append*
           (for/list ([reg (reverse callee-regs)])
             (format "\tpopq\t%~a\n" reg))))
       (define callee-space
         (* (length (set->list callee-save)) 8))
       
       (define initialize-roots
         (string-append*
           (for/list ([i (range (/ root-size 8))])
             (string-append
               (format "\tmovq\t$0, (%~a)\n" rootstack-reg)
               (format "\taddq\t$~a, %~a\n" 8 rootstack-reg)))))
       
       (string-append
         (format "\t.globl ~a\n" (replace-dash fname))
         (format "~a:\n" (replace-dash fname))
         (format "\tpushq\t%rbp\n")
         (format "\tmovq\t%rsp, %rbp\n")
         save-callee-regs
         (subtract-stack stack-size)
         initialize-roots
         "\n"
         (string-append* (map print-x86 instrs))
         "\n"
         (add-stack stack-size)
         restore-callee-regs
         (format "\tsubq\t$~a, %~a\n" root-size rootstack-reg)
         (format "\tpopq\t%rbp\n")
         (format "\tretq\n")
         "\n"
         )]
      
      [`(program (,stk-size ,root-size) (type ,t) (defines ,fun-defs ...) ,instrs ...)
       (define all-fun-x86
         (string-append*
           (map print-x86 fun-defs)))
       
       (define initialize-heaps
         (string-append
           (format "\tmovq\t$~a, %rdi\n" (rootstack-size))
           (format "\tmovq\t$~a, %rsi\n" (heap-size))
           (format "\tcallq\t~a\n" (label-name "initialize"))
           (format "\tmovq\t~a, %~a\n"
                   (print-x86 '(global-value rootstack_begin))
                   rootstack-reg)))
       
       (define initialize-roots
         (string-append*
           (for/list ([i (range (/ root-size 8))])
             (string-append
               (format "\tmovq\t$0, (%~a)\n" rootstack-reg)
               (format "\taddq\t$~a, %~a\n" 8 rootstack-reg)))))

       (string-append
         all-fun-x86
         "\n"
         (format "\t.globl ~a\n" (label-name "main"))
         (format "~a:\n" (label-name "main"))
         (format "\tpushq\t%rbp\n")
         (format "\tmovq\t%rsp, %rbp\n")
         (string-append* (map
                          (lambda (reg)
                            (format "\tpushq\t%~a\n" reg))
                          (set->list callee-save)))
         (subtract-stack stk-size)
         initialize-heaps
         initialize-roots
         "\n"
         (string-append* (map print-x86 instrs))
         "\n"
         (print-by-type t)
         (format "\tmovq\t$0, %rax\n")
         (add-stack stk-size)
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
           [closure (convert-to-closure revealed)]
           [expo (expose-allocation closure)]
           [flat ((flatten #t) expo)]
           [instrs (select-instructions flat)]
           [liveness ((uncover-live (void)) instrs)]
           [graph ((build-interference (void) (void) (void) (void)) liveness)]
           [allocs (allocate-registers graph)]
           [lower-if (lower-conditionals allocs)]
           [patched (patch-instructions lower-if)]
           [x86 (print-x86 patched)]
           )
     ; (log checked)
     ; (log uniq)
     ; (log revealed)
     ; (log closure)
     ; (log expo)  
     ; (log flat)
     ; (log instrs)
     ; (log liveness)
     ; (log graph)
     ; (log allocs)
     ; (log lower-if)
     ; (log patched)
      ; (log x86)
      ; 1

     (write-to-file "test.s" x86)
    )))

(run 
 '(program

(define (f [x : Integer]) : (Integer -> Integer)
   (let ([y 4])
      (lambda: ([z : Integer]) : Integer
   (+ x (+ y z)))))

(let ([g (f 5)])
  (let ([h (f 3)])
    (+ (g 11) (h 15))))

))



(define interp (new interp-R4))
(define interp-F (send interp interp-F '()))

(define test-passes
    (list
     `("uniquify"                ,(uniquify '())                                   ,interp-scheme)
     `("reveal-functions"        ,(reveal-functions '())                           ,interp-F)
     `("convert-to-closures"     ,convert-to-closure                               ,interp-F)
     `("expose allocation"       ,expose-allocation                                ,interp-F)
     `("flatten"                 ,(flatten #f)                                     ,interp-C)
     `("instruction selection"   ,select-instructions                              ,interp-x86)
     `("liveness analysis"       ,(uncover-live (void))                            ,interp-x86)
     `("build interference"      ,(build-interference (void) (void) (void) (void)) ,interp-x86)
     `("allocate register"       ,allocate-registers                               ,interp-x86) 
     `("lower-conditionals"      ,lower-conditionals                               ,interp-x86)
     `("patch-instructions"      ,patch-instructions                               ,interp-x86)
     `("x86"                     ,print-x86                                        #f)
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
  ;; Name           Typechecker                     Compiler-Passes      Initial interpreter   Test-name    Valid suites
  `(
    ; ("conditionals"  ,(type-check (void) (void))    ,test-passes          ,interp-scheme       "s0"         ,(cdr (assq 0 suite-list)))
    ; ("conditionals"  ,(type-check (void) (void))    ,test-passes          ,interp-scheme       "s1"         ,(cdr (assq 1 suite-list)))
    ; ("conditionals"  ,(type-check (void) (void))    ,test-passes          ,interp-scheme       "s2"         ,(cdr (assq 2 suite-list)))
    ; ("conditionals"  ,(type-check (void) (void))    ,test-passes          ,interp-scheme       "s3"         ,(cdr (assq 3 suite-list)))
    ("conditionals"  ,(type-check (void) (void))    ,test-passes          ,interp-scheme       "s4"         ,(cdr (assq 4 suite-list)))
    
    ))

; (begin
;    (for ([test compiler-list])
;     (apply interp-tests test))
;    (pretty-display "all passed"))





  