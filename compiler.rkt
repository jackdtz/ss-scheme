#lang racket

(require data/heap)
(require "utilities/utilities.rkt")
(require "utilities/interp.rkt")
(provide (all-defined-out))

(define cmp-instructions
  (set 'eq? '< '<= '> '>=))

(define logic-instructions
  (set 'and 'or 'not))

(define primitive-set
  (set-union
   (set '+ '- 'read       
        'and 'or 'not)
   cmp-instructions))

(define instruction-set
  (set 'addq 'negq 'movq 'subq 'movzbq 'cmpq 'xorq))


(define look-up
  (lambda (env key)
    (if (hash-has-key? env key)
        (hash-ref env key)
        (error "no value found for key" env key))))


(define add-env
  (lambda (env key val)
    (if (hash-has-key? env key)
        (hash-set env key (cons val (hash-ref env key)))
        (hash-set env key (list val)))))

(define type-integer? (lambda (x) (equal? x 'Integer)))
(define type-boolean? (lambda (x) (equal? x 'Boolean)))
(define same-type? (lambda (x y) (equal? x y)))


(define type-check
  (lambda (env)
    (lambda (ast)
      (define recur (type-check env))
      (match ast
        [`(program ,body) 
         (let ([body-type ((type-check (hash)) body)])
           `(program (type ,body-type) ,body))]
        ['(read) 'Integer]
        [(? fixnum?) 'Integer]
        [(? boolean?) 'Boolean]
        [(? symbol?) (car (look-up env ast))]
        
        ; Vector
        [`(void) (values `(has-type (void) Void) 'Void)]

        [`(Vector ,(app (recur type-check) e* t*) ...)
         (let ([t `(Vector ,@t*)])
           (values `(has-type (vector ,@e*) ,t) t))]  
        ; [`(vector-ref ,(app recur v-type) ,ind)
        ;  (match v-type
        ;    [`(Vector ,ts ...)
        ;     (unless (and (exact-nonnegative-integer? ind)
        ;                  (< ind (length ts)))
        ;       (error 'type-check "invalid index ~a[~a]" ast ind))
        ;     (let ([ret-elt (list-ref )]))])]
        
        [`(eq? ,e1 ,e2)
         (let ([e1-type (recur e1)]
               [e2-type (recur e2)])
           (if (equal? e1-type e2-type)
               'Boolean
               (error "type-check: eq? expect two operands have the same type" ast e1 e2)))]
        [`(,cmp-op ,e1 ,e2)
         #:when (set-member? cmp-instructions cmp-op)
         (let ([e1-type (recur e1)]
               [e2-type (recur e2)])
           (cond [(and (type-integer? e1-type) (type-integer? e2-type)) 'Boolean]
                 [else 
                  (error
                   (format "type-check: ~a expects two operands: ~a(type:~a) and ~a(type:~a) to have the same type Integer (ast: ~a"
                           cmp-op e1 e1-type e2 e2-type ast))]))]            
        [`(,logic-op ,e1 ,e2)
         #:when (set-member? logic-instructions logic-op)
         (let ([e1-type (recur e1)]
               [e2-type (recur e2)])
           (cond [(and (type-boolean? e1-type) (type-boolean? e2-type)) 'Boolean]
                 [else 
                  (error
                   (format "type-check: ~a expects two operands: ~a(type:~a) and ~a(type:~a) to have the same type 'Boolean (ast: ~a"
                           logic-op e1 e1-type e2 e2-type ast))]))]
        [`(let ([,var ,(app (type-check env) var-type)]) ,body)
         (let ([new-env (add-env env var var-type)])
           ((type-check new-env) body))]
        [`(not ,(app recur type))
         (match type
           ['Boolean 'Boolean]
           [else (error "type-check: 'not expects a Boolean" ast type)])]
        [`(- ,(app recur type))
         (match type
           ['Integer 'Integer]
           [else (error "type-check: '- expects an Integer" ast type)])]
        [`(+ ,e1 ,e2)
         (let ([type1 (recur e1)]
               [type2 (recur e2)])
           (cond [(and (type-integer? type1) (type-integer? type2)) 'Integer]
                 [(type-integer? type1) 
                  (error "type-check: '+ expects e2 to be an Integer" ast e2)]
                 [(type-integer? type2) 
                  (error "type-check: '+ expects e1 to be an Integer" ast e1)]
                 [else 
                  (error "type-check: '+ expects e1, e2 to be Integer" ast e1 e2)]))]
        [`(if ,cmp ,t ,f) 
         (let ([type-cmp (recur cmp)]
               [type-t (recur t)]
               [type-f (recur f)])
           (cond [(and (type-boolean? type-cmp) (same-type? type-t type-f)) type-t]
                 [(not (same-type? type-t type-f)) 
                  (error (format "type-check: (thn:~a) and (els:~a) from ~a have different type ~a and ~a" 
                                  t f ast type-t type-f))]
                 [(not (type-boolean? type-cmp)) 
                  (error (format "type-check: condition expression ~a from ~a expects a boolean type, but get type ~a" 
                                  cmp ast type-cmp))]))]))))


(define uniquify
  (lambda (env)
    (lambda (e)
      (define recur (uniquify env))
      (match e
        [(? symbol?) (cdr (assq e env))]
        [(? integer?) e]
        [(? boolean?) e]
        [`(if ,cnd ,thn ,els)
         `(if ,(recur cnd)
              ,(recur thn)
              ,(recur els))]
        [`(let ([,x ,(app recur new-e)]) ,body)
         (let ([new-x (gensym x)])
           `(let ([,new-x ,new-e])
              ,((uniquify (cons `(,x . ,new-x) env)) body)))]
        [`(vector ,exp ...) `(vector ,@(map recur exp))]
        [`(vector-ref ,exp ,int) `(vector-ref ,(recur exp) ,int)]
        [`(vector-set! ,e1 ,int ,e2) `(vector-set! ,(recur e1) ,int ,(recur e2))]
        ; [`(program (type ,t) ,e) `(program (type ,t) ,(recur e))]
        [`(program ,e) `(program ,(recur e))]
        [`(,op ,es ...) #:when (set-member? primitive-set op)
                        `(,op ,@(map (lambda (e) (recur e)) es))]
        [else (error "Uniquify could not match " e)]))))

(define flatten
  (lambda (need-temp)
    (lambda (e)
      
      (define (flatten-if thn-temp thn-stms els-temp els-stms vars)
        (lambda (cnd)
          (match cnd
            [#t (values thn-temp thn-stms vars)]
            [#f (values els-temp els-stms vars)] 
            [`(not ,e) ((flatten-if els-temp els-stms thn-temp thn-stms vars) e)]
            [`(and ,e1 ,e2)
             ((flatten-if thn-temp thn-stms els-temp els-stms vars)
              `(if (not ,e1) #f ,e2))]
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
                         `(,if-temp ,@(append e1-vars e2-vars vars)))))]
            [`(let ([,x ,e]) ,body) 
             (let*-values ([(new-e e-stms e-vars) ((flatten #f) e)]
                           [(new-body body-stms body-vars)
                            ((flatten-if thn-temp thn-stms els-temp els-stms vars) body)])
                  (values new-body
                          `(,@e-stms 
                            (assign ,x ,new-e) 
                            ,@body-stms)
                          (cons x (append e-vars body-vars vars))))]
            [else
             (let-values ([(new-cnd cnd-stms cnd-vars) ((flatten #t) cnd)])
               (let ([if-temp (gensym 'if.)])
                 (values if-temp
                         `(,@cnd-stms
                           (if (eq? #t ,new-cnd)
                               (,@thn-stms (assign ,if-temp ,thn-temp))
                               (,@els-stms (assign ,if-temp ,els-temp))))
                         (cons if-temp (append cnd-vars vars)))))])))
      (match e
        [(? symbol?) (values e '() '())]
        [(? integer?) (values e '() '())]
        [(? boolean?) (values e '() '())]
        [`(vector ,(app (flatten #t) es* es-stms* es-vars*) ...)
         (let ([rtn `(vector ,@es*)]
               [stms (append* es-stms*)]
               [vars (append* es-vars*)]
               [rtn-tmp (gensym 'temp.)])
           (values rtn-tmp
                   (append stms `((assign ,rtn-tmp ,rtn)))
                   (cons rtn-tmp vars)))]
        [`(vector-ref ,vec ,int)
         (let-values ([(new-e vec-stms vec-vars) ((flatten #t) vec)])
           (let ([new-temp (gensym 'temp.)])
             (values new-temp
                     (append vec-stms `((assign ,new-temp (vector-ref ,new-e ,int))))
                     (cons new-temp vec-vars))))]

        [`(program ,e) 
         (let-values ([(e-exp e-stms e-vars) ((flatten #t) e)])
           `(program ,e-vars ,@(append e-stms `((return ,e-exp)))))]
        [`(if ,cnd ,thn ,els)
         (let-values ([(new-thn thn-stms thn-vars) ((flatten #t) thn)]
                      [(new-els els-stms els-vars) ((flatten #t) els)])
           ((flatten-if new-thn thn-stms new-els els-stms (append thn-vars els-vars)) cnd))]                 
        [`(let ([,x ,e]) ,body)
         (let-values ([(new-e e-stms e-vars) ((flatten #f) e)]
                      [(new-body body-stms body-vars) ((flatten need-temp) body)])
           (values new-body
                   (append e-stms `((assign ,x ,new-e)) body-stms)
                   (cons x (append e-vars body-vars))))]
        [`(,op ,(app (flatten #t) new-es* es-stms* es-vars*) ...)
         #:when (set-member? primitive-set op)         
         (let ([prim-exp `(,op ,@new-es*)]
               [es-stms (append* es-stms*)]
               [es-vars (append* es-vars*)])               
           (case need-temp
             [(#f) (values prim-exp es-stms es-vars)]
             [(#t) (let ([temp (gensym 'temp.)])
                     (values temp 
                             (append es-stms `((assign ,temp ,prim-exp)))
                             (cons temp es-vars)))]))]
        
        
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
      [`(,cmp ,e1 ,e2)
       #:when (set-member? cmp-instructions cmp)
       `(,cmp ,(select-instructions e1) ,(select-instructions e2))]
      [`(assign ,lhs (read))
      `((callq read_int) (movq (reg rax) ,(select-instructions lhs)))]
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
       
      [`(program ,vars (type ,t) ,stms ...)
       (let ([new-stms (map (lambda (s) (select-instructions s)) stms)])
         `(program ,vars (type ,t) ,@(append* new-stms)))]
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
      [`(byte-reg ,r) (set (byte-reg->full-reg r))]
      [else (error "unhandled case " e)])))

(define write-vars
  (lambda (ast)
    (match ast
      [`(,op ,src ,dst) #:when (set-member? instruction-set op)
                        (free-var dst)]
      [`(negq ,x) (free-var x)]
      [`(callq ,f) caller-save]
      [`(set ,cc ,arg) (free-var arg)]
      [`(cmpq ,e1 ,e2) (set)]
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

    (if is-if?
        (loop (reverse old-instrs) origin-live-after '() '())
        (loop (reverse old-instrs) origin-live-after `(,origin-live-after) '())))))

(define uncover-live
  (lambda (live-after) 
   (lambda (e)
    (match e
     [`(program ,vars (type ,t) ,instrs ...)
      (let-values ([(new-instrs new-live-after) ((liveness (set)) instrs #f)])
        `(program (,vars ,(cdr new-live-after)) (type ,t) ,@new-instrs))]
      [`(if (,cmp-op ,e1 ,e2) ,thn ,els)
       (let-values ([(new-thn thns-before) ((liveness live-after) thn #t)]
                    [(new-els elss-before) ((liveness live-after) els #t)])

         (values `(if (,cmp-op ,e1 ,e2) ,new-thn ,thns-before ,new-els ,elss-before)
                 (set-union (apply set-union thns-before)
                            (apply set-union elss-before)
                            (free-var e1)
                            (free-var e2))))]                            
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
        
       [`(program (,vars ,lives) (type ,t) ,instrs ...)
        (let ([graph (make-graph vars)]
              [mgraph (make-graph vars)])
          (let ([new-instrs
                 (for/list ([inst instrs] [live-after lives])
                           ((build-interference live-after graph mgraph) inst))])
            `(program (,vars ,graph ,mgraph) (type ,t) ,@new-instrs)))]
        
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
        [else (error "assign-homes could not match " e)]))))      


(define allocate-registers
  (lambda (ast)
    (match ast
      [`(program (,vars ,graph ,mgraph) (type ,t) ,instrs ...)
       (let* ([annot-graph (annotate graph)]
              [color-map ((color-graph annot-graph mgraph) vars)])
        (let-values ([(reg-map stk-size) (reg-spill color-map)])
          `(program ,stk-size (type ,t) ,@(map (assign-homes reg-map) instrs))))]
      [else (error "allocate-registers could not match " ast)])))
                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define lower-conditionals
  (lambda (ast)
    (match ast
      [`(program ,stk-size (type ,t) ,instrs ...)
       `(program ,stk-size (type ,t) ,@(append* (map lower-conditionals instrs)))]
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

        
(define patch-instructions
 (lambda (e)

  (define in-memory?
   (lambda (x)
    (match x
     [`(deref rbp ,n) #t]
     [else #f])))

  (match e
    [`(program ,frame-size (type ,t) ,instr ...)
    `(program ,frame-size (type ,t) ,@(append* (map patch-instructions instr)))]
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
      
      [`(program ,stack-space (type ,t) ,instrs ...)
       (string-append
         (format "\t.globl ~a\n" (label-name "main"))
         (format "~a:\n" (label-name "main"))
         (format "\tpushq\t%rbp\n")
         (format "\tmovq\t%rsp, %rbp\n")
         (string-append* (map
                          (lambda (reg)
                            (format "\tpushq\t%~a\n" reg))
                          (set->list callee-save)))
         (format "\tsubq\t$~a, %rsp\n" stack-space)
         "\n"
         (string-append* (map print-x86 instrs))
         "\n"
         (format "\tmovq\t%rax, %rdi\n")
         (format "\tcallq\t~a\n" (label-name "print_int"))
         (format "\tmovq\t$0, %rax\n")
         (format "\taddq\t$~a, %rsp\n" stack-space)
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
           [checked ((type-check (void) 0) e)]
           [uniq ((uniquify '()) checked)]
           [flat ((flatten #t) uniq)]
           [instrs (select-instructions flat)]
           [liveness ((uncover-live (void)) instrs)]
           [graph ((build-interference (void) (void) (void)) liveness)]
           [allocs (allocate-registers graph)]
           [lower-if (lower-conditionals allocs)]
           [patched (patch-instructions lower-if)]
           [x86 (print-x86 patched)]
           )
    (log checked)
    (log uniq)
    (log flat)
    (log instrs)
    (log liveness)
    (log graph)
    (log allocs)
    (log lower-if)
    (log patched)
    (log x86)
    )))


(define t
  (lambda (r)
    ; (define tc (type-check '()))
    ((flatten #t) ((uniquify '()) r))))

(t '(program
     (vector-ref (vector-ref (vector (vector 42)) 0) 0)))


