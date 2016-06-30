#lang racket

(require data/heap)
(require "utilities/utilities.rkt")
(require "utilities/interp.rkt")
(provide (all-defined-out))

(define cmp-instructions
  (set 'eq? '< '<= '> '>=))

(define primitive-set
  (set-union
   (set '+ '- 'read       
        'and 'or 'not)
   cmp-instructions))

(define instruction-set
  (set 'addq 'negq 'movq 'subq))



; (define caller-save (set 'rdx 'rcx 'rsi 'rdi 'r8 'r9 'r10))

; (define general-registers (vector 'rbx 'rcx 'rdx 'rsi 'rdi
;                                   'r8 'r9 'r10 'r12 
;                                   'r13 'r14))

; (define (align n alignment)
;   (cond [(eq? 0 (modulo n alignment)) n]
;         [else
;          (+ n (- alignment (modulo n alignment)))]))

(define look-up
  (lambda (env key)
    (if (hash-has-key? env key)
        (car (hash-ref env key))
        (error "no value found for key" env key))))

(define add1 (lambda (x) (+ 1 x)))

(define add-env
  (lambda (env key val)
    (if (hash-has-key? env key)
        (hash-set env key (cons val (hash-ref env key)))
        (hash-set env key (list val)))))

(define type-integer? (lambda (x) (equal? x 'Integer)))
(define type-boolean? (lambda (x) (equal? x 'Boolean)))
(define same-type? (lambda (x y) (equal? x y)))


; (define type-check
;   (lambda (env level)
;     (lambda (ast)
;       ; (define recur (type-check env level))
;       (match ast
;         [`(program ,body) 
;          (let ([body-type ((type-check (hash) level) body)])
;            `(program ,body))]
;         ['(read) 'Integer]
;         [(? fixnum?) 'Integer]
;         [(? boolean?) 'Boolean]
;         [(? symbol?) (look-up env ast )]
;         [`(eq? ,e1 ,e2)
;          (let ([e1-type ((type-check env level) e1)]
;                [e2-type ((type-check env level) e2)])
;            (if (equal? e1-type e2-type)
;                'Boolean
;                (error "type-check: eq? expect two operands have the same type" ast e1 e2)))]            
;         [`(,cmp-op ,e1 ,e2)
;          #:when (set-member? cmp-instructions cmp-op)
;         (cond [(and (fixnum? e1) (fixnum? e2)) 'Boolean]
;               [(and (boolean? e1) (boolean? e2)) 'Boolean]
;               [else 
;                (error "type-check: expects two operands have the same type" ast cmp-op e1 e2)])]
;         [`(let ([,var ,(app (type-check env (add1 level)) var-type)]) ,body)
;          (let* ([new-level (add1 level)]
;                 [new-env (add-env env var var-type)])
;            ((type-check new-env new-level) body))]
;         [`(not ,(app (type-check env level) type))
;          (match type
;            ['Boolean 'Boolean]
;            [else (error "type-check: 'not expects a Boolean" ast type)])]
;         [`(- ,(app (type-check env level) type))
;          (match type
;            ['Integer 'Integer]
;            [else (error "type-check: '- expects an Integer" ast type)])]
;         [`(+ ,e1 ,e2)
;          (let ([type1 ((type-check env level) e1)]
;                [type2 ((type-check env level) e2)])
;            (cond [(and (type-integer? type1) (type-integer? type2)) 'Integer]
;                  [(type-integer? type1) 
;                   (error "type-check: '+ expects e2 to be an Integer" ast e2)]
;                  [(type-integer? type2) 
;                   (error "type-check: '+ expects e1 to be an Integer" ast e1)]
;                  [else 
;                   (error "type-check: '+ expects e1, e2 to be Integer" ast e1 e2)]))]
;         [`(if ,cmp ,t ,f) 
;          (let ([type-cmp ((type-check env level) cmp)]
;                [type-t ((type-check env level) t)]
;                [type-f ((type-check env level) f)])
;            (cond [(and (type-boolean? cmp) (same-type? type-t type-f)) type-t]
;                  [(not (same-type? type-t type-f)) 
;                   (error (format "type-check: (thn:~a) and (els:~a) from ~a have different type ~a and ~a" 
;                                   t f ast type-t type-f))]
;                  [(not (type-boolean? type-cmp)) 
;                   (error (format "type-check: condition expression ~a from ~a expects a boolean type, but get type ~a" 
;                                   cmp ast type-cmp))]))]))))


         
(define uniquify
  (lambda (env)
    (lambda (e)
      (match e
        [(? symbol?) (cdr (assq e env))]
        [(? integer?) e]
        [(? boolean?) e]
        [`(if ,cnd ,thn ,els)
         `(if ,((uniquify env) cnd)
              ,((uniquify env) thn)
              ,((uniquify env) els))]
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
      
      (define (flatten-if thn-temp thn-stms els-temp els-stms vars)
        (lambda (cnd)
          (match cnd
            [#t (values thn-temp thn-stms vars)]
            [#f (values els-temp els-stms vars)] 
            [`(not ,e) ((flatten-if els-temp els-stms thn-temp thn-stms vars) e)]
            [`(,cmp ,e1 ,e2) #:when (set-member? cmp-instructions cmp) 
             (let-values ([(new-e1 e1-stms e1-vars) ((flatten #t) e1)]
                          [(new-e2 e2-stms e2-vars) ((flatten #t) e2)])
               (let ([if-temp (gensym 'if.)])
                 (values if-temp
                         `(,@e1-stms
                           ,@e2-stms
                           (if (eq? ,new-e1 ,new-e2)
                               (,@thn-stms (assign ,if-temp ,thn-temp))
                               (,@els-stms (assign ,if-temp ,els-temp))))
                         (cons if-temp (append e1-vars e2-vars vars)))))]
            [`(let ([,x ,e]) ,body) 
             (let*-values ([(new-e e-stms e-vars) ((flatten #f) e)]
                           [(new-body body-stms body-vars)
                            ((flatten-if thn-temp thn-stms els-temp els-stms vars) body)])
                  (values new-body
                          `(,@e-stms 
                            (assign ,x ,new-e) 
                            ,@body-stms)
                          (cons x (append e-vars body-vars vars))))])))
      (match e
        [(? symbol?) (values e '() '())]
        [(? integer?) (values e '() '())]
        [(? boolean?) (values e '() '())]                     
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
  (lambda (live-after graph mgraph)
    
    (define var? (lambda (x) (equal? (car x) 'var)))
    (define get-var (lambda (x) (cadr x)))
    
    (lambda (ast)
      (match ast
        
       [`(program (,vars ,lives) ,instrs)
        (let ([graph (make-graph vars)]
              [mgraph (make-graph vars)])
          (let ([new-instrs
                 (for/list ([inst instrs] [live-after lives])
                           ((build-interference live-after graph mgraph) inst))])
            `(program (,vars ,graph ,mgraph) ,@new-instrs)))]
        
        [`(movq ,src ,dst) 
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

(define choose-color
  (lambda (var interfered mgraph col-map satu)
    (let* ([move-related-vars (hash-ref mgraph var)]
           [non-interfered (set-subtract move-related-vars
                                        interfered)]
           [non-interfered-alloc (filter (lambda (var) (hash-has-key? col-map var))
                                         (set->list non-interfered))])
      (cond [(not (null? non-interfered-alloc)) (hash-ref col-map (car non-interfered-alloc))]
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
           [adjs (car (hash-ref hash key))])
      (set-for-each adjs
                    (lambda (var)
                      (let ([adj (car (hash-ref hash var))]
                            [sat (cadr (hash-ref hash var))])
                        (hash-set! hash var `(,adj ,(set-add sat new-color))))))
      (hash->heap hash))))
  
(define (color-graph graph mgraph)
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
            (let ([color (choose-color char adjs mgraph map-col (set->list saturations))])
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
        [`(program (,vars ,graph ,mgraph) ,instrs ...)
         (let* ([annot-graph (annotate graph)]
                [color-map ((color-graph annot-graph mgraph) vars)])
           (let-values ([(reg-map stk-size) (reg-spill color-map)])
;             (list graph color-map))]
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


; (define compile 
;   (lambda (e)
;     (let* (
;            ; [checked ((type-check (void) 0) e)]
;            [uniq ((uniquify '()) e)]
;            [flat ((flatten #t) uniq)]
;            ; [instrs (select-instructions flat)]
;            ; [liveness ((uncover-live (void)) instrs)]
;            ; [graph ((build-interference (void) (void) (void)) liveness)]
;            ; [reg-alloc ((allocate-registers (void)) graph)]
;            ; [patched (patch-instructions reg-alloc)]
;            ; [x86 (print-x86 patched)]
;           )
;       ; (define out (open-output-file #:exists 'replace "assembly/output.s"))
;       ; (display x86 out)
;       ; (close-output-port out)
;       (pretty-print flat))))


(define passes
 (list
  `("uniquify"              ,(uniquify '())          ,interp-scheme)
  `("flatten"               ,(flatten #f)            ,interp-C)
  ; `("instruction selection" ,select-instructions     ,interp-x86)
  ; `("assign homes"          ,(assign-homes (void))   ,interp-x86)
  ; `("insert spill code"     ,patch-instructions      ,interp-x86)
 ))



; (compile 
;   '(program
; (if (eq? (read) 1) 42 0))
; )


; (compile '(program (+ 13 22)))



