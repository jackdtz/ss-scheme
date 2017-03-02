This compiler implements a Scheme-like statically-typed programming language.

It currently supports:

- Function
- Lambda function (lexically-scoped funtion)
- Integer, Boolean, Vector type
- Garbage collection



Example:

```
(define (add [x : Integer]) : (Integer -> Integer)
      (lambda: ([y : Integer]) : Integer
        (+ x y)))
    
    ((add 10) 20)

```

Type check phase:


```
(program
 (type Integer)
 (define (add (x : Integer))
   :
   (Integer -> Integer)
   (has-type
    (lambda:
     ((y : Integer))
     :
     Integer
     (has-type (+ (has-type x Integer) (has-type y Integer)) Integer))
    (Integer -> Integer)))
 (has-type
  ((has-type
    ((has-type add (Integer -> (Integer -> Integer))) (has-type 10 Integer))
    (Integer -> Integer))
   (has-type 20 Integer))
  Integer))
```

Uniquify phase(Make every variable name unique)

```
(program
 (type Integer)
 (define (add161915 (x161916 : Integer))
   :
   (Integer -> Integer)
   (has-type
    (lambda:
     ((y161917 : Integer))
     :
     Integer
     (has-type
      (+ (has-type x161916 Integer) (has-type y161917 Integer))
      Integer))
    (Integer -> Integer)))
 (has-type
  ((has-type
    ((has-type add161915 (Integer -> (Integer -> Integer)))
     (has-type 10 Integer))
    (Integer -> Integer))
   (has-type 20 Integer))
  Integer))

```


flatten (convert code to three address code)

```
(program
 ((app.14108 Integer -> Integer)
  (app.14107 Vector _)
  (vec-elt.14116 . _)
  (temp.14119 . _)
  (collectret.14118 . Void)
  (if.14123 . Void)
  (temp.14121 . Integer)
  (global.14120 . Integer)
  (global.14122 . Integer)
  (alloc-addr.14115 Vector _)
  (initrec.14117 . Void)
  (temp.14124 . _)
  (temp.14126 . Integer)
  (temp.14125 . _))
 (type Integer)
 (defines
  (define (add14101 (fvs.14104 : _) (x14102 : Integer))
    :
    (Integer -> Integer)
    ((vec-elt.14110 . _)
     (temp.14127 . _)
     (vec-elt.14111 . Integer)
     (collectret.14114 . Void)
     (if.14131 . Void)
     (temp.14129 . Integer)
     (global.14128 . Integer)
     (global.14130 . Integer)
     (alloc-addr.14109 Vector _ Integer)
     (initrec.14113 . Void)
     (initrec.14112 . Void))
    (assign temp.14127 (function-ref lambda.14106))
    (assign vec-elt.14110 temp.14127)
    (assign vec-elt.14111 x14102)
    (assign global.14128 (global-value free_ptr))
    (assign temp.14129 (+ global.14128 24))
    (assign global.14130 (global-value fromspace_end))
    (if (< temp.14129 global.14130)
      ((assign if.14131 (void)))
      ((collect 24) (assign if.14131 (void))))
    (assign collectret.14114 if.14131)
    (assign alloc-addr.14109 (allocate 2 (Vector _ Integer)))
    (assign initrec.14113 (vector-set! alloc-addr.14109 0 vec-elt.14110))
    (assign initrec.14112 (vector-set! alloc-addr.14109 1 vec-elt.14111))
    (return alloc-addr.14109))
  (define (lambda.14106 (fvs.14105 : _) (y14103 : Integer))
    :
    Integer
    ((x14102 . Integer) (temp.14132 . Integer))
    (assign x14102 (vector-ref fvs.14105 1))
    (assign temp.14132 (+ x14102 y14103))
    (return temp.14132)))
 (assign temp.14119 (function-ref add14101))
 (assign vec-elt.14116 temp.14119)
 (assign global.14120 (global-value free_ptr))
 (assign temp.14121 (+ global.14120 16))
 (assign global.14122 (global-value fromspace_end))
 (if (< temp.14121 global.14122)
   ((assign if.14123 (void)))
   ((collect 16) (assign if.14123 (void))))
 (assign collectret.14118 if.14123)
 (assign alloc-addr.14115 (allocate 1 (Vector _)))
 (assign initrec.14117 (vector-set! alloc-addr.14115 0 vec-elt.14116))
 (assign app.14107 alloc-addr.14115)
 (assign temp.14124 (vector-ref app.14107 0))
 (assign app.14108 (app temp.14124 app.14107 10))
 (assign temp.14125 (vector-ref app.14108 0))
 (assign temp.14126 (app temp.14125 app.14108 20))
 (return temp.14126))
```

Instruction selection (Conver to IR)

```
(program
 ((app.14108 Integer -> Integer)
  (app.14107 Vector _)
  (vec-elt.14116 . _)
  (temp.14119 . _)
  (collectret.14118 . Void)
  (if.14123 . Void)
  (temp.14121 . Integer)
  (global.14120 . Integer)
  (global.14122 . Integer)
  (alloc-addr.14115 Vector _)
  (initrec.14117 . Void)
  (temp.14124 . _)
  (temp.14126 . Integer)
  (temp.14125 . _))
 (type Integer)
 (defines
  (define (add14101)
    2
    (((vec-elt.14110 . _)
      (temp.14127 . _)
      (vec-elt.14111 . Integer)
      (collectret.14114 . Void)
      (if.14131 . Void)
      (temp.14129 . Integer)
      (global.14128 . Integer)
      (global.14130 . Integer)
      (alloc-addr.14109 Vector _ Integer)
      (initrec.14113 . Void)
      (initrec.14112 . Void)
      (fvs.14104 . _)
      (x14102 . Integer))
     0)
    (movq (reg rdi) (var fvs.14104))
    (movq (reg rsi) (var x14102))
    (leaq (function-ref lambda.14106) (var temp.14127))
    (movq (var temp.14127) (var vec-elt.14110))
    (movq (var x14102) (var vec-elt.14111))
    (movq (global-value free_ptr) (var global.14128))
    (movq (var global.14128) (var temp.14129))
    (addq (int 24) (var temp.14129))
    (movq (global-value fromspace_end) (var global.14130))
    (if (< (var temp.14129) (var global.14130))
      ((movq (int 0) (var if.14131)))
      ((movq (reg r15) (reg rdi))
       (movq (int 24) (reg rsi))
       (callq collect)
       (movq (int 0) (var if.14131))))
    (movq (var if.14131) (var collectret.14114))
    (movq (global-value free_ptr) (var alloc-addr.14109))
    (addq (int 24) (global-value free_ptr))
    (movq (var alloc-addr.14109) (reg r11))
    (movq (int 5) (deref r11 0))
    (movq (var alloc-addr.14109) (reg r11))
    (movq (var vec-elt.14110) (deref r11 8))
    (movq (int 0) (var initrec.14113))
    (movq (var vec-elt.14111) (deref r11 16))
    (movq (int 0) (var initrec.14112))
    (movq (var alloc-addr.14109) (reg rax)))
  (define (lambda.14106)
    2
    (((x14102 . Integer)
      (temp.14132 . Integer)
      (fvs.14105 . _)
      (y14103 . Integer))
     0)
    (movq (reg rdi) (var fvs.14105))
    (movq (reg rsi) (var y14103))
    (movq (var fvs.14105) (reg r11))
    (movq (deref r11 16) (var x14102))
    (movq (var x14102) (var temp.14132))
    (addq (var y14103) (var temp.14132))
    (movq (var temp.14132) (reg rax))))
 (leaq (function-ref add14101) (var temp.14119))
 (movq (var temp.14119) (var vec-elt.14116))
 (movq (global-value free_ptr) (var global.14120))
 (movq (var global.14120) (var temp.14121))
 (addq (int 16) (var temp.14121))
 (movq (global-value fromspace_end) (var global.14122))
 (if (< (var temp.14121) (var global.14122))
   ((movq (int 0) (var if.14123)))
   ((movq (reg r15) (reg rdi))
    (movq (int 16) (reg rsi))
    (callq collect)
    (movq (int 0) (var if.14123))))
 (movq (var if.14123) (var collectret.14118))
 (movq (global-value free_ptr) (var alloc-addr.14115))
 (addq (int 16) (global-value free_ptr))
 (movq (var alloc-addr.14115) (reg r11))
 (movq (int 3) (deref r11 0))
 (movq (var alloc-addr.14115) (reg r11))
 (movq (var vec-elt.14116) (deref r11 8))
 (movq (int 0) (var initrec.14117))
 (movq (var alloc-addr.14115) (var app.14107))
 (movq (var app.14107) (reg r11))
 (movq (deref r11 8) (var temp.14124))
 (movq (var app.14107) (reg rdi))
 (movq (int 10) (reg rsi))
 (indirect-callq (var temp.14124))
 (movq (reg rax) (var app.14108))
 (movq (var app.14108) (reg r11))
 (movq (deref r11 8) (var temp.14125))
 (movq (var app.14108) (reg rdi))
 (movq (int 20) (reg rsi))
 (indirect-callq (var temp.14125))
 (movq (reg rax) (var temp.14126))
 (movq (var temp.14126) (reg rax)))
```

Register allocation

```
(program
 (0 0)
 (type Integer)
 (defines
  (define (add14101)
    2
    (0 0)
    (movq (reg rdi) (reg rbx))
    (movq (reg rsi) (reg rbx))
    (leaq (function-ref lambda.14106) (reg rcx))
    (movq (reg rcx) (reg r12))
    (movq (reg rbx) (reg rbx))
    (movq (global-value free_ptr) (reg rcx))
    (movq (reg rcx) (reg rcx))
    (addq (int 24) (reg rcx))
    (movq (global-value fromspace_end) (reg rdx))
    (if (< (reg rcx) (reg rdx))
      ((movq (int 0) (reg rcx)))
      ((movq (reg r15) (reg rdi))
       (movq (int 24) (reg rsi))
       (callq collect)
       (movq (int 0) (reg rcx))))
    (movq (reg rcx) (reg rcx))
    (movq (global-value free_ptr) (reg rcx))
    (addq (int 24) (global-value free_ptr))
    (movq (reg rcx) (reg r11))
    (movq (int 5) (deref r11 0))
    (movq (reg rcx) (reg r11))
    (movq (reg r12) (deref r11 8))
    (movq (int 0) (reg rdx))
    (movq (reg rbx) (deref r11 16))
    (movq (int 0) (reg rbx))
    (movq (reg rcx) (reg rax)))
  (define (lambda.14106)
    2
    (0 0)
    (movq (reg rdi) (reg rcx))
    (movq (reg rsi) (reg rbx))
    (movq (reg rcx) (reg r11))
    (movq (deref r11 16) (reg rcx))
    (movq (reg rcx) (reg rcx))
    (addq (reg rbx) (reg rcx))
    (movq (reg rcx) (reg rax))))
 (leaq (function-ref add14101) (reg rbx))
 (movq (reg rbx) (reg rbx))
 (movq (global-value free_ptr) (reg rcx))
 (movq (reg rcx) (reg rdx))
 (addq (int 16) (reg rdx))
 (movq (global-value fromspace_end) (reg rcx))
 (if (< (reg rdx) (reg rcx))
   ((movq (int 0) (reg rcx)))
   ((movq (reg r15) (reg rdi))
    (movq (int 16) (reg rsi))
    (callq collect)
    (movq (int 0) (reg rcx))))
 (movq (reg rcx) (reg rcx))
 (movq (global-value free_ptr) (reg rcx))
 (addq (int 16) (global-value free_ptr))
 (movq (reg rcx) (reg r11))
 (movq (int 3) (deref r11 0))
 (movq (reg rcx) (reg r11))
 (movq (reg rbx) (deref r11 8))
 (movq (int 0) (reg rbx))
 (movq (reg rcx) (reg rcx))
 (movq (reg rcx) (reg r11))
 (movq (deref r11 8) (reg rbx))
 (movq (reg rcx) (reg rdi))
 (movq (int 10) (reg rsi))
 (indirect-callq (reg rbx))
 (movq (reg rax) (reg rcx))
 (movq (reg rcx) (reg r11))
 (movq (deref r11 8) (reg rbx))
 (movq (reg rcx) (reg rdi))
 (movq (int 20) (reg rsi))
 (indirect-callq (reg rbx))
 (movq (reg rax) (reg rbx))
 (movq (reg rbx) (reg rax)))
```






