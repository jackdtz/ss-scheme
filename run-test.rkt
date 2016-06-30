#lang racket

(require "utilities/utilities.rkt")
(require "utilities/interp.rkt")
(require "compiler.rkt")



(define log 
  (lambda (message)
    (display message) (newline)))

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
  `(("conditionals"      #f                        ,passes               ,interp-scheme       "s0"         ,(cdr (assq 0 suite-list)))
    ;("reg_int_exp"  #f                        ,reg-int-exp-passes  ,interp-scheme                    (0))
    ;("conditionals" ,conditionals-typechecker ,conditionals-passes ,interp-scheme                    (0 1))
    ;("vectors"      ,vectors-typechecker      ,vectors-passes      ,interp-scheme       (0 1 2))
    ;("functions"    ,functions-typechecker    ,functions-passes    ,interp-scheme       (0 1 2 3))
    ;("lambda"       ,lambda-typechecker       ,lambda-passes       ,interp-scheme       (0 1 2 3 4))
    ;("any"          ,R6-typechecker           ,R6-passes           ,interp-scheme       (0 1 2 3 4 6))
    ;("dynamic"      #f                        ,R7-passes           ,(interp-r7 '())     (7))
    ))


(begin
  (for ([test compiler-list])
   (apply interp-tests test))
  (log "all passed"))
