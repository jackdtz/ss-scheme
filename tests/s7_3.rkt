(let ([x (vector 1 2 3 42)])
  (let ([voidy (vector-set! x 1 42)])
    (let ([voidy (vector-set! x 0 42)])
      ((lambda (vec) (vector-ref vec 3)) x))))

