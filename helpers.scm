(define print
  (lambda (printable)
    (display printable)
    (newline)))

(define add1
  (lambda (n)
    (+ 1 n)))

(define sub1
  (lambda (n)
    (- n 1)))

(define one?
  (lambda (n)
    (= 1 n)))
