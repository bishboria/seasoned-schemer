(define print
  (lambda (printable)
    (display printable)
    (newline)))

(define atom?
  (lambda (x)
    (and (not (pair? x))
	 (not (null? x)))))

(define add1
  (lambda (n)
    (+ 1 n)))

(define sub1
  (lambda (n)
    (- n 1)))

(define one?
  (lambda (n)
    (= 1 n)))

(define pick
  (lambda (n lat)
    (cond ((one? n) (car lat))
	  (else (pick (sub1 n) (cdr lat))))))

(define-syntax try
  (syntax-rules
      ()
    ((try x a b)
     (call-with-current-continuation
       (lambda (success)
	 (call-with-current-continuation
	   (lambda (x)
	     (success a)))
	 b)))))

'helpers-loaded
