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

(define-syntax letcc
  (syntax-rules
      ()
    ((letcc foobar foobaz)
     (call-with-current-continuation
      (lambda (foobar)
	foobaz)))))

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

; annoyingly, there is a primitive find in scheme
; that we're redefining.
(define find
  (lambda (n Ns Rs)
    (letrec
	((A (lambda (ns rs)
	      (cond ((null? ns) #f)
		    ((= n (car ns)) (car rs))
		    (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

'helpers-loaded
