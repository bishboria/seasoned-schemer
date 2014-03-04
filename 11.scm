(load "helpers")

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  (else (or (eq? a (car lat))
		    (member? a (cdr lat)))))))

(print (member? 'sardines '(Italian sardines spaghetti parsley)))

(define is-first?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  (else (eq? a (car lat))))))

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
	  (else (or (is-first? (car lat) (cdr lat))
		    (two-in-a-row? (cdr lat)))))))

(print (two-in-a-row? '(italian sardines spaghetti parsley)))
(print (two-in-a-row? '(italian sardines sardines spaghetti parsley)))
(print (two-in-a-row? '(italian sardines more sardines spaghetti)))

(define two-in-a-row-v2?
  (lambda (lat)
    (cond ((null? lat) #f)
	  (else (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  (else (or (eq? a (car lat))
		    (two-in-a-row-v2? lat))))))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond ((null? lat) #f)
	  (else (or (eq? preceding (car lat))
		    (two-in-a-row-b? (car lat) (cdr lat)))))))

(set! two-in-a-row?
      (lambda (lat)
	(cond ((null? lat) #f)
	      (else (two-in-a-row-b? (car lat)
				     (cdr lat))))))

(print (two-in-a-row? '(b d e i i a g)))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(define sum-of-prefixes-b
  (lambda (acc tup)
    (cond ((null? tup) '())
	  (else (cons (+ acc (car tup))
		      (sum-of-prefixes-b (+ acc (car tup))
					 (cdr tup)))))))

(print (sum-of-prefixes '(2 1 9 17 0)))
(print (sum-of-prefixes '(1 1 1 1 1)))



;; The Eleventh Commandment
;;
;; Use additional arguments when a function needs to know
;; what other arguments to the function have been like so
;; far.


(define pick
  (lambda (n lat)
    (cond ((one? n) (car lat))
	  (else (pick (sub1 n) (cdr lat))))))

(print (pick 4 '(4 3 1 1 1)))
(print (pick 2 '(2 4 3 1 1 1)))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(define scramble-b
  (lambda (tup rev-pre)
    (cond ((null? tup) '())
	  (else (cons (pick (car tup) (cons (car tup)
					    rev-pre))
		      (scramble-b (cdr tup)
				  (cons (car tup)
					rev-pre)))))))

(print (scramble '(1 1 1 3 4 2 1 1 9 2)))
(print (scramble '(1 2 3 4 5 6 7 8 9)))
(print (scramble '(1 2 3 1 2 3 4 1 8 2 10)))
