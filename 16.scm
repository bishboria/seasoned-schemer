(load "helpers")

(define sweet-tooth
  (lambda (food)
    (cons food
	  (cons 'cake
		()))))

(define last 'angelfood)

(print (sweet-tooth 'chocolate))
(print last)
(print (sweet-tooth 'fruit))
(print last)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
	  (cons 'cake
		()))))

(print (sweet-toothL 'chocolate))
(print last)
(print (sweet-toothL 'fruit))
(print last)

(print (sweet-toothL 'cheese))
(print (sweet-toothL 'carrot))

(define ingredients ())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
	  (cons 'cake
		()))))

(print (sweet-toothR 'chocolate))
(print ingredients)

(print (sweet-toothR 'fruit))
(print ingredients)

(print (sweet-toothR 'cheese))
(print ingredients)

(define deep
  (lambda (m)
    (cond ((zero? m) 'pizza)
	  (else (cons (deep (sub1 m))
		      ())))))

(print (deep 3))
(print (deep 7))
(print (deep 0))

(define Ns ())

(define deepR
  (lambda (n)
    (set! Ns (cons n Ns))
    (deep n)))

(print (deepR 1))
(print (deepR 2))
(print Ns)

(set! Ns ())
(define Rs ())

(set! deepR
      (lambda (n)
	(let ((result (deep n)))
	  (set! Rs (cons result Rs))
	  (set! Ns (cons n Ns))
	  result)))

(print (deepR 3))
(print (deepR 5))
(print Ns)
(print Rs)



;; The Nineteenth Commandment
;;
;; Use `set!` to remember value things between
;; two distinct uses of a function.


(print (deepR 3))
(print Rs)

(define find
  (lambda (n Ns Rs)
    (letrec
	((A (lambda (ns rs)
	      (cond ((= n (car ns)) (car rs))
		    (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(print (find 3 Ns Rs))
(print (find 5 Ns Rs))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  ((eq? a (car lat)) #t)
	  (else (member? a (cdr lat))))))

(define deepM
  (lambda (n)
    (if (member? n Ns)
	(find n Ns Rs)
	(deepR n))))

(print Ns)
(print Rs)

; Remove the duplicate from Ns and Rs
(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

(print Ns)
(print Rs)

(set! deepM
      (lambda (n)
	(if (member n Ns)
	    (find n Ns Rs)
	    (let ((result (deep n)))
	      (set! Rs (cons result Rs))
	      (set! Ns (cons n Ns))
	      result))))
(print (deepM 3))
(print Ns)

(print (deepM 6))
(print Ns)

(set! deep
      (lambda (m)
	(cond ((zero? m) 'pizza)
	      (else (cons (deepM (sub1 m))
			  ())))))

(print (deepM 9))
(print Ns)

(set! deepM
      (let ((Rs ())
	    (Ns ()))
	(lambda (n)
	  (if (member? n Ns)
	      (find n Ns Rs)
	      (let ((result (deep n)))
		(set! Rs (cons result Rs))
		(set! Ns (cons n Ns))
		result)))))

(print (deepM 16))

(set! find
      (lambda (n Ns Rs)
	(letrec
	    ((A (lambda (ns rs)
		  (cond ((null? ns) #f)
			((= n (car ns)) (car rs))
			(else (A (cdr ns) (cdr rs)))))))
	  (A Ns Rs))))

(set! deepM
      (let ((Rs ())
	    (Ns ()))
	(lambda (n)
	  (if (atom? (find n Ns Rs))
	      (let ((result (deep n)))
		(set! Rs (cons result Rs))
		(set! Ns (cons n Ns))
		result)
	      (find n Ns RS)))))
; This has find repeated...


(set! deepM
      (let ((Rs ())
	    (Ns ()))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (atom? exists)
		(let ((result (deep n)))
		  (set! Rs (cons result Rs))
		  (set! Ns (cons n Ns))
		  result)
		exists)))))

(print (deepM 10))

(define length
  (lambda (l)
    0))

(set! length
      (lambda (l)
	(cond ((null? l) 0)
	      (else (add1 (length (cdr l)))))))

(set! length
      (let ((h (lambda (l) 0)))
	(set! h
	      (lambda (l)
		(cond ((null? l) 0)
		      (else (add1 (h (cdr l)))))))
	h))

(print (length '(a b c)))



;; The Seventeenth Commandment
;;
;; (final version)
;;
;; Use `(set! x ...)` for `(let ((x ...)) ...)` only if there is at least
;; one `(lambda ...)` between it and the `(let ...)`, or if the new value
;; for `x` is a function that refers to `x`.

(define L
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
	    (else (add1 (length (cdr l))))))))

(set! length
      (let ((h (lambda (l) 0)))
	(set! h (L (lambda (arg) (h arg))))
	h))

;; The applicative order, imperative Y combinator
(define Y!
  (lambda (L)
    (let ((h (lambda (l) ())))
      (set! h (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec ((h (f (lambda (arg) (h arg)))))
      h)))

;; The applicative order Y combinator (from chapter 9)
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(set! length (Y! L))
(print (length '(a b c)))

(define D
  (lambda (depth*)
    (lambda (s)
      (cond ((null? s) 1)
	    ((atom? (car s))
	       (depth* (cdr s)))
	    (else (max (add1 (depth* (car s)))
		       (depth* (cdr s))))))))

(define bizzare
  (let ((x 0))
    (lambda (f)
      (print '(f given))
      (print x)
      (set! x (add1 x))
      (print x)
      (lambda (a)
	(print '(a given))
	(print x)
	(if (= a x)
	    0
	    (f a))))))

; Does as expected: because f is called multiple times at the point
; of recursion, x is incremented until it reaches a and then stops
; returning 0 (the consequent of the if function)
;(print '((Y bizzare) 5))
;(print ((Y bizzare) 5))

; Y! only passes f once, and then calls the inner lambda (that accepts `a`)
; over and over. Strangely, x seems to be initially set to 5... adding 1 to
; this then makes it 6. The outer lambda is only called once, so x always is 6
; and a is always 5. Hence it never terminates.
;
; Why x = 5 to begin with though?
;
; Commenting out the above code, x starts at 0. Y! is picking up the
; previous value of x set by bizzare.
; Same case applies. the lambda with f is called only once, then the lambda
; with a is called repeatedly.
(print '((Y! bizzare) 5))
;(print (Y! bizzare)) ; uncomment this line, and it terminates.
(print ((Y-BANG bizzare) 2))
