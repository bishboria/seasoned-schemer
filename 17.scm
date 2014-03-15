(load "helpers")

(define deep
  (lambda (m)
    (if (zero? m)
	'pizza
	(cons (deep (sub1 m))
	      '()))))

(print (deep 3))

(define deepM
  (let ((Rs '())
	(Ns '()))
    (letrec
	((D (lambda (m)
	      (if (zero? m)
		  'pizza
		  (cons (deepM (sub1 m))
			'())))))
      (lambda (n)
	(let ((exists (find n Ns Rs)))
	  (if (atom? exists)
	      (let ((result (D n)))
		(set! Rs (cons result Rs))
		(set! Ns (cons n Ns))
		result)
	      exists))))))

(print (deepM 3))

(set! deepM
      (let ((Rs '())
	    (Ns '()))
	(let
	    ((D (lambda (m)
		  (if (zero? m)
		      'pizza
		      (cons (deepM (sub1 m))
			    '())))))
	  (lambda (n)
	    (let ((exists (find n Ns Rs)))
	      (if (atom? exists)
		  (let ((result (D n)))
		    (set! Rs (cons result Rs))
		    (set! Ns (cons n Ns))
		    result)
		  exists))))))
(print (deepM 3))

(set! deepM
      (let ((Rs '())
	    (Ns '())
	    (D (lambda (m)
		 (if (zero? m)
		     'pizza
		     (cons (deepM (sub1 m))
			   '())))))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (atom? exists)
		(let ((result (D n)))
		  (set! Rs (cons result Rs))
		  (set! Ns (cons n Ns))
		  result)
		exists)))))

(set! deepM
      (let ((Rs '())
	    (Ns '()))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (atom? exists)
		(let ((result ((lambda (m)
				 (if (zero? m)
				     'pizza
				     (cons (deepM (sub1 m))
					   '())))
			       n)))
		  (set! Rs (cons result Rs))
		  (set! Ns (cons n Ns))
		  result)
		exists)))))

(print (deepM 3))

(set! deepM
      (let ((Rs '())
	    (Ns '()))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (atom? exists)
		(let ((result (let ((m n))
				(if (zero? m)
				    'pizza
				    (cons (deepM (sub1 m))
					  '())))))
		  (set! Rs (cons result Rs))
		  (set! Ns (cons n Ns))
		  result)
		exists)))))

(print (deepM 3))

(set! deepM
      (let ((Rs '())
	    (Ns '()))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (atom? exists)
		(let ((result (if (zero? n)
				  'pizza
				  (cons (deepM (sub1 n))
					'()))))
		  (set! Rs (cons result Rs))
		  (set! Ns (cons n Ns))
		  result)
		exists)))))

(print (deepM 3))

(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(set! deep
      (lambda (m)
	(if (zero? m)
	    'pizza
	    (consC (deep (sub1 m))
		   '()))))

(print (deep 3))

(define counter)
(set! consC
      (let ((N 0))
	(set! counter
	      (lambda ()
		N))
	(lambda (x y)
	  (set! N (add1 N))
	  (cons x y))))

(print (deep 3))
(print (counter))

(print (deep 5))
(print (counter))

(define supercounter
  (lambda (f)
    (letrec
	((S (lambda (n)
	      (if (zero? n)
		  (f n)
		  (let ()
		    (f n)
		    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(print (supercounter deep))

(set! counter)
(define set-counter)
(set! consC
      (let ((N 0))
	(set! counter
	      (lambda ()
		N))
	(set! set-counter
	      (lambda (x)
		(set! N x)))
	(lambda (x y)
	  (set! N (add1 N))
	  (cons x y))))

(print (supercounter deep))

(set! deepM
      (let ((Rs '())
	    (Ns '()))
	(lambda (n)
	  (let ((exists (find n Ns Rs)))
	    (if (atom? exists)
		(let ((result (if (zero? n)
				  'pizza
				  (consC (deepM (sub1 n))
					 '()))))
		  (set! Rs (cons result Rs))
		  (set! Ns (cons n Ns))
		  result)
		exists)))))

(print (deepM 5))
(print (counter))

(set-counter 0)
(print (deepM 5))
(print (counter))


(print (deep 7))
(print (counter))

(print (supercounter deepM))
(print (counter))

(define rember1*C
  (lambda (a l)
    (letrec
	((R (lambda (l oh)
	      (cond ((null? l) (oh 'no))
		    ((atom? (car l))
		       (if (eq? a (car l))
			   (cdr l)
			   (consC (car l)
				  (R (cdr l) oh))))
		    (else
		       (let ((new-car (letcc oh (R (car l)
						   oh))))
			 (if (atom? new-car)
			     (consC (car l)
				    (R (cdr l) oh))
			     (consC new-car
				    (cdr l)))))))))
      (let ((new-l (letcc oh (R l oh))))
	(if (atom? new-l)
	    l
	    new-l)))))

(set-counter 0)
(print (rember1*C 'noodles '((food) more (food))))
(print (counter))

(define rember1*C2
  (lambda (a l)
    (letrec
	((R (lambda (l)
	      (cond ((null? l) '())
		    ((atom? (car l))
		       (if (eq? a (car l))
			   (cdr l)
			   (consC (car l)
				  (R (cdr l)))))
		    (else (let ((av (R (car l))))
			    (if (equal? av (car l))
				(consC (car l)
				       (R (cdr l)))
				(consC av
				       (cdr l)))))))))
      (R l))))

(set-counter 0)

(print (consC (consC 'food '())
	      (consC 'more
		     (consC (consC 'food '())
			    '()))))
(print (counter))
(set-counter 0)

(print (rember1*C2 'noodles '((food) more (food))))
(print (counter))
