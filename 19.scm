(load "helpers")

(define deep
  (lambda (m)
    (cond ((zero? m) 'pizza)
	  (else (cons (deep (sub1 m))
		      '())))))

(print (deep 6))

(define six-layers
  (lambda (p)
    (cons
      (cons
        (cons
	  (cons
	    (cons
	      (cons p '())
	      '())
	    '())
	  '())
	'())
      '())))

(print (six-layers 'pizza))

(define four-layers
  (lambda (p)
    (cons
      (cons
        (cons
	  (cons p '())
	  '())
	'())
      '())))

(print (four-layers 'pizza))

(define toppings)
(define deepB
  (lambda (m)
    (cond ((zero? m)
	     (letcc jump
		    (begin
		      (set! toppings jump)
		      'pizza)))
	  (else (cons (deepB (sub1 m))
		      '())))))

(print (deepB 6))
(print (six-layers 'mozzarella))

(print (toppings 'mozzarella))
(print (toppings 'cake))
(print (toppings 'pizza))
(print (cons (toppings 'cake) '())) ; still 6 parens surrounding cake, not 7.

(print (cons
	 (cons
	   (cons (toppings 'mozzarella)
		 '())
	   '())
	 '())) ; again, 6 parens surrounding mozzarella, not 9.

(print (deepB 4))


;; The Twentieth Commandment
;;
;; When thinking about a value created with `letcc`, write down
;; the function that is equivalent but does not forget. Then,
;; when you use it, remember to forget.

(print (cons (toppings 'cake)
	     (toppings 'cake)))

(print (cons (toppings 'cake)
	     (cons (toppings 'mozzarella)
		   (cons (toppings 'pizza)
			 '())))) ; book says '((((cake))))'
                                 ; we get '((((pizza))))' with mit-scheme


(define deep&co
  (lambda (m k)
    (cond ((zero? m) (k 'pizza))
	  (else (deep&co (sub1 m)
			 (lambda (x)
			   (k (cons x '()))))))))

(define id (lambda (x) x))

(print (deep&co 0 id))
(print (deep&co 6 id))
(print (deep&co 2 id))

(define deep&coB
  (lambda (m k)
    (cond ((zero? m)
	     (begin
	       (set! toppings k)
	       (k 'pizza)))
	  (else
	     (deep&coB (sub1 m)
		       (lambda (x)
			 (k (cons x '()))))))))

(print (deep&coB 2 id))
(print (toppings 'steak))

(print (deep&coB 6 id))
(print (toppings 'porkchops))

(print (deep&coB 4 id))
(print (toppings 'bacon))

(print (cons (toppings 'cake)
	     (cons (toppings 'mozzarella)
		   (cons (toppings 'pizza)
			 '()))))

;; toppings defined with deep&coB's k is not the same
;; as deep&co' letcc version. The letcc version bails
;; as soon as it is executed. k does not.


(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
	    (cond ((null? lat) #f)
		  (else
		     (let ((nxt (car lat)))
		       (or (eq? a nxt)
			   (W nxt (cdr lat)))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
	    (else (W (car lat) (cdr lat)))))))

(print (two-in-a-row? '(mozzarella cake mozzarella)))
(print (two-in-a-row? '(mozzarella mozzarella pizza)))

(define leave)
(define walk
  (lambda (l)
    (cond ((null? l) '())
	  ((atom? (car l))
	     (leave (car l)))
	  (else (begin
		  (walk (car l))
		  (walk (cdr l)))))))

(define start-it
  (lambda (l)
    (letcc here
	   (begin
	     (set! leave here)
	     (walk l)))))

(print (start-it '((potato) (chips (chips (with))) fish)))

(define fill)
(define waddle
  (lambda (l)
    (cond ((null? l) '())
	  ((atom? (car l))
	     (begin
	       (letcc rest
		      (begin
			(set! fill rest)
			(leave (car l))))
	       (waddle (cdr l))))
	  (else
	     (begin
	       (waddle (car l))
	       (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (letcc here
	   (begin
	     (set! leave here)
	     (waddle l)))))

(print (start-it2 '((donuts)
		    (cheerios (cheerios (spaghettios)))
		    donuts)))

(define get-next
  (lambda (x)
    (letcc here-again
	   (begin
	     (set! leave here-again)
	     (fill 'go)))))

(print (get-next 'go))
(print (get-next 'go))
(print (get-next 'go))
(print (get-next 'go))
(print (get-next 'go))

(define get-first
  (lambda (l)
    (letcc here
	   (begin
	     (set! leave here)
	     (waddle l)
	     (leave '())))))

(print (get-first '(donut)))
(print (get-next 'go))
(print (get-first '(fish (chips))))
(print (get-next 'go))
(print (get-next 'go))

(print (get-first '(fish (chips) chips)))
(print (get-next 'go))
(print (get-next 'go))

(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst)
	  (two-in-a-row-b*? fst)
	  #f))))

(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next 'go)))
      (if (atom? n)
	  (or (eq? a n)
	      (two-in-a-row-b*? n))
	  #f))))

(print (two-in-a-row*? '((mozzarella) (cake) mozzarella)))
(print (two-in-a-row*? '((potato) (chips ((with) fish) (fish)))))
(print (two-in-a-row*? '((potato) (chips ((with) fish) (chips)))))
(print (two-in-a-row*? '((potato) (chips (chips (with) fish)))))

(set! two-in-a-row*?
      (letrec
	  ((T? (lambda (a)
		 (let ((n (get-next 0)))
		   (if (atom? n)
		       (or (eq? a n)
			   (T? n))
		       #f))))
	   (get-next
	     (lambda (x)
	       (letcc here-again
		      (begin
			(set! leave here-again)
			(fill 'go)))))
	   (fill (lambda (x) x))
	   (waddle
	     (lambda (l)
	       (cond ((null? l) '())
		     ((atom? (car l))
		        (begin
			  (letcc rest
				 (begin
				   (set! fill rest)
				   (leave (car l))))
			  (waddle (cdr l))))
		     (else (begin
			     (waddle (car l))
			     (waddle (cdr l)))))))
	   (leave (lambda (x) (x))))
	(lambda (l)
	  (let ((fst (letcc here
			    (begin
			      (set! leave here)
			      (waddle l)
			      (leave '())))))
	    (if (atom? fst)
		(T? fst)
		#f)))))

(print (two-in-a-row*? '(((food) ()) (((food))))))
