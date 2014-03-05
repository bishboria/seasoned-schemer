(load "helpers")

(define Y
  (lambda (g)
    ((lambda (f) (f f))
     (lambda (f)
       (g (lambda (x) ((f f) x)))))))

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
	  (lambda (lat)
	    (cond ((null? lat) '())
		  ((eq? a (car lat)) (mr (cdr lat)))
		  (else (cons (car lat)
			      (mr (cdr lat))))))))
     lat)))

(print (multirember 'tuna '(shrimp salad tuna salad and tuna)))

(define length
  ((lambda (le)
     ((lambda (f) (f f))
      (lambda (f)
	(le (lambda (x) ((f f) x))))))
   (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
	     (else (add1 (length (cdr l)))))))))

(set! length
      (Y (lambda (length)
	   (lambda (l)
	     (cond ((null? l) 0)
		   (else (add1 (length (cdr l)))))))))

(set! multirember
      (lambda (a lat)
	((letrec
	     ((mr (lambda (lat)
		    (cond ((null? lat) '())
			  ((eq? a (car lat))
			     (mr (cdr lat)))
			  (else (cons (car lat)
				      (mr (cdr lat))))))))
	   mr)
	 lat)))

;; (letrec ((mr ...)) mr)
;; the last `mr` returns the value of `letrec`, i.e. the function
;; `mr` that was defined inside `((mr ...))`
;; which is then applied to `lat`.

(print (multirember 'pie '(apple custard pie linzer pie torte)))

(set! multirember
      (lambda (a lat)
	(letrec
	    ((mr (lambda (lat)
		   (cond ((null? lat) '())
			 ((eq? a (car lat))
			    (mr (cdr lat)))
			 (else (cons (car lat)
				     (mr (cdr lat))))))))
	  (mr lat))))
;; now the thing returned from `letrec` is the value of `(mr lat)`

(print (multirember 'pie '(apple custard pie linzer pie torte)))



;; The Twelfth Commandment
;;
;; Use `letrec` to remove arguments that do not change for
;; recursive applications.


(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
	    ((test? a (car l))
	       (cdr l))
	    (else (cons (car l)
			((rember-f test?) a
			                  (cdr l))))))))

(define rember-eq? (rember-f eq?))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
	    ((test? a (car l))
	       ((multirember-f test?) a (cdr l)))
	    (else (cons (car l)
			((multirember-f test?) a (cdr l))))))))

(set! multirember-f
      (lambda (test?)
	(letrec
	    ((m-f
	       (lambda (a lat)
		 (cond ((null? lat) '())
		       ((test? a (car lat))
			  (m-f a (cdr lat)))
		       (else (cons (car lat)
				   (m-f a (cdr lat))))))))
	  m-f)))

(print ((multirember-f eq?) 'apple '(apple banana custard apple pie banana)))

(set! multirember
      (letrec
	  ((mr (lambda (a lat)
		 (cond ((null? lat) '())
		       ((eq? a (car lat))
			  (mr a (cdr lat)))
		       (else (cons (car lat)
				   (mr a (cdr lat))))))))
	mr))

(set! multirember
      (letrec
	  ((multirember (lambda (a lat)
			  (cond ((null? lat) '())
				((eq? a (car lat))
				   (multirember a (cdr lat)))
				(else (cons (car lat)
					    (multirember a (cdr lat))))))))
	multirember))

(define member?
  (lambda (a lat)
    (letrec
	((m? (lambda (lat)
	       (cond ((null? lat) #f)
		     ((eq? a (car lat)) #t)
		     (else (m? (cdr lat)))))))
      (m? lat))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
	  ((member? (car set1) set2)
	     (union (cdr set1) set2))
	  (else (cons (car set1)
		      (union (cdr set1) set2))))))

(print (union '(tomatoes and macaroni casserole)
	      '(macaroni and cheese)))


(set! union
      (lambda (s1 s2)
	(letrec
	    ((U (lambda (set)
		  (cond ((null? set) s2)
			((member? (car set) s2)
			   (U (cdr set)))
			(else (cons (car set)
				    (U (cdr set))))))))
	  (U s1))))

(print (union '(tomatoes and macaroni casserole)
	      '(macaroni and cheese)))

;; if member is defined
(set! member?
      (lambda (lat a)
	(cond ((null? lat) #f)
	      ((eq? (car lat) a) #t)
	      (else (member? (cdr lat) a)))))
;; union is now broken

(set! union
      (lambda (s1 s2)
	(letrec
	    ((U (lambda (set)
		  (cond ((null? set) s2)
			((member? (car set) s2)
			   (U (cdr set)))
			(else (cons (car set)
				    (U (cdr set)))))))
	     (member? (lambda (a lat)
			(cond ((null? lat) #f)
			      ((eq? a (car lat)) #t)
			      (else (member? a (cdr lat)))))))
	  (U s1))))

(print (union '(tomatoes and macaroni casserole)
	      '(macaroni and cheese)))



;; The Thirteenth Commandment
;;
;; Use `letrec` to hide and to protect functions.

(set! union
      (lambda (s1 s2)
	(letrec
	    ((U (lambda (set)
		  (cond ((null? set) s2)
			((M? (car set) set2)
			   (U (cdr set)))
			(else (cons (car set)
				    (U (cdr set)))))))
	     (M? (lambda (a lat)
		   (letrec
		       ((N? (lambda (lat)
			      (cond ((null? lat) #f)
				    ((eq? a (car lat)) #t)
				    (else (N? (cdr lat)))))))
		     (N? lat)))))
	  (U s1))))

(define two-in-a-row?
  (lambda (lat)
    (letrec
	((W (lambda (a lat)
	      (cond ((null? lat) #f)
		    (else (or (eq? a (car lat))
			      (W (car lat)
				 (cdr lat))))))))
      (cond ((null? lat) #f)
	    (else (W (car lat)
		     (cdr lat)))))))

(print (two-in-a-row? '(a b c a b c c)))

(set! two-in-a-row?
      (letrec
	  ((W (lambda (a lat)
		(cond ((null? lat) #f)
		      (else (or (eq? a (car lat))
				(W (car lat)
				   (cdr lat))))))))
	(lambda (lat)
	  (cond ((null? lat) #f)
		(else (W (car lat) (cdr lat)))))))

(print (two-in-a-row? '(a b c a b c c)))

(define sum-of-prefixes
  (letrec
      ((S (lambda (acc tup)
	    (cond ((null? tup) '())
		  (else (cons (+ acc (car tup))
			      (S (+ acc (car tup))
				 (cdr tup))))))))
    (lambda (tup)
      (S 0 tup))))

(print (sum-of-prefixes '(1 2 3 4 5 6 7 8 9 10)))

(define scramble
  (letrec
      ((P (lambda (tup rp)
	    (cond ((null? tup) '())
		  (else (cons (pick (car tup)
				    (cons (car tup) rp))
			      (P (cdr tup)
				 (cons (car tup) rp))))))))
    (lambda (tup)
      (P tup '()))))

(print (scramble '(1 2 3 4 5 6 7 8 9)))
