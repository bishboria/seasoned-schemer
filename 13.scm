(load "helpers")

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
	  ((eq? a (car lat)) #t)
	  (else (member? a (cdr lat))))))

(define intersect
  (lambda (s1 s2)
    (cond ((null? s1) '())
	  ((member? (car s1) s2)
	     (cons (car s1)
		   (intersect (cdr s1) s2)))
	  (else (intersect (cdr s1) s2)))))

(print (intersect '(tomatoes and macaroni)
		  '(macaroni and cheese)))

(set! intersect
      (lambda (s1 s2)
	(letrec
	    ((I (lambda (set)
		  (cond ((null? set) '())
			((member? (car set) s2)
			   (cons (car set)
				 (I (cdr set))))
			(else (I (cdr set)))))))
	  (I s1))))

(print (intersect '(tomatoes and macaroni)
		  '(macaroni and cheese)))

(define intersectall
  (lambda (lset)
    (cond ((null? (cdr lset)) (car lset))
	  (else (intersect (car lset)
			   (intersectall (cdr lset)))))))

(print (intersectall '((tomatoes and macaroni)
		       (macaroni and cheese)
		       (cheese and steak)
		       (steak and chips))))

(set! intersectall
      (lambda (lset)
	(cond ((null? lset) '())
	      ((null? (cdr lset)) (car lset))
	      (else (intersect (car lset)
			       (intersectall (cdr lset)))))))

(print (intersectall '((tomatoes and macaroni)
		       (macaroni and cheese)
		       (cheese and steak)
		       (steak and chips))))

(set! intersectall
      (lambda (lset)
	(letrec ((intersectall
		   (lambda (lset)
		     (cond ((null? (cdr lset)) (car lset))
			   (else (intersect (car lset)
					    (intersectall (cdr lset))))))))
	  (cond ((null? lset) '())
		(else (intersectall lset))))))

(print (intersectall '((tomatoes and macaroni)
		       (macaroni and cheese)
		       (cheese and steak)
		       (steak and chips))))

(set! intersectall
      (lambda (lset)
	(letrec ((A (lambda (lset)
		      (cond ((null? (cdr lset)) (car lset))
			    (else (intersect (car lset)
					     (A (cdr lset))))))))
	  (cond ((null? lset) '())
		(else (A lset))))))

(print (intersectall '((tomatoes and macaroni)
		       (macaroni and cheese)
		       (cheese and steak)
		       (steak and chips))))
(print (intersectall '((3 mangos and)
		       (3 kiwis and)
		       (3 hamburgers))))

(set! intersectall
      (lambda (lset)
	(call-with-current-continuation
	  (lambda (hop)
	    (letrec ((A (lambda (lset)
			  (cond ((null? (car lset)) (hop '()))
				((null? (cdr lset)) (car lset))
				(else (intersect (car lset)
						 (A (cdr lset))))))))
	      (cond ((null? lset) '())
		    (else (A lset))))))))

(print (intersectall '((3 mangos and)
		       (3 kiwis and)
		       (3 hamburgers))))
(print (Intersectall '((3 mangos and)
		       ()
		       (3 diet hamburgers))))

;; The Fourteenth Commandment
;;
;; Use `letcc`/`call-with-current-continuation` to return values
;; abruptly and promptly.

(print (intersectall '((3 steaks and)
		       (no food and)
		       (three baked potatoes)
		       (3 diet hamburgers))))

(set! intersect
      (lambda (s1 s2)
	(letrec ((I (lambda (set)
		      (cond ((null? set) '())
			    ((member? (car set) s2)
			       (cons (car set)
				     (I (cdr set))))
			    (else (I (cdr set)))))))
	  (cond ((null? s2) '())
		(else (I s1))))))


(set! intersectall
      (lambda (lset)
	(call-with-current-continuation
	  (lambda (hop)
	    (letrec ((A (lambda (lset)
			  (cond ((null? (car lset))
				   (hop '()))
				((null? (cdr lset))
				   (car lset))
				(else (I (car lset)
					 (A (cdr lset)))))))
		     (I (lambda (s1 s2)
			  (letrec ((J (lambda (s1)
					(cond ((null? s1) '())
					      ((member? (car s1) s2)
					         (cons (car s1)
						       (J (cdr s1))))
					      (else (J (cdr s1)))))))
			    (cond ((null? s2) (hop '()))
				  (else (J s1)))))))
	      (cond ((null? lset) '())
		    (else (A lset))))))))

(print (intersectall '((1 2 3)
		       (2 3 4)
		       (3 4 5))))

(define rember
  (lambda (a lat)
    (letrec ((R (lambda (lat)
		  (cond ((null? lat) '())
			((eq? a (car lat))
			   (cdr lat))
			(else (cons (car lat)
				    (R (cdr lat))))))))
      (R lat))))

(print (rember 'a '(b c d a e f)))

(define rember-beyond-first
  (lambda (a lat)
    (letrec ((R (lambda (lat)
		  (cond ((null? lat) '())
			((eq? a (car lat)) '())
			(else (cons (car lat)
				    (R (cdr lat))))))))
      (R lat))))

(print (rember-beyond-first 'roots '(noodles spaghetti
				     spatzle bean-thread
				     roots potatoes yam
				     others rice)))

(print (rember-beyond-first 'others '(noodles spaghetti
				      spatzle bean-thread
				      roots potatoes yam
				      others rice)))

(print (rember-beyond-first 'sweetthing '(noodles spaghetti
					  spatzle bean-thread
					  roots potatoes yam
					  others rice)))

(define rember-upto-last
  (lambda (a lat)
    (call-with-current-continuation
      (lambda (skip)
	(letrec ((R (lambda (lat)
		      (cond ((null? lat) '())
			    ((eq? a (car lat))
			       (skip (R (cdr lat))))
			    (else (cons (car lat)
					(R (cdr lat))))))))
	  (R lat))))))

(print (rember-upto-last 'roots '(noodles spaghetti
				  spatzle bean-thread
				  roots potatoes yam
				  others rice)))

(print (rember-upto-last 'sweetthing '(noodles spaghetti
				       spatzle bean-thread
				       roots potatoes yam
				       others rice)))


(print (rember-upto-last 'cookies '(cookies
				    chocolate mints
				      caramel delight ginger snaps
				    desserts
				    chocolate mousse
				    vanilla ice cream
				    German chocolate cake
				    more cookies
				    gingerbreadman chocolate
				      chip brownies)))
