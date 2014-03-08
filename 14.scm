(load "helpers")

(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
	  (else (leftmost (car l))))))

(print (leftmost '(((a) b) (c d))))
(print (leftmost '(((a) ()) () (e))))
;(print (leftmost '(((() a) ()))))

(set! leftmost
      (lambda (l)
	(cond ((null? l) '())
	      ((atom? (car l)) (car l))
	      (else (cond ((atom? (leftmost (car l)))
			     (leftmost (car l)))
			  (else (leftmost (cdr l))))))))
(define (leftmost-examples)
  (print (leftmost '(((b) c) (d e))))
  (print (leftmost '(((b) ()) () (f))))
  (print (leftmost '(((() b) ()))))) ; now prints `b`
(leftmost-examples)

; now using `let` to name the repeated computation
(set! leftmost
      (lambda (l)
	(cond ((null? l) '())
	      ((atom? (car l)) (car l))
	      (else (let ((a (leftmost (car l))))
		      (cond ((atom? a) a)
			    (else (leftmost (cdr l)))))))))

(leftmost-examples)

(define rember1*
  (lambda (a l)
    (letrec
	((R (lambda (l)
	      (cond ((null? l) '())
		    ((atom? (car l))
		       (cond ((eq? a (car l))
			        (cdr l))
			     (else (cons (car l)
					 (R (cdr l))))))
		    (else
		      (cond ((equal? (R (car l)) (car l))
			       (cons (car l)
				     (R (cdr l))))
			    (else (cons (R (car l))
					(cdr l)))))))))
      (R l))))

(define (rember1*-examples)
  (print (rember1* 'salad '((Swedish rye)
			    (French (mustard salad turkey))
			    salad)))
  (print (rember1* 'meat '((pasta meat)
			   pasta
			   (noodles meat sauce)
			   meat tomatoes))))
(rember1*-examples)

; Now using `let` again to remove the repeated computation
(set! rember1*
      (lambda (a l)
	(letrec
	    ((R (lambda (l)
		  (cond ((null? l) '())
			((atom? (car l))
			   (cond ((eq? a (car l))
				  (cdr l))
				 (else (cons (car l)
					     (R (cdr l))))))
			(else
			  (let ((av (R (car l))))
			    (cond ((equal? av (car l))
				   (cons (car l)
					 (R (cdr l))))
				  (else (cons av
					      (cdr l))))))))))
	  (R l))))

(rember1*-examples)


;; The Fifteenth Commandment
;;
;; Use `let` to name the values of repeated expressions


(define depth*
  (lambda (l)
    (cond ((null? l) 1)
	  ((atom? (car l))
	     (depth* (cdr l)))
	  ((> (depth* (cdr l))
	      (add1 (depth* (car l))))
	     (depth* (cdr l)))
	  (else (add1 (depth* (car l)))))))

(define depth*-examples
  (lambda ()
    (print (depth* '((pickled) peppers (peppers pickled))))
    (print (depth* '(margarine
		     ((bitter butter)
		      (makes)
		      (batter (bitter)))
		     butter)))
    (print (depth* '(c (b (a b) a) a)))))
(depth*-examples)

;; Can't use the following definition
;; It recurs before it has encountered any operations
;; on the values
;; (set! depth*
;;       (lambda (l)
;; 	(let ((a (add1 (depth* (car l))))
;; 	      (d (depth* (cdr l))))
;; 	  (cond ((null? l) 1)
;; 		((atom? (car l)) d)
;; 		((> d a) d)
;; 		(else a)))))

(set! depth*
      (lambda (l)
	(cond ((null? l) 1)
	      ((atom? (car l)) (depth* (cdr l)))
	      (else
	        (let ((a (add1 (depth* (car l))))
		      (d (depth* (cdr l))))
		  (cond ((> d a) d)
			(else a)))))))
(depth*-examples)

(set! depth*
      (lambda (l)
	(cond ((null? l) 1)
	      (else (let ((d (depth* (cdr l))))
		      (cond ((atom? (car l)) d)
			    (else (let ((a (add1 (depth* (car l)))))
				    (cond
				      ((> d a) d)
				      (else a))))))))))
(depth*-examples)


;; The Fifteenth Commandment
;;
;; Use `let` to name the values of repeated expressions
;; in a function definition if they may be evaluated twice
;; for one and the same use of the function.


(set! depth*
      (lambda (l)
	(cond ((null? l) 1)
	      ((atom? (car l))
	         (depth* (cdr l)))
	      (else (let ((a (add1 (depth* (car l))))
			  (d (depth* (cdr l))))
		      (if (> d a) d a))))))
(depth*-examples)

(define max
  (lambda (n m)
    (if (> n m) n m)))

(set! depth*
      (lambda (l)
	(cond ((null? l) 1)
	      ((atom? (car l))
	         (depth* (cdr l)))
	      (else (max (add1 (depth* (car l)))
			 (depth* (cdr l)))))))
(depth*-examples)

(define scramble
  (lambda (tup)
    (letrec
	((P (lambda (tup rev-pair)
	      (let ((rp (cons (car tup) rev-pair)))
		(cond ((null? tup) '())
		      (else (cons (pick (car tup) rp)
				  (P (cdr tup) rp))))))))
      (P tup '()))))


(set! leftmost
      (lambda (l)
	(call-with-current-continuation
	  (lambda (skip)
	    (lm l skip)))))

(define lm
  (lambda (l out)
    (cond ((null? l) '())
	  ((atom? (car l)) (out (car l)))
	  (else (begin
		  (lm (car l) out)
		  (lm (cdr l) out))))))

(leftmost-examples)

(set! leftmost
      (letrec
	  ((lm (lambda (l out)
		 (cond ((null? l) '())
		       ((atom? (car l))
			  (out (car l)))
		       (else (begin
			       (lm (car l) out)
			       (lm (cdr l) out)))))))
	(lambda (l)
	  (call-with-current-continuation
	     (lambda (skip)
	       (lm l skip))))))

(print '(leftmost with an inner call/cc))
(leftmost-examples)

(set! leftmost
      (lambda (l)
	(call-with-current-continuation
	  (lambda (skip)
	    (letrec
		((lm (lambda (l out)
		       (cond ((null? l) '())
			     ((atom? (car l)) (out (car l)))
			     (else (begin
				     (lm (car l) out)
				     (lm (cdr l) out)))))))
	      (lm l skip))))))

(print '(leftmost with an outer call/cc))
(leftmost-examples)

(set! leftmost
      (lambda (l)
	(call-with-current-continuation
	  (lambda (skip)
	    (letrec
		((lm (lambda (l)
		       (cond ((null? l) '())
			     ((atom? (car l)) (skip (car l)))
			     (else (begin
				     (lm (car l))
				     (lm (cdr l))))))))
	      (lm l))))))

(print '(leftmost without passing skip to lm))
(leftmost-examples)

(define rm
  (lambda (a l oh)
    (cond ((null? l) (oh 'no))
	  ((atom? (car l))
	     (if (eq? a (car l))
		 (cdr l)
		 (cons (car l)
		       (rm a (cdr l) oh))))
	  (else
	     (if (atom?
		    (call-with-current-continuation
		       (lambda (oh)
			 (rm a (car l) oh))))
		 (cons (car l)
		       (rm a (cdr l) oh))
		 (cons (rm a (car l) 0)
		       (cdr l)))))))

(set! rember1*
      (lambda (a l)
	(if (atom? (call-with-current-continuation
		     (lambda (oh)
		       (rm a l oh))))
	    l
	    (rm a l '()))))
(rember1*-examples)

(set! rember1*
      (lambda (a l)
	(let ((new-l (call-with-current-continuation
		       (lambda (oh)
			 (rm a l oh)))))
	  (if (atom? new-l)
	      l
	      new-l))))
(rember1*-examples)


(set! rm
      (lambda (a l oh)
	(cond ((null? l) (oh 'no))
	      ((atom? (car l))
	         (if (eq? a (car l))
		     (cdr l)
		     (cons (car l)
			   (rm a (cdr l) oh))))
	      (else
	         (let ((new-car
			 (call-with-current-continuation
			    (lambda (oh)
			      (rm a (car l) oh)))))
		   (if (atom? new-car)
		       (cons (car l)
			     (rm a (cdr l) oh))
		       (cons new-car (cdr l))))))))
(rember1*-examples)

;; `try` is in helpers, created using `define-syntax`
(set! rm
      (lambda (a l oh)
	(cond ((null? l) (oh 'no))
	      ((atom? (car l))
	         (if (eq? a (car l))
		     (cdr l)
		     (cons (car l)
			   (rm a (cdr l) oh))))
	      (else
	         (try oh2
		      (cons (rm a (car l) oh2)
			    (cdr l))
		      (cons (car l)
			    (rm a (cdr l) oh)))))))
(rember1*-examples)
