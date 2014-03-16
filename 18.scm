(load "helpers")

(define lots
  (lambda (m)
    (cond ((zero? m) '())
	  (else (kons 'egg
		      (lots (sub1 m)))))))

(define lenkth
  (lambda (l)
    (cond ((null? l) 0)
	  (else (add1 (lenkth (kdr l)))))))

(define add-at-end
  (lambda (l)
    (cond ((null? (kdr l))
	   (konsC (kar l)
		  (kons 'egg '())))
	  (else (konsC (kar l)
		       (add-at-end (kdr l)))))))

(define add-at-end-too
  (lambda (l)
    (letrec ((A (lambda (ls)
		  (cond ((null? (kdr ls))
			   (set-kdr ls (kons 'egg '())))
			(else (A (kdr ls)))))))
      (A l)
      l)))

(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))

(define kar
  (lambda (c)
    (c (lambda (a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (a d) d))))


(define kounter)
(define set-kounter)
(define konsC
  (let ((N 0))
    (set! kounter
	  (lambda ()
	    N))
    (set! set-kounter
	  (lambda (x)
	    (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (kons x y))))

(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
	(selector (lambda (x) (set! kdr x))
		  kar
		  kdr)))))

(set! kar
      (lambda (c)
	(c (lambda (s a d) a))))

(set! kdr
      (lambda (c)
	(c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(set! kons
      (lambda (a d)
	(let ((c (bons a)))
	  (set-kdr c d)
	  c)))

(define dozen (lots 12))

(define bakers-dozen (add-at-end dozen))

(define bakers-dozen-too (add-at-end-too dozen))

(define bakers-dozen-again (add-at-end dozen))

(define eklist?
  (lambda (ls1 ls2)
    (cond ((null? ls1) (null? ls2))
	  ((null? ls2) #f)
	  (else
	    (and (eq? (kar ls1) (kar ls2))
		 (eklist? (kdr ls1) (kdr ls2)))))))

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
	  (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
	(set-kdr c1 t1)
	(set-kdr c2 t2)
	v))))

(define last-kons
  (lambda (ls)
    (cond ((null? (kdr ls))
	     ls)
	  (else (last-kons (kdr ls))))))

(define long (lots 12))

(define finite-lenkth
  (lambda (p)
    (letcc infinite
	   (letrec
	       ((C (lambda (p q)
		     (cond ((null? q) 0)
			   ((null? (kdr q)) 1)
			   ((same? p q)
			      (infinite #f))
			   (else (+ (C (sl p) (qk q))
				    2)))))
		(qk (lambda (x) (kdr (kdr x))))
		(sl (lambda (x) (kdr (x)))))
	     (cond ((null? p) 0)
		   (else (add1 (C p (kdr p)))))))))

(print (lenkth (lots 3)))
(print (lenkth (lots 5)))
(print (lenkth (lots 15)))

(print (add-at-end (lots 3)))

(print (kounter))

(set-kounter 0)
(print (kounter))
(print (add-at-end-too (lots 3)))

(print (eklist? bakers-dozen bakers-dozen-too))

(print (same? bakers-dozen bakers-dozen-too))

(print (same? (kons 'egg '())
	      (kons 'egg '())))

(set-kdr (last-kons long) long) ; creates an infinite list
				; even though there are only
				; 12 `kons`es

(print (finite-lenkth (kons 'a (kons 'b '()))))
