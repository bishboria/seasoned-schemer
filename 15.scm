(load "helpers")

(define x (cons 'chicago (cons 'pizza ())))
(print x)

(set! x 'gone)
(print x)

(set! x 'skins)
(print x)

(define gourment
  (lambda (food)
    (cons food (cons x ()))))

(print (gourment 'onion))
(print (cons x ()))

(set! x 'rings)
(print (gourment 'onion))

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food (cons x ()))))

(print (gourmand 'potato))
(print x)
(print (gourmand 'rice))
(print x)

(define diner
  (lambda (food)
    (cons 'milkshake
	  (cons food
		()))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
	  (cons food
		()))))

(print (dinerR 'onion))
(print x)
(print (dinerR  'pecanpie))
(print x)

(print (gourmand 'onion))
(print x)

(define omnivore
  (let ((x 'ministrone))
    (lambda (food)
      (set! x food)
      (cons food
	    (cons x
		  ())))))

(print (omnivore 'beef))
(print x) ; still onion, not beef this time


;; The Sixteenth Commandment
;;
;; Use `set!` only with names defined in `let`s


(define gobbler
  (let ((x 'ministrone))
    (lambda (food)
      (set! x food)
      (cons food
	    (cons x
		  ())))))

(print (gobbler 'pork))

(define nibbler
  (lambda (food)
    (let ((x 'donut))
      (set! x food)
      (cons food
	    (cons x
		  ())))))

(print (nibbler 'cheerio))
(print x)


;; The Seventeenth Commandment
;;
;; (preliminary version)
;;
;; Use `(set! x ...)` for `(let ((x ...)) ...)` only if
;; there is at least one `(lambda ...)` between it and
;; the `(let ((x ...)) ...)`.


(define food 'none)

(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
	  (cons x
		(cons 'more
		      (cons x
			    ()))))))

(print (glutton 'onion))
(print food)
(print (glutton 'garlic))
(print food)

(define chez-nous
  (lambda ()
    (let ((swap x))
      (set! x food)
      (set! food swap))))

(chez-nous)

(print x)
(print food)

;; The Eighteenth Commandment
;;
;; Use `(set! x ...)` only when the value that x
;; refers to is no longer needed.

(print (glutton 'garlic))
(print food)
(print (gourmand 'potato))
(print x)

(chez-nous)
(print food)
(print x)
