



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random

(define random (lambda(n) 

		 (exp2 n)
			  ))


;; random auxiliares

(define (random num)
  (nums_centro(exp2 num))
)


;; saca los numeros del centro
(define (nums_centro num)
  (let
    (
     (len (len_int num))
    )
  (cond
      ((impar? len) (shaveleft
		    (shaveright num (fx/ (+ 1 len) 4))
		    (- (fx/ (+ 1 len) 4) 1)))
      (#t (shaveleft (shaveright num (fx/ len 4)) (fx/ len 4)))
  )))


(define (shaveleft num delete) 	;le quita <delete> cantidad de numeros
  				;a la izquierda de <num>
  (
   shaveleft_aux num (- (len_int num) delete)
  ))

(define (shaveleft_aux num delete)
  (cond
    ((= 0 delete) 0)
    (#t (+ 
	  (* 
	    (shaveleft_aux (fx/ num 10)(- delete 1))
	    10)
	  (fxmod num 10)))
  ))

(define (shaveright num delete)	;le quita <delete> cantidad de numeros
  				;a la derecha de <num>
  (cond
	((= 0 delete) num)
	(#t (shaveright(fx/ num 10)(- delete 1)))
  ))


;funcion que no se usa, jejeps

(define (fixedlen num)
  (let ((len (len_int num)))
  (cond
	((impar? len)(+ 1 len))
	(#t len)
   )))



;; length de un int


(define (len_int num)(lenint_aux num 1))


(define lenint_aux(lambda(num size)
		    (cond
		    ((< num 10)1)
		    (#t (+ 1 (lenint_aux (fx/ num 10) size)))
		    )))

;; num impar
(define (impar? num)
  		(cond
		 ((>(fxmod num 2)0)#t)
		 (#t #f)
		 ))

;; exponencial de un numero^2

(define exp2 (lambda (n)
  (expt n 2)))



;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))











