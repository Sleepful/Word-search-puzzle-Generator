;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main loops


;;;;;;;; sopa de letras silvestre








;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	general



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	soup letter


(define(word_list_zealot wlist randstep mlist)
; recieves word list
; goes through it linearly
; tries to insert word
; if succesful repeat with rest of list
; return #t when complete
; if fail return #f and backtrack
; return #f if all recursion fails
; reuse random (returns it too)
	(let*
		((randstep 
		   	(random randstep))
			(position(gen_xy randstep wlist)))
		(cond
			((word_inserter position word)word_list_zealot)
			(#t(#t randstep))
		)
	)
)


(define(word_inserter position word) #t)

(define(insert_word word direction position) #t)

(define(can_insert_word? word direction position) #t)

(define(fill_matrix_with_letters)#t)

(define (direction)'(n s e w nw ne sw se))

(define (abecedario)'(a b c d e f g h i j k l r m n o p q r s t u v w x y z))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	general matrix

(define(gen_xy randnum wlist)
; apartir de un random, genera una posicion
; (x y) basado en el tamanho de la matriz
)

(define(genMatrix columnas filas)	   
;genera matrix llena de ?. (x,y) de la matriz
;columnas y filas representan las dimensiones
	(genMatrix_aux(genFila columnas)filas) 
					
)

(define(genMatrix_aux lista filas)
	(cond
		((eq? lista '()) '())
		(#t (cons (genFila filas) (genMatrix_aux (cdr lista) filas)))
	)
)


;; genera listas llenas de ?
;; se llama genFILA pero funciona para columnas tambien

(define (genFila n)
	(genFila_aux n '())
)

(define (genFila_aux n lista)
	(cond((= 0 n) '()) 
	        (#t (cons '? (genFila_aux (- n 1) lista)))
	)
)





;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	random
;;


(define random (lambda(n) 
;; esta funcion no se usa
		 (exp2 n))
)


(define (random num)
;; funcion random, llamarla con su mismo resultado
;; para generar secuencia pseudo random
;; nota: no funciona con numeros menores a 10
  (nums_centro(exp2 num))
)


(define (nums_centro num)
;; saca los numeros del centro
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


(define (shaveleft num delete) 	
;le quita <delete> cantidad de numeros
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

(define (shaveright num delete)	
;le quita <delete> cantidad de numeros
;a la derecha de <num>
  (cond
	((= 0 delete) num)
	(#t (shaveright(fx/ num 10)(- delete 1)))
  ))


(define (fixedlen num)
;funcion que no se usa, jejeps
  (let ((len (len_int num)))
  (cond
	((impar? len)(+ 1 len))
	(#t len)
   )))


(define (len_int num)
;length de un int
(lenint_aux num 1))

(define lenint_aux(lambda(num size)
		    (cond
		    ((< num 10)1)
		    (#t (+ 1 (lenint_aux (fx/ num 10) size)))
		    )))

(define (impar? num)
;num impar
  		(cond
		 ((>(fxmod num 2)0)#t)
		 (#t #f)
		 ))

(define exp2 (lambda (n)
;exponencial de un numero^2
  (expt n 2)))



;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))











