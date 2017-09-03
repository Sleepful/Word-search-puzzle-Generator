;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main loops


;;;;;;;; sopa de letras silvestre






;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	general



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	soup letter
(define(func)#t)


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
			((word_inserter position word) word_list_zealot) 
			;word inserter, if fails then backtrack
			(#t(#t randstep))
		)
	)
)


(define(word_inserter position word wlist)
; recieves random, from there it tries to insert
; a word by iterating over the whole list
	  (
		word_inserter_aux(positionfinal position word wlist )
	  )
  #t)

(define(word_inserter position word wlist direction wlist)
; inserts a word into the matrix position at the direction directed
	(cond
	  	((can_insert_word? position word wlist direction) insert_word word direction position)
		(#t #f)

	)
#t)


; checks whether a word fits in the puzzle or not
(define (can_insert_word? word direction position wlist)
	(cond
	((equal? word '()) #t)
	((eq? (position_available position wlist) #f) #f)
	((or 
		(eq? (get_symbol position wlist) '?)
		(eq? (get_symbol position wlist) (car word)))
			(can_insert_word? 
				(cdr word) direction (next_position direction position) wlist))
	(#t #t)
	))
; is word empty, return tru (we confirmed all word letters)
; is current position out of range, return fals (word dont fit)
; is current position a ? char, continue next letter
; is current position the same char as the cuurrent char in the word, 
;	continue next letter
; else, return fals (position already has another word that doesnt match)


; inserts a word into the matrix, it handles position and direction
; check if word can be inserted before calling this function (can_insert_word?)
(define(insert_word word direction position wlist) 
	(cond
		((equal? word '()) wlist)
		(#t (insert_word 
				(cdr word)
				direction
				(next_position direction position)
				(set_symbol position wlist (car word))))
	))
; checks if word is empty, it's finished
; inserts car word into current position
; repeats with cdr word

; computes next (x y) position based on an (x y) position and a direction
(define(next_position direction position)
	(cond
		((eq? direction 's)
			(cons (car position)(cons (+ (cadr position) 1)'())))
		((eq? direction 'n)
			(cons (car position)(cons (- (cadr position) 1)'())))
		((eq? direction 'w)
			(cons (- (car position) 1)(cdr position)))
		((eq? direction 'e)
			(cons (+ (car position) 1)(cdr position)))
        ((eq? direction 'sw)
			(cons (- (car position) 1)(cons (+ (cadr position) 1)'())))
		((eq? direction 'ne)
			(cons (+ (car position) 1)(cons (- (cadr position) 1)'())))
		((eq? direction 'se)
			(cons (+ (car position) 1)(cons (+ (cadr position) 1)'())))
		((eq? direction 'nw)
			(cons (- (car position) 1)(cons (- (cadr position) 1)'())))
		))

; checks whether the position is inside or outside of
; the matrix and returns #t or #f accordingly
(define(position_available position wlist)
	(and
		(< (car position) (len_list wlist)) 
		(< (cadr position) (len_list(car wlist))) 
		(< -1(car position))
		(< -1(cadr position))))

(define(fill_matrix_with_letters)#t)

(define (direction)'(n s e w nw ne sw se))

(define (abecedario)'(a b c d e f g h i j k l r m n o p q r s t u v w x y z))

(define(random_dir) #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	general matrix

(define (get_symbol position wlist)
; gets symbol in a matrix at position (x y)
	(cond
		; itera sobre x
		((= (car position) 0)
	  		(cond
			  ; condicion terminal
			  ((= (len_list position) 1) (car wlist))
			  ; solo llega aqui una vez, cuando encuentra el x
			  (#t (get_symbol (cdr position)(car wlist)))))
	   (#t (get_symbol (cons (- (car position) 1)(cdr position)) (cdr wlist)))
	))

(define (set_symbol position wlist symbol)
; sets symbol in a matrix at position (x y)
; returns back the new list
	(cond
		; itera sobre x
	  	((= (car position) 0)
	  		(cond
			  ; condicion terminal
			  ((= (len_list position) 1) (cons symbol (cdr wlist)))
			  ; solo llega aqui una vez, cuando encuentra el x
			  (#t (cons
				(set_symbol (cdr position)(car wlist) symbol)
				(cdr wlist)))))
	   (#t (cons
	   			(car wlist)
				(set_symbol (cons (- (car position) 1)(cdr position)) (cdr wlist) symbol)))
	))


(define(rand_list randnum wlist)
; devuelve un elem random de un list
 (n-elem(fxmod randnum (len_list wlist)) wlist)
)

(define(gen_xy randnum wlist)
; apartir de un random, genera una posicion
; (x y) basado en el tamanho de la matriz
; wlist es una lista hecha de listas, se asume que
; representa una matriz cuadrada ya que solo usa
; el tamano de la primera lista para determinar el tamanho de la matriz
	(cons
	  	
		(fxmod randnum (len_list wlist))
		(cons (fxmod (fx/ randnum 10) (len_list (car wlist)))'())
		
	)
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



(define (genFila n)
;; genera listas llenas de ?
;; se llama genFILA pero funciona para columnas tambien
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
;;	extra
;;	general

(define (len_list l)
	(cond
		((eq? l '())0) 
		(#t (+ 1 (len_list (cdr l))))
	)
)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))


(define (decimals n)
	(cond
		((< n 10)10)
		(#t (* 10 (decimals (fxmod n 10))))

	  )
  )

(define (n-elem n l)
  (cond
    ((= n 0)(car l))
	(#t (n-elem (- n 1)(cdr l)))
   )
)





