#lang racket


;; FUNCION PRINCIPAL;;; 
;  Funcion principal del programa para inicializar, dar dimension y mover el cubo

(define (RS X Cubo Movs)
  (cond
    [(or (null? Cubo) (null? Movs)) (Mensaje_Error_1)]
    [(not (number? X)) (Mensaje_Error_2)]
    [(or (< X 2) (> X 6)) (Mensaje_Error_2)]
    [(not (cubo-Valido? X Cubo)) (Mensaje_Error_1)] ;funcion que verifica que la matriz sea de las dimensiones correctas segun el parametro x, si tiene dimensiones incorrectas devuelve true
    [else (leer_movimientos X Cubo Movs '())]))

;; FIN FUNCION PRINCIPAL;;

;;; FUNCIONES DE VALIDACIÓN DE PARAMETROS ;;;;;;

;Funcion encargada de revisar si la matriz Cubo está bien definida con respecto a las dimensiones y los valores de cada sublista
(define (cubo-Valido? X Cubo)
  ; Función auxiliar para revisar las caras
  (define (revisar-caras caras)
    (cond
      [(null? caras) #t] ; Ya revisamos todo, no hubo errores
      [(not (list? (car caras))) #f] ; La cara no es una lista
      [(not (= (length (car caras)) (* X X))) #f] ; Dimensiones incorrectas
      [(not (colores-Valido? (car caras))) #f] ; Alguno de los colores es incorrecto (no es un número o no va del 1 al 6)
      [else (revisar-caras (cdr caras))])) ; Se sigue revisando

  ; Función para verificar que cada número aparece la cantidad correcta de veces 
  (define (frecuencias-correctas? X cubo)
    (define (contar num lista)
      (cond
        [(null? lista) 0]
        [(= (car lista) num) (+ 1 (contar num (cdr lista)))]
        [else (contar num (cdr lista))]))

    (define (verificar n total todos)
      (cond
        [(> n 6) #t]
        [(not (= (contar n todos) total)) #f]
        [else (verificar (+ n 1) total todos)]))

    (verificar 1 (* X X) (aplanar cubo)))

  ; Verificar que el cubo tenga 6 caras, cada cara sea válida y las frecuencias sean correctas
  (if (not (= (length Cubo) 6))
      #f ; No tiene 6 caras
      (and
        (revisar-caras Cubo)          ; Validar cada cara
        (frecuencias-correctas? X Cubo)))) ; Validar las frecuencias
       
; Función para aplanar el cubo en una lista
(define (aplanar cubo)
  (if (null? cubo)
      '()
      (append (car cubo) (aplanar (cdr cubo)))))
      
; Función para validar los colores de una cara
(define (colores-Valido? lista)
  (cond
    [(null? lista) #t] ; Se revisó toda la cara y todos los colores están representados por números del 1 al 6
    [(not (number? (car lista))) #f]  
    [(or (< (car lista) 1) (> (car lista) 6)) #f]
    [else (colores-Valido? (cdr lista))])) ; Continuar revisando

;;; FIN FUNCIONES DE VALIDACIÓN DE PARAMETROS ;;;;;;

;;;; MENSAJES DE ERROR ;;;; 
;Mensaje de error por si la lista con los movimientos esta vacía o si el cubo no se inicializó bien con la matriz
(define (Mensaje_Error_1)
  (error "Error, no inicializaste de manera correcta el cubo"))

;Mensaje de error por si las dimensiones del cubo son incorrectas, menores a 2x2 o menores a 6x6 
(define (Mensaje_Error_2)
  (error "Error, las dimensiones para inicializar el cubo pueden ser de 2x2 hasta 6x6"))
;; Mensaje de error por si la lista de movimientos posee algún movimiento inválido
(define (Mensaje_Error_3)
  (error "Error en la lista de movimientos, se encontró un movimiento no válido"))

;;;; FIN MENSAJES DE ERROR ;;;;

;;;; FUNCIONES PARA LAS INSTRUCCIONES DE MOVIMIENTO DEL CUBO ;;;;

;Funcion principal para leer los movimientos que se le quieren dar al cubo
(define (leer_movimientos dimension cubo simbolos lista_movimientos)
  (cond
    [(null? simbolos) (leer_movimientos_aux dimension cubo lista_movimientos '())]
    [else 
     (leer_movimientos
      dimension
      cubo
      (cdr simbolos)
      (append lista_movimientos (list (symbol->string (car simbolos)))))])) ;; se hace una llamada recursiva para ir creando la lista con los string de cada movimiento (conversion simbolo a string)


;; Función auxiliar para crear una nueva lista con los movimientos explicitos el entero que sugiere cual fila o columna se debe mover y el movimiento de esta si es
;; izquierda-derecha o arriba-abajo
(define (leer_movimientos_aux dimension cubo mov_desordenados mov_ordenados)
  (cond
    [(null? mov_desordenados)(elegir_movimiento dimension cubo mov_ordenados)]
    [else
     (leer_movimientos_aux
      dimension
      cubo
      (cdr mov_desordenados)
      (append
       mov_ordenados
       (list
        (list
         (cond [(string=? (substring (car mov_desordenados) 0 1) "F") "fila"]
               [(string=? (substring (car mov_desordenados) 0 1) "C") "columna"]
               [else (Mensaje_Error_3)])
         (string->number (substring (car mov_desordenados) 1 2)) ;; conversión para el segundo valor de cada movimiento que es un número (indica cual columna o fila se mueve)
         (cond [(string=? (substring (car mov_desordenados) 2 3) "D") "derecha"]
               [(string=? (substring (car mov_desordenados) 2 3) "I") "izquierda"]
               [(string=? (substring (car mov_desordenados) 2 3) "A") "arriba"]
               [(string=? (substring (car mov_desordenados) 2 3) "B") "abajo"]
               [else (Mensaje_Error_3)])))))])) ;; llamada recursiva agregando a la lista vacía una lista con movimientos explicitos a partir de lista mov_desordenados

;;Función para llamar funciones de movimientos según la lista mov_ordenados (Lista de listas con movimientos explícitos)
(define (elegir_movimiento dimension cubo movs)
  (cond
    [(null? movs) cubo]
    [(and (string=? (caar movs) "fila") (string=? (caddr(car movs))"derecha"))(mov_fila_derecha dimension cubo (cadar movs) movs)]
    [(and (string=? (caar movs) "fila") (string=? (caddr(car movs))"izquierda"))(mov_fila_izquierda dimension cubo (cadar movs) movs)]
    [(and (string=? (caar movs) "columna") (string=? (caddr(car movs))"arriba"))(mov_columna_arriba dimension cubo (cadar movs) movs)]
    [(and (string=? (caar movs) "columna") (string=? (caddr(car movs))"abajo"))(mov_columna_abajo dimension cubo (cadar movs) movs)]
    [else (Mensaje_Error_3)]))


(define (mov_fila_derecha dimension cubo n movimientos)
  (display cubo)
  (newline)
  (display "Realizar movimiento fila: ")
  (display n)
  (display " a la derecha")
  (newline)
  (elegir_movimiento dimension (mov_derecha dimension cubo cubo n '() '() 1 0) (cdr movimientos)))

(define (mov_fila_izquierda dimension cubo n movimientos)
  (display cubo)
  (newline)
  (display "Realizar movimiento fila: ")
  (display n)
  (display " a la izquierda")
  (newline)
  (elegir_movimiento dimension (mov_izquierda dimension cubo cubo n '() '() 1 0) (cdr movimientos)))

(define (mov_columna_arriba dimension cubo n movimientos)
  (display cubo)
  (newline)
  (display "Realizar movimiento columna: ")
  (display n)
  (display " hacia arriba")
  (newline)
  (elegir_movimiento dimension (mov_arriba dimension cubo cubo n '() '() 1 0) (cdr movimientos)))

(define (mov_columna_abajo dimension cubo n movimientos)
  (display cubo)
  (newline)
  (display "Realizar movimiento columna: ")
  (display n)
  (display " hacia abajo")
  (newline)
  (elegir_movimiento dimension (mov_abajo dimension cubo cubo n '() '() 1 0) (cdr movimientos)))

;;;; FIN FUNCIONSE PARA LAS INSTRUCCIONES DE MOVIMIENTO DEL CUBO ;;;;

;;; FUNCIONES LOGICA MOVIMIENTOS ;;;
;;FUNCION LOGICA MOVIMIENTO FILA A LA DERECHA;;
(define (mov_derecha dimension cuboOG cubo n result lista cara cuadro)
  (cond((> cara 6) lista)
  (else(cond((= cara 1)(cond((>= cuadro (* dimension n)) (mov_derecha dimension cuboOG cuboOG n '() (append lista (list(append result (car cubo)))) (+ cara 1) 0))
                           ((< cuadro (* dimension (- n 1)))(mov_derecha dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caar cubo))) lista cara (+ cuadro 1)))
                           (else (mov_derecha dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (caddr cubo) dimension n (+ (modulo cuadro dimension) 1)))) lista cara (+ cuadro 1)))   ))
            
         ((= cara 2)(cond((>= cuadro (* dimension n)) (mov_derecha dimension cuboOG cuboOG n '() (append lista (list(append result (cadr cubo)))) (+ cara 1) 0))
                           ((< cuadro (* dimension (- n 1)))(mov_derecha dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caadr cubo))) lista cara (+ cuadro 1)))
                           (else (mov_derecha dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (car cubo) dimension n (+ (modulo cuadro dimension) 1)))) lista cara (+ cuadro 1)))   ))

         ((= cara 3)(cond((>= cuadro (* dimension n)) (mov_derecha dimension cuboOG cuboOG n '() (append lista (list(append result (caddr cubo)))) (+ cara 1) 0))
                           ((< cuadro (* dimension (- n 1)))(mov_derecha dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caaddr cubo))) lista cara (+ cuadro 1)))
                           (else (mov_derecha dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (espejo(cadr(cddddr cubo)) dimension) dimension n (+ (modulo cuadro dimension) 1)))) lista cara (+ cuadro 1)))   ))
         
         ((= cara 4) (cond ((= n 1) (mov_derecha dimension cuboOG cuboOG n '() (append lista (append result (list(rotar-antihoraria (cadddr cubo) dimension)))) (+ cara 1) 0) )
                           (else (mov_derecha dimension cuboOG cuboOG n '() (append lista (append result (list(cadddr cubo)))) (+ cara 1) 0) )))
                     
         ((= cara 5) (cond ((= n dimension) (mov_derecha dimension cuboOG cuboOG n '() (append lista (append result (list(rotar-horaria (car(cddddr cubo)) dimension)))) (+ cara 1) 0) )
                           (else (mov_derecha dimension cuboOG cuboOG n '() (append lista (append result (list(car(cddddr cubo))))) (+ cara 1) 0) )))
         
         ((= cara 6)(cond((>= cuadro (* dimension n)) (mov_derecha dimension cuboOG cuboOG n '() (append lista (list(append result (caddr(cdddr cubo))))) (+ cara 1) 0))
                           ((< cuadro (* dimension (- n 1)))(mov_derecha dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caadr(cddddr cubo)))) lista cara (+ cuadro 1)))
                           (else (mov_derecha dimension cuboOG (actCubo cubo cara 0 1 '()) n (append result (list(obtener-elemento (cadr cubo) dimension n (+ (modulo cuadro dimension) 1)))) lista cara (+ cuadro 1)))   ))
         ))     
  ))

;;FUNCION LOGICA MOVIMIENTO FILA A LA IZQUIERDA;;
(define (mov_izquierda dimension cuboOG cubo n result lista cara cuadro)
  (cond((> cara 6) lista)
  (else(cond((= cara 1)(cond((>= cuadro (* dimension n)) (mov_izquierda dimension cuboOG cuboOG n '() (append lista (list(append result (car cubo)))) (+ cara 1) 0))
                           ((< cuadro (* dimension (- n 1)))(mov_izquierda dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caar cubo))) lista cara (+ cuadro 1)))
                           (else (mov_izquierda dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (cadr cubo) dimension n (+ (modulo cuadro dimension) 1)))) lista cara (+ cuadro 1)))   ))
            
         ((= cara 2)(cond((>= cuadro (* dimension n)) (mov_izquierda dimension cuboOG cuboOG n '() (append lista (list(append result (cadr cubo)))) (+ cara 1) 0))
                           ((< cuadro (* dimension (- n 1)))(mov_izquierda dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caadr cubo))) lista cara (+ cuadro 1)))
                           (else (mov_izquierda dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (espejo(cadr(cddddr cubo)) dimension) dimension n (+ (modulo cuadro dimension) 1)))) lista cara (+ cuadro 1)))   ))

         ((= cara 3)(cond((>= cuadro (* dimension n)) (mov_izquierda dimension cuboOG cuboOG n '() (append lista (list(append result (caddr cubo)))) (+ cara 1) 0))
                           ((< cuadro (* dimension (- n 1)))(mov_izquierda dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caaddr cubo))) lista cara (+ cuadro 1)))
                           (else (mov_izquierda dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (car cubo) dimension n (+ (modulo cuadro dimension) 1)))) lista cara (+ cuadro 1)))   ))
         
         ((= cara 4) (cond ((= n 1) (mov_izquierda dimension cuboOG cuboOG n '() (append lista (append result (list(rotar-horaria (cadddr cubo) dimension)))) (+ cara 1) 0) )
                           (else (mov_izquierda dimension cuboOG cuboOG n '() (append lista (append result (list(cadddr cubo)))) (+ cara 1) 0) )))
                     
         ((= cara 5) (cond ((= n dimension) (mov_izquierda dimension cuboOG cuboOG n '() (append lista (append result (list(rotar-antihoraria (car(cddddr cubo)) dimension)))) (+ cara 1) 0) )
                           (else (mov_izquierda dimension cuboOG cuboOG n '() (append lista (append result (list(car(cddddr cubo))))) (+ cara 1) 0) )))
         
         ((= cara 6)(cond((>= cuadro (* dimension n)) (mov_izquierda dimension cuboOG cuboOG n '() (append lista (list(append result (caddr(cdddr cubo))))) (+ cara 1) 0))
                           ((< cuadro (* dimension (- n 1)))(mov_izquierda dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caadr(cddddr cubo)))) lista cara (+ cuadro 1)))
                           (else (mov_izquierda dimension cuboOG (actCubo cubo cara 0 1 '()) n (append result (list(obtener-elemento (espejo (caddr cubo) dimension) dimension n (+ (modulo cuadro dimension) 1)))) lista cara (+ cuadro 1)))   ))
         ))
  ))

;;FUNCION LOGICA MOVIMIENTO COLUMNA HACIA ABAJO;;
(define (mov_abajo dimension cuboOG cubo n result lista cara cuadro)
  (cond((> cara 6) lista)
  (else(cond((= cara 1)(cond((>= cuadro (* dimension dimension)) (mov_abajo dimension cuboOG cuboOG n '() (append lista (list result)) (+ cara 1) 0))
                           ((not(= (modulo cuadro dimension) (- n 1)))(mov_abajo dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caar cubo))) lista cara (+ cuadro 1)))
                           (else (mov_abajo dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (cadddr cubo) dimension (+ (quotient cuadro dimension) 1) n ))) lista cara (+ cuadro 1)))   ))
            
         ((= cara 2) (cond ((= n dimension) (mov_abajo dimension cuboOG cuboOG n '() (append lista (append result (list(rotar-antihoraria (cadr cubo) dimension)))) (+ cara 1) 0) )
                           (else(mov_abajo dimension cuboOG cuboOG n '() (append lista (append result (list(cadr cubo))) ) (+ cara 1) 0) )))

         ((= cara 3) (cond ((= n 1) (mov_abajo dimension cuboOG cuboOG n '() (append lista (append result (list(rotar-horaria (caddr cubo) dimension)))) (+ cara 1) 0) )
                           (else (mov_abajo dimension cuboOG cuboOG n '() (append lista (append result (list(caddr cubo)))) (+ cara 1) 0) ) ))
         
         ((= cara 4) (cond((>= cuadro (* dimension dimension)) (mov_abajo dimension cuboOG cuboOG n '() (append lista (list result)) (+ cara 1) 0))
                           ((not(= (modulo cuadro dimension) (- n 1)))(mov_abajo dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(car(cadddr cubo)))) lista cara (+ cuadro 1)))
                           (else (mov_abajo dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (espejo-vertical (cadr(cddddr cubo)) dimension) dimension (+ (quotient cuadro dimension) 1) n ))) lista cara (+ cuadro 1)))   ))

         ((= cara 5) (cond((>= cuadro (* dimension dimension)) (mov_abajo dimension cuboOG cuboOG n '() (append lista (list result)) (+ cara 1) 0))
                           ((not(= (modulo cuadro dimension) (- n 1)))(mov_abajo dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caar(cddddr cubo)))) lista cara (+ cuadro 1)))
                           (else (mov_abajo dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (car cubo) dimension (+ (quotient cuadro dimension) 1) n ))) lista cara (+ cuadro 1)))   ))
         
         ((= cara 6)(cond((>= cuadro (* dimension dimension)) (mov_abajo dimension cuboOG cuboOG n '() (append lista (list result)) (+ cara 1) 0))
                           ((not(= (modulo cuadro dimension) (- n 1)))(mov_abajo dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caadr(cddddr cubo)))) lista cara (+ cuadro 1)))
                           (else (mov_abajo dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (car(cddddr cubo)) dimension (+ (quotient cuadro dimension) 1) n ))) lista cara (+ cuadro 1)))   ))
         ))
  ))

;;FUNCION LOGICA MOVIMIENTO COLUMNA HACIA ARRIBA;;
(define (mov_arriba dimension cuboOG cubo n result lista cara cuadro)
  (cond((> cara 6) lista)
  (else(cond((= cara 1)(cond((>= cuadro (* dimension dimension)) (mov_arriba dimension cuboOG cuboOG n '() (append lista (list result)) (+ cara 1) 0))
                           ((not(= (modulo cuadro dimension) (- n 1)))(mov_arriba dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caar cubo))) lista cara (+ cuadro 1)))
                           (else (mov_arriba dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (car(cddddr cubo)) dimension (+ (quotient cuadro dimension) 1) n ))) lista cara (+ cuadro 1)))   ))
            
         ((= cara 2) (cond ((= n dimension) (mov_arriba dimension cuboOG cuboOG n '() (append lista (append result (list(rotar-horaria (cadr cubo) dimension)))) (+ cara 1) 0) )
                           (else(mov_arriba dimension cuboOG cuboOG n '() (append lista (append result (list(cadr cubo))) ) (+ cara 1) 0) )))

         ((= cara 3) (cond ((= n 1) (mov_arriba dimension cuboOG cuboOG n '() (append lista (append result (list(rotar-antihoraria (caddr cubo) dimension)))) (+ cara 1) 0) )
                           (else (mov_arriba dimension cuboOG cuboOG n '() (append lista (append result (list(caddr cubo)))) (+ cara 1) 0) ) ))
         
         ((= cara 4) (cond((>= cuadro (* dimension dimension)) (mov_arriba dimension cuboOG cuboOG n '() (append lista (list result)) (+ cara 1) 0))
                           ((not(= (modulo cuadro dimension) (- n 1)))(mov_arriba dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(car(cadddr cubo)))) lista cara (+ cuadro 1)))
                           (else (mov_arriba dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (car cubo) dimension (+ (quotient cuadro dimension) 1) n ))) lista cara (+ cuadro 1)))   ))

         ((= cara 5) (cond((>= cuadro (* dimension dimension)) (mov_arriba dimension cuboOG cuboOG n '() (append lista (list result)) (+ cara 1) 0))
                           ((not(= (modulo cuadro dimension) (- n 1)))(mov_arriba dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caar(cddddr cubo)))) lista cara (+ cuadro 1)))
                           (else (mov_arriba dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (espejo-vertical (espejo (cadr(cddddr cubo)) dimension) dimension) dimension (+ (quotient cuadro dimension) 1) n ))) lista cara (+ cuadro 1)))   ))
         
         ((= cara 6)(cond((>= cuadro (* dimension dimension)) (mov_arriba dimension cuboOG cuboOG n '() (append lista (list result)) (+ cara 1) 0))
                           ((not(= (modulo cuadro dimension) (- n 1)))(mov_arriba dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(caadr(cddddr cubo)))) lista cara (+ cuadro 1)))
                           (else (mov_arriba dimension cuboOG (actCubo cubo cara 0 1  '()) n (append result (list(obtener-elemento (cadddr cubo) dimension (+ (quotient cuadro dimension) 1) n ))) lista cara (+ cuadro 1)))   ))
         ))
  ))

;;FUNCION PARA ACTUALIZAR EL CUBO;;
(define (actCubo cubo cara cont contCara result)
  (cond((null? cubo)result)
  ((and (= cont 0) (= cara contCara)) (actCubo (cdr cubo) cara cont (+ contCara 1)  (append result (list(eliminar 0 (car cubo) 0)))) ) 
  (else(actCubo (cdr cubo) cara cont (+ contCara 1) (append result (list(car cubo)))))
 ))

;;;FIN FUNCIONES LOGICA MOVIMIENTOS ;;;


;;; FUNCIONES PARA MODIFICAR LISTAS ;;;
;;FUNCION PARA ELIMINAR ELEMENTO EN POSICION X DE UNA LISTA;;
(define (eliminar elem lista cont)
  (cond
    [(null? lista) '()]  
    [(equal? cont elem) (eliminar elem (cdr lista) (+ cont 1))] 
    [else (cons (car lista) (eliminar elem (cdr lista) (+ cont 1)))]))

;;FUNCION ROTACION ANTI-HORARIA;;
(define (rotar-antihoraria lista dimension)
  (rotar-antihoraria-aux 0 dimension lista))

(define (rotar-antihoraria-aux col dimension lista)
  (cond
    [(= col dimension) '()]
    [else
     (append (obtener-columna lista dimension (- dimension col 1) 0)
             (rotar-antihoraria-aux (+ col 1) dimension lista))]))

;;FUNCION ROTACION HORARIA;;
(define (rotar-horaria lista dimension)
  (rotar-horaria-aux 0 dimension lista))

(define (rotar-horaria-aux col dimension lista)
  (cond
    [(= col dimension) '()]
    [else
     (append (obtener-columna-al-reves lista dimension col dimension)
             (rotar-horaria-aux (+ col 1) dimension lista))]))

;;FUNCION ESPEJO HORIZONTAL;;
(define (espejo lista dimension)
  (cond
    [(null? lista) '()]
    [else
     (append (invertir-fila (tomar lista dimension) dimension)
             (espejo (saltar lista dimension) dimension))]))

;;FUNCION ESPEJO VERTICAL;;
(define (espejo-vertical lista dimension)
  (cond
    [(null? lista) '()]
    [else
     (append (espejo-vertical (saltar lista dimension) dimension)
             (tomar lista dimension))]))

;;; FIN FUNCIONES PARA MODIFICAR LISTAS ;;;

;;; FUNCIONES PARA OBTENER DATOS DE UNA LISTA ;;;
;;FUNCION PARA OBTENER UN ELEMENTO EN POSICION X,Y DE UNA LISTA;;
(define (obtener-elemento lista dimension fila columna)
  (obtener-posicion
   lista
   (+ (* dimension (- fila 1)) (- columna 1))))

(define (obtener-posicion lista indice)
  (cond
    [(zero? indice) (car lista)]
    [(null? lista) '()]
    [else (obtener-posicion (cdr lista) (- indice 1))]))

;;FUNCION OBTENER COLMUMNA;;
(define (obtener-columna lista dimension columna actual)
  (cond
    [(null? lista) '()]
    [(= (modulo actual dimension) columna)
     (cons (car lista)
           (obtener-columna (cdr lista) dimension columna (+ actual 1)))]
    [else
     (obtener-columna (cdr lista) dimension columna (+ actual 1))]))

;;FUNCION OBTENER COLMUMNA-REVERSA;;
(define (obtener-columna-al-reves lista dimension columna fila)
  (cond
    [(= fila 0) '()]
    [else
     (let ((pos (+ (* (- fila 1) dimension) columna)))
       (cons (obtener-en-posicion lista pos)
             (obtener-columna-al-reves lista dimension columna (- fila 1))))]))
(define (obtener-en-posicion lista n)
  (cond
    [(zero? n) (car lista)]
    [else (obtener-en-posicion (cdr lista) (- n 1))]))

;; Toma los primeros `n` elementos de la lista
(define (tomar lista n)
  (cond
    [(or (null? lista) (= n 0)) '()]
    [else (cons (car lista) (tomar (cdr lista) (- n 1)))]))

;; Salta los primeros `n` elementos de la lista
(define (saltar lista n)
  (cond
    [(or (null? lista) (= n 0)) lista]
    [else (saltar (cdr lista) (- n 1))]))

;; Invierte una lista de tamaño `n`
(define (invertir-fila fila n)
  (cond
    [(null? fila) '()]
    [else (append (invertir-fila (cdr fila) (- n 1)) (list (car fila)))]))

;;; FIN FUNCIONES PARA OBTENER DATOS DE UNA LISTA ;;;


;;INICIALIZAR CUBO;;
(RS 2 '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6)) '(F1D C1A C2B F2I F1D))

