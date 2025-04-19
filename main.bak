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


;;;; FUNCION PARA LAS INSTRUCCIONES DE MOVIMIENTO DEL CUBO ;;;;

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
  (display "Realizar movimiento fila:")
  (display n)
  (display "a la derecha")
  (newline)
  (elegir_movimiento dimension cubo (cdr movimientos)))

(define (mov_fila_izquierda dimension cubo n movimientos)
  (display "Realizar movimiento fila:")
  (display n)
  (display "a la izquierda")
  (newline)
  (elegir_movimiento dimension cubo (cdr movimientos)))

(define (mov_columna_arriba dimension cubo n movimientos)
  (display "Realizar movimiento columna:")
  (display n)
  (display "hacia arriba")
  (newline)
  (elegir_movimiento dimension cubo (cdr movimientos)))

(define (mov_columna_abajo dimension cubo n movimientos)
  (display "Realizar movimiento columna:")
  (display n)
  (display "hacia abajo")
  (newline)
  (elegir_movimiento dimension cubo (cdr movimientos)))

(RS 2 '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6)) '(F1D F2I C2A C1B))
