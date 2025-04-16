#lang racket

;  Funcion principal del programa para inicializar, dar dimension y mover el cubo

(define (RS X Cubo Movs)
  (cond
    [(or (null? Cubo) (null? Movs)) (Mensaje_Error_1)]
    [(not (number? X)) (Mensaje_Error_2)]
    [(or (< X 2) (> X 6)) (Mensaje_Error_2)]
    [(not (cubo-Valido? X Cubo)) (Mensaje_Error_1)] ;funcion que verifica que la matriz sea de las dimensiones correctas segun el parametro x, si tiene dimensiones incorrectas devuelve true
    [else (leer_movimientos X Cubo Movs)]))

;Mensaje de error por si la lista con los movimientos esta vacía o si el cubo no se inicializó bien con la matriz
(define (Mensaje_Error_1)
  (display "Error, no inicializaste de manera correcta el cubo"))

;Mensaje de error por si las dimensiones del cubo son incorrectas, menores a 2x2 o menores a 6x6 
(define (Mensaje_Error_2)
  (display "Error, las dimensiones para inicializar el cubo pueden ser de 2x2 hasta 6x6"))





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
    (define total-por-color (* X X)) ; Cuántas veces debería aparecer cada número
    (define todos (aplanar cubo))    ; Aplanamos el cubo en una lista de todos los colores
    (define (contar num lista)
      (cond
        [(null? lista) 0]
        [(= (car lista) num) (+ 1 (contar num (cdr lista)))]
        [else (contar num (cdr lista))]))
    (define (verificar n)
      (cond
        [(> n 6) #t] ; Ya revisamos todos los números
        [(not (= (contar n todos) total-por-color)) #f] ; Si la frecuencia no es correcta, retorna #f
        [else (verificar (+ n 1))])) ; Verificar el siguiente número
    (verificar 1)) ; Empezamos desde el número 1

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

;Funcion principal para leer los movimientos que se le quieren dar al cubo
(define (leer_movimientos dimension cubo movimientos)
  (display "leyendo movimientos de la lista \n"))


(RS 2 '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6)) '(F1D F2I C2A C1B))
(RS 2 '((1 1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6)) '(F1D F2I C2A C1B))
(RS 2 '((1 1 1 a) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6)) '(F1D F2I C2A C1B))
(RS 2 '((1 1 1 1) (1 1 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6)) '(F1D F2I C2A C1B))


