#lang racket/gui
(require "main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilidades
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (color-de-num n)
  (case n
    [(1) "white"]
    [(2) "blue"]
    [(3) "green"]
    [(4) "orange"]
    [(5) "red"]
    [(6) "yellow"]
    [else "gray"]))

(define (crear-lista-caras n)
  (define total (* n n))
  (build-list 6 (λ (i) (make-list total (add1 i))))) ; ((1 1 1 1) (2 2 2 2) ...)

(define (list->face plano n)
  (define (fila i) (take (drop plano (* i n)) n))
  (build-list n fila))

(define (lista-a-cubo lista n)
  (map (λ (cara) (list->face cara n)) lista)) ; Devuelve: '(((1 1)(1 1))...) para usar en dibujo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dibujo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-parallelogram dc x y color dx1 dy1 dx2 dy2)
  (send dc set-brush color 'solid)
  (send dc draw-polygon
        (list (cons x y)
              (cons (+ x dx1)     (+ y dy1))
              (cons (+ x dx1 dx2) (+ y dy1 dy2))
              (cons (+ x dx2)     (+ y dy2)))))

(define (draw-face dc ox oy face dx1 dy1 dx2 dy2)
  (let loop ((i 0) (rows face))
    (when (pair? rows)
      (let loop2 ((j 0) (cols (car rows)))
        (when (pair? cols)
          (let* ((x (+ ox (* j dx1) (* i dx2)))
                 (y (+ oy (* j dy1) (* i dy2))))
            (draw-parallelogram dc
                                x y
                                (color-de-num (car cols))
                                dx1 dy1 dx2 dy2))
          (loop2 (add1 j) (cdr cols))))
      (loop (add1 i) (cdr rows)))))

(define (draw-top dc ox oy face)
  (define s sticker-size)
  (draw-face dc ox oy face s (- (/ s 2)) (- s) (- (/ s 2))))

(define (draw-left dc ox oy face)
  (define s sticker-size)
  (draw-face dc ox oy face (- s) (- (/ s 2)) 0 s))

(define (draw-right dc ox oy face)
  (define s sticker-size)
  (draw-face dc ox oy face s (- (/ s 2)) 0 s))

(define (draw-cube-front dc cube ox oy n)
  (draw-top   dc ox       oy       (espejo-horizontal(rotar-antihorario (first  cube) n) n))
  (draw-left  dc ox       oy       (espejo-horizontal (second cube) n))
  (draw-right dc ox       oy       (third  cube)))

(define (draw-cube-back dc cube ox oy n)
  (draw-top   dc ox       oy       (espejo-vertical (first  cube) n))
  (draw-left  dc ox       oy       (espejo-vertical(second cube) n))
  (draw-right dc ox       oy       (espejo-vertical (third  cube) n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables y estado inicial (2x2 por defecto)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define initial-n 2)
(define initial-lista (crear-lista-caras initial-n))
(define initial-cubo (lista-a-cubo initial-lista initial-n))

(define current-cube-n (box initial-n))
(define current-cube1 (box (list (list-ref initial-cubo 3)
                                 (list-ref initial-cubo 0)
                                 (list-ref initial-cubo 1))))
(define current-cube2 (box (list (list-ref initial-cubo 4)
                                 (list-ref initial-cubo 2)
                                 (list-ref initial-cubo 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfaz combinada
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sticker-size 40)
(define current-lista-cara-plana (box '((1 1 1 1) (2 2 2 2) (3 3 3 3) (4 4 4 4) (5 5 5 5) (6 6 6 6))))
;; Función para redibujar canvas
(define (draw-cube canvas dc)
  (define cube-n (unbox current-cube-n))
  (define gap sticker-size)
  (define margin sticker-size)
  (define cube-extent (* 2 cube-n sticker-size))
  (define origin-x1 (+ margin (* cube-n sticker-size)))
  (define origin-y  (+ margin (* cube-n sticker-size)))
  (define origin-x2 (+ origin-x1 cube-extent gap))

  (draw-cube-front dc (unbox current-cube1) origin-x1 origin-y cube-n)
  (draw-cube-back  dc (unbox current-cube2) origin-x2 origin-y cube-n))

;; Frame principal
(define frame (new frame% [label "Rubik Simulator 3D"] [width 800] [height 600]))
(define main-panel (new vertical-panel% [parent frame] [alignment '(center center)]))

;; Entrada para tamaño del cubo
(define size-panel (new horizontal-panel% [parent main-panel]))
(new message% [parent size-panel] [label "Tamaño del Cubo (2-6):"])
(define size-input (new text-field% [parent size-panel] [label ""]))

(define canvas #f) ; Será creado más adelante

(define (generar-cubo! n)
  (set-box! current-cube-n n)
  (define lista-cara-plana (crear-lista-caras n))
  (set-box! current-lista-cara-plana lista-cara-plana)
  (define cubo-caras (lista-a-cubo lista-cara-plana n))
  (set-box! current-cube1
            (list (list-ref cubo-caras 3)   ; arriba
                  (list-ref cubo-caras 0)   ; izq
                  (list-ref cubo-caras 1))) ; der
  (set-box! current-cube2
            (list (list-ref cubo-caras 4)   ; frente
                  (list-ref cubo-caras 2)   ; abajo
                  (list-ref cubo-caras 5))) ; fondo
  (send canvas refresh))

(new button%
     [parent size-panel]
     [label "Crear Cubo"]
     [callback
      (λ (_ evt)
        (define val (string->number (send size-input get-value)))
        (if (and val (integer? val) (<= 2 val 6))
            (generar-cubo! val)
            (message-box "Error" "Por favor ingresa un número entre 2 y 6")))])

;; Canvas para dibujo del cubo
(set! canvas
      (new canvas%
           [parent     main-panel]
           [min-width  1100]
           [min-height 600]
           [paint-callback draw-cube]))

;; Sección para ingresar movimiento
(new message% [parent main-panel] [label "Movimiento a Realizar:"])
(define mov-input (new text-field% [parent main-panel] [label ""]))

(new button%
     [parent main-panel]
     [label "Enviar"]
     [callback
      (λ (_ evt)
        (define mov (send mov-input get-value))
        (printf "Movimiento ingresado: ~a\n" mov)
        (set-box! current-lista-cara-plana (leer_movimientos (unbox current-cube-n) (unbox current-lista-cara-plana) (list mov) '()))
        (define cubo-caras (lista-a-cubo (unbox current-lista-cara-plana) (unbox current-cube-n)))
        (set-box! current-cube1
            (list (list-ref cubo-caras 3)   ; arriba
                  (list-ref cubo-caras 0)   ; izq
                  (list-ref cubo-caras 1))) ; der
        (set-box! current-cube2
            (list (list-ref cubo-caras 4)   ; frente
                  (list-ref cubo-caras 2)   ; abajo
                  (list-ref cubo-caras 5))) ; fondo
        (send canvas refresh))])

(send frame show #t)

(define (rotar-horario face n)
  ;; Lógica de rotación horaria para una cara NxN
  (for/list ([j (in-range n)])
    (for/list ([i (in-range (sub1 n) -1 -1)])
      (list-ref (list-ref face i) j))))

(define (espejo-horizontal face n)
  (for/list ([fila face])
    (reverse fila)))

(define (rotar-antihorario face n)
  (for/list ([j (in-range (sub1 n) -1 -1)])
    (for/list ([i (in-range n)])
      (list-ref (list-ref face i) j))))

(define (espejo-vertical face n)
  (reverse face))