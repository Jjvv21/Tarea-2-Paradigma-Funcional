#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) Parámetros que tú controlas
;;    - cube-n entre 2 y 6
;;    - sticker-size: ancho/profundidad de cada sticker (px)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cube-n       3)   ; <— Cámbialo aquí (2 .. 6)
(define sticker-size 40)  ; <— Ancho de cada “sticker”

;; Gap y margen los igualamos a sticker-size para simplificar
(define gap    sticker-size)
(define margin sticker-size)

;; Extensión “total” de un cubo en cada eje: 2·N·sticker-size
(define cube-extent (* 2 cube-n sticker-size))

;; Orígenes de dibujo para cada cubo, calculados para que no choquen
(define origin-x1 (+ margin (* cube-n sticker-size)))
(define origin-y  (+ margin (* cube-n sticker-size)))

(define origin-x2 (+ origin-x1 cube-extent gap))

;; Tamaño de ventana = espacio de dos cubos + gap + márgenes
(define canvas-width  
  (+ (* 4 cube-n sticker-size)   ; 2 cubos → 4·N·sticker-size
     gap                         ; hueco entre cubos
     (* 2 margin)))              ; márgenes izquierda + derecha

(define canvas-height 
  (+ (* 2 cube-n sticker-size)   ; altura necesaria para un cubo
     (* 2 margin)))              ; márgenes arriba + abajo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) Colores y generador de caras N×N
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (color-de letra)
  (cond [(equal? letra "R") "red"]
        [(equal? letra "O") "orange"]
        [(equal? letra "Y") "yellow"]
        [(equal? letra "G") "green"]
        [(equal? letra "B") "blue"]
        [(equal? letra "W") "white"]
        [else               "gray"]))

(define (make-face letra n)
  (build-list n (λ (_) (build-list n (λ (_) letra)))))

(define cube1
  (list (make-face "R" cube-n)
        (make-face "B" cube-n)
        (make-face "W" cube-n)))

(define cube2
  (list (make-face "O" cube-n)
        (make-face "G" cube-n)
        (make-face "Y" cube-n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3) Dibujo de un paralelogramo relleno
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-parallelogram dc x y color dx1 dy1 dx2 dy2)
  (send dc set-brush color 'solid)
  (send dc draw-polygon
        (list (cons x y)
              (cons (+ x dx1)     (+ y dy1))
              (cons (+ x dx1 dx2) (+ y dy1 dy2))
              (cons (+ x dx2)     (+ y dy2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4) Proyección isométrica de cada cara
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-top dc ox oy face)
  (let* ((dx1    sticker-size)
         (dy1    (- (/ sticker-size 2)))
         (dx2    (- sticker-size))
         (dy2    (- (/ sticker-size 2))))
    (let loop ((i 0) (rows face))
      (when (pair? rows)
        (let loop2 ((j 0) (cols (car rows)))
          (when (pair? cols)
            (let* ((x (+ ox (* j dx1) (* i dx2)))
                   (y (+ oy (* j dy1) (* i dy2))))
              (draw-parallelogram dc
                                  x y
                                  (color-de (car cols))
                                  dx1 dy1 dx2 dy2))
            (loop2 (add1 j) (cdr cols))))
        (loop (add1 i) (cdr rows))))))

(define (draw-left dc ox oy face)
  (let* ((dx1   (- sticker-size))
         (dy1   (- (/ sticker-size 2)))
         (dx2    0)
         (dy2    sticker-size))
    (let loop ((i 0) (rows face))
      (when (pair? rows)
        (let loop2 ((j 0) (cols (car rows)))
          (when (pair? cols)
            (let* ((x (+ ox (* j dx1) (* i dx2)))
                   (y (+ oy (* j dy1) (* i dy2))))
              (draw-parallelogram dc
                                  x y
                                  (color-de (car cols))
                                  dx1 dy1 dx2 dy2))
            (loop2 (add1 j) (cdr cols))))
        (loop (add1 i) (cdr rows))))))

(define (draw-right dc ox oy face)
  (let* ((dx1    sticker-size)
         (dy1   (- (/ sticker-size 2)))
         (dx2    0)
         (dy2    sticker-size))
    (let loop ((i 0) (rows face))
      (when (pair? rows)
        (let loop2 ((j 0) (cols (car rows)))
          (when (pair? cols)
            (let* ((x (+ ox (* j dx1) (* i dx2)))
                   (y (+ oy (* j dy1) (* i dy2))))
              (draw-parallelogram dc
                                  x y
                                  (color-de (car cols))
                                  dx1 dy1 dx2 dy2))
            (loop2 (add1 j) (cdr cols))))
        (loop (add1 i) (cdr rows))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5) Ensamblar las tres caras
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw-cube dc cube ox oy)
  (draw-top   dc ox       oy       (first  cube))
  (draw-left  dc ox       oy       (second cube))
  (draw-right dc ox       oy       (third  cube)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6) Interfaz: frame y canvas único
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define frame
  (new frame%
       [label  "Rubik Simulator 3D"]
       [width  canvas-width]
       [height canvas-height]))

(new canvas%
     [parent     frame]
     [min-width  canvas-width]
     [min-height canvas-height]
     [paint-callback
      (λ (canvas dc)
        ;; primer cubo
        (draw-cube dc cube1 origin-x1 origin-y)
        ;; segundo cubo desplazado en x
        (draw-cube dc cube2 origin-x2 origin-y))])

(send frame show #t)
