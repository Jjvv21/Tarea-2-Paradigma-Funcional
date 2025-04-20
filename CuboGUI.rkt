#lang racket/gui

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

(define (draw-cube dc cube ox oy)
  (draw-top   dc ox       oy       (first  cube))
  (draw-left  dc ox       oy       (second cube))
  (draw-right dc ox       oy       (third  cube)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfaz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sticker-size 40)

(define (crear-interfaz cube-n)
  (define gap sticker-size)
  (define margin sticker-size)
  (define cube-extent (* 2 cube-n sticker-size))

  (define origin-x1 (+ margin (* cube-n sticker-size)))
  (define origin-y  (+ margin (* cube-n sticker-size)))
  (define origin-x2 (+ origin-x1 cube-extent gap))

  (define canvas-width  
    (+ (* 4 cube-n sticker-size) gap (* 2 margin)))

  (define canvas-height 
    (+ (* 2 cube-n sticker-size) (* 2 margin)))

  (define frame
    (new frame%
         [label  "Rubik Simulator 3D"]
         [width  canvas-width]
         [height canvas-height]))

  (define lista-cara-plana (crear-lista-caras cube-n)) ; ((1 1 1 1) ...)
  (define cubo-caras (lista-a-cubo lista-cara-plana cube-n))

  (define cube1
    (list (list-ref cubo-caras 3)   ;Cara arriba
          (list-ref cubo-caras 0)   ;Cara izq
          (list-ref cubo-caras 1))) ;Cara derecha

  (define cube2
    (list (list-ref cubo-caras 4)
          (list-ref cubo-caras 2)
          (list-ref cubo-caras 5)))

  (new canvas%
       [parent     frame]
       [min-width  canvas-width]
       [min-height canvas-height]
       [paint-callback
        (λ (canvas dc)
          (draw-cube dc cube1 origin-x1 origin-y)
          (draw-cube dc cube2 origin-x2 origin-y))])

  (send frame show #t)
  (printf "Lista usada: ~a\n" lista-cara-plana)) ; Para ver en consola

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfaz para entrada de número
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define entrada-frame (new frame% [label "Tamaño del Cubo"] [width 300] [height 100]))
(define panel (new horizontal-panel% [parent entrada-frame]))

(new message% [parent panel] [label "Introduce tamaño (2-6):"])

(define txt (new text-field% [parent panel] [label ""]))
(define btn
  (new button%
       [parent panel]
       [label "Crear Cubo"]
       [callback
        (λ (canvas dc)
          (define val (string->number (send txt get-value)))
          (cond
            [(and val (integer? val) (<= 2 val 6))
             (send entrada-frame show #f)
             (crear-interfaz val)]
            [else (message-box "Error" "Por favor ingresa un número entre 2 y 6")]))]))

(send entrada-frame show #t)