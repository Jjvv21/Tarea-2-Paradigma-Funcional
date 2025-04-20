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
  (define cara (build-list total (λ (i) (add1 i)))) ; (1 2 3 4 ... n^2)
  (build-list 6 (λ (_) cara))) ; ((1 2 3 4) (1 2 3 4) ...)

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
  (draw-top   dc ox       oy       (rotar-horario (rotar-horario (first  cube) n) n))
  (draw-left  dc ox       oy       (espejo-horizontal (second cube) n))
  (draw-right dc ox       oy       (third  cube)))

(define (draw-cube-back dc cube ox oy n)
  (draw-top   dc ox       oy       (espejo-vertical (first  cube) n))
  (draw-left  dc ox       oy       (espejo-vertical(second cube) n))
  (draw-right dc ox       oy       (espejo-vertical (third  cube) n)))


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
          (draw-cube-front dc cube1 origin-x1 origin-y cube-n)
          (draw-cube-back dc cube2 origin-x2 origin-y cube-n))])

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