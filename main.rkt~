#lang racket

(require racket/gui)
(require (lib "graphics.ss" "graphics")) (open-graphics)

(define window (open-viewport "window" 600 700))

(define window2 (open-pixmap "invisibleWindow" 600 700))
((draw-solid-rectangle window2) (make-posn 0 0) 600 700 "white")

(define gameBoard
  (for/vector ([i (in-range 6)])
    (for/vector ([j (in-range 7)])
      random 0)))

(define (setGameBoardCoordinate row column value)
  (vector-set! (vector-ref gameBoard row) column value))

(define (startConnect4 option)
  (cond [(equal? option 0)
         ()]
        [(equal? option 1)
         (close-graphics)
         (exit)]
        ))

(define frame (new frame%
                   [label "Conecta 4"]
                   [width 700]
                   [height 600]))

(define winFrame (new frame%
                   [label "Conecta 4"]
                   [width 700]
                   [600]))

(define loseFrame (new frame%
                   [label "Conecta 4"]
                   [width 700]
                   [600]))

(define msg (new message% [parent frame]
                          [label "Bienvenido a Conecta 4. Selecciona una opción:"]))

(define winMsg (new message% [parent endFrame]
                          [label "Felicidades, has ganado el juego!!"]))

(define loseMsg (new message% [parent endFrame]
                          [label "Ha ganado la computadora. Vuelve a intentarlo cuando quieras!"]))

(new button% [parent frame]
             [label "Nuevo Juego"]
             [callback (lambda (button event)
                          (send frame show #f)
                          (startConnect4 0))])

(new button% [parent frame]
             [label "Cerrar"]
             [callback (lambda (button event)
                         (send frame show #f)
                         (startConnect4 1))])

(new button% [parent frame]
             [label "Volver al Menu Principal"]
             [callback (lambda (button event)
                         (send endFrame show #f)
                         (startConnect4 1))])

(send frame show #t)