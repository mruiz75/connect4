#lang racket

(require racket/gui)
(require (lib "graphics.ss" "graphics")) (open-graphics)

(define window (open-viewport "window" 700 600))

(define window2 (open-pixmap "invisibleWindow" 700 600))
((draw-solid-rectangle window2) (make-posn 0 0) 700 600 "white")

(define gameBoard
  (for/vector ([i (in-range 6)])
    (for/vector ([j (in-range 7)])
      (random 1))))

(define (setGameBoardInt row column value)
  (vector-set! (vector-ref gameBoard row) column value))

(define (getGameBoardInt row column)
  (vector-ref (vector-ref gameBoard row) column))

(define (showBoard gameBoard)
  (for/vector ([i (in-range 6)])
    (for/vector ([j (in-range 7)])
      (cond
        [(= (getGameBoardInt i j) 0) ((draw-pixmap window2) "/Users/Manuel/Documents/TEC/2020/IA/connect4/tiles/whiteTile.png" (make-posn (* 100 j) (* 100 i)))]
        [(= (getGameBoardInt i j) 1) ((draw-pixmap window2) "/Users/Manuel/Documents/TEC/2020/IA/connect4/tiles/redTile.png" (make-posn (* 100 j) (* 100 i)))]
        [(= (getGameBoardInt i j) 2) ((draw-pixmap window2) "/Users/Manuel/Documents/TEC/2020/IA/connect4/tiles/blueTile.png" (make-posn (* 100 j) (* 100 i)))])
        ))
  (copy-viewport window2 window))


(define (startConnect4 option)
  (cond [(equal? option 0)
         (showBoard gameBoard)
         ]
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
                   [height 600]))

(define loseFrame (new frame%
                   [label "Conecta 4"]
                   [width 700]
                   [height 600]))

(define msg (new message% [parent frame]
                          [label "Bienvenido a Conecta 4. Selecciona una opción:"]))

(define winMsg (new message% [parent winFrame]
                          [label "Felicidades, has ganado el juego!!"]))

(define loseMsg (new message% [parent loseFrame]
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

(new button% [parent loseFrame]
             [label "Volver al Menu Principal"]
             [callback (lambda (button event)
                         (send loseFrame show #f)
                         (startConnect4 1))])

(send frame show #t)
