; Instituto Tecnológico de Costa Rica
; Inteligencia Artificial
; I Tarea
; Manuel Ruiz
; Jorge Gatgens
;
; Profesora: Maria Auxiliadora Mora
; I semestre 2020

#lang racket


; LIBRERIAS =====================================================================================

(require racket/gui)
(require (lib "graphics.ss" "graphics")) (open-graphics)

;  ==============================================================================================

; DEFINICIONES ==================================================================================

(define turn 1)

(define dc-path (new dc-path%))

(define bitmap (make-object bitmap% 700 600))

(define bitmap-dc (make-object bitmap-dc% bitmap))

(send bitmap-dc clear)

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

(define gameFrame (new frame%
                    [label "Conecta 4"]
                    [width 700]
                    [height 600]))


(define msg (new message% [parent frame]
                          [label "Bienvenido a Conecta 4. Selecciona una opción:"]))


(define winMsg (new message% [parent winFrame]
                          [label "Felicidades, has ganado el juego!!"]))


(define loseMsg (new message% [parent loseFrame]
                          [label "Ha ganado la computadora. Vuelve a intentarlo cuando quieras!"]))

(define my-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (cond ([send event get-left-down]
          (let* ([col (floor (/ (send event get-x) 100))])
            (vector-set! gameBoard 5 turn)))))))


(define canvas
  (new my-canvas% [parent gameFrame]
       [min-width 700]
       [min-height 600]
       [paint-callback
         (lambda (canvas dc) (paint dc))]))


(new button% [parent frame]
             [label "Nuevo Juego"]
             [callback (lambda (button event)
                          (send frame show #f)
                          (send gameFrame show #t)
                          (startConnect4 0))])


(new button% [parent frame]
             [label "Cerrar"]
             [callback (lambda (button event)
                         ;(send frame show #f)
                         (startConnect4 1))])


(new button% [parent loseFrame]
             [label "Volver al Menu Principal"]
             [callback (lambda (button event)
                         ;(send loseFrame show #f)
                         (startConnect4 1))])

; ==============================================================================================

; FUNCIONES ====================================================================================

(define (setGameBoardInt row column value)
  (vector-set! (vector-ref gameBoard row) column value))

(define (getGameBoardInt row column)
  (vector-ref (vector-ref gameBoard row) column))

; Creación del tablero
(define gameBoard
  (for/vector ([i (in-range 6)])
    (for/vector ([j (in-range 7)])
      (random 1))))


(define (showBoard gameBoard)
  (begin
    (send bitmap-dc clear)
    (drawBoard)
    (addTiles)
    (send canvas refresh)))

(define (drawBoard)
  (begin
    (send bitmap-dc set-brush (make-object brush% "BLACK" 'transparent))
    (send bitmap-dc set-pen (make-object pen% "BLACK" 2 'solid))
    (divideGrid 6 7)
    (send bitmap-dc draw-path dc-path)))

(define (divideGrid row col)
  (begin
    (verticalLines col)
    (horizontalLines row)
    (send dc-path close)))

(define (horizontalLines row)
   (define (whileLoopRow j)
     (send dc-path move-to 0 (* j (/ 600 row)))
     (send dc-path line-to 700 (* j (/ 600 row)))
     (cond ((not (= j row)) (whileLoopRow (+ j 1)))))
   (whileLoopRow 1))

(define (verticalLines col)
  (define (whileLoopCol i)
    (send dc-path move-to (* i (/ 700 col)) 0)
    (send dc-path line-to (* i (/ 700 col)) 600)
    (cond ((not (= i col)) (whileLoopCol (+ i 1)))))
  (whileLoopCol 1))
  
  
(define (addTiles)
  (begin
    (for/vector ([i (in-range 6)])
      (for/vector ([j (in-range 7)])
        (cond
          [(= (getGameBoardInt i j) 0)
           (begin
             (printf "blah ")
             (send bitmap-dc set-brush (make-object brush% "WHITE" 'solid))
             (send bitmap-dc set-pen (make-object pen% "BLACK" 1 'solid))
             (send bitmap-dc draw-ellipse (+ (* i 100) 10) (+ (* j 100) 10) 80 80))]
          [(= (getGameBoardInt i j) 1)
           (begin
             (send bitmap-dc set-brush (make-object brush% "BLUE" 'solid))
             (send bitmap-dc set-pen (make-object pen% "BLACK" 1 'solid))
             (send bitmap-dc draw-ellipse (+ (* i 100) 10) (+ (* j 100) 10) 80 80))]
          [(= (getGameBoardInt i j) 2)
           (begin
             (send bitmap-dc set-brush (make-object brush% "RED" 'solid))
             (send bitmap-dc set-pen (make-object pen% "BLACK" 1 'solid))
             (send bitmap-dc draw-ellipse (+ (* i 100) 10) (+ (* j 100) 10) 80 80))])))))


;
(define (startConnect4 option)
  (cond [(equal? option 0)
         (showBoard gameBoard)
         (send canvas refresh)
         ;(startGame)
         ]
        [(equal? option 1)
         (send frame show #f)
         (exit)]
        ))

(define (paint dc)
  (send dc draw-bitmap bitmap 0 0))

(define (main)
  (send frame show #t))

;  ==================================================================================================

; MAIN CALL =========================================================================================

(main)