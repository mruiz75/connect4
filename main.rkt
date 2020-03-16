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

(require "minimax.rkt")
(require racket/gui)
(require (lib "graphics.ss" "graphics")) (open-graphics)

;  ==============================================================================================

; DEFINICIONES ==================================================================================

(define turn 1)
(define winner #f)
(define tie #f)
(define iniciaComputadora #f)
(define color #f)  ; #f = red, #t = blue
(define maxTime 400)
(define firstSlotAvailable (vector 5 5 5 5 5 5 5))

(define dc-path (new dc-path%))

(define bitmap (make-object bitmap% 700 600))

(define bitmap-dc (make-object bitmap-dc% bitmap))

(send bitmap-dc clear)

(define frame (new frame%
                   [label "Conecta 4"]
                   [width 400]
                   [height 300]))


(define winFrame (new frame%
                   [label "Conecta 4"]
                   [width 400]
                   [height 300]))


(define loseFrame (new frame%
                   [label "Conecta 4"]
                   [width 400]
                   [height 500]))

(define optionsFrame (new frame%
                      [label "Conecta 4"]
                      [width 400]
                      [height 300]))

(define colorPickFrame (new frame%
                        [label "Conecta 4"]
                        [width 400]
                        [height 300]))

(define gameFrame (new frame%
                    [label "Conecta 4"]
                    [width 700]
                    [height 600]))


(define msg (new message% [parent frame]
                          [label "Bienvenido a Conecta 4. Selecciona una opción:"]))

(define msgGameBoard (new message% [parent gameFrame]
                          [label "Elige una columna donde desees jugar"]))

(define playerMsg (new message% [parent optionsFrame]
                          [label "¿Quién comenzará el juego?"]))

(define colorMsg (new message% [parent colorPickFrame]
                          [label "¿Qué color de fichas deseas?"]))


(define winMsg (new message% [parent winFrame]
                          [label "Felicidades, has ganado el juego!!"]))


(define loseMsg (new message% [parent loseFrame]
                          [label "Ha ganado la computadora. Vuelve a intentarlo cuando quieras!"]))

; Esta clase de objeto crea un canvas que determina la posición en el eje X donde se hace click. Una vez hecho
; el click, se procesa esa información para encontrar la columna donde colocar la ficha y se hace el análisis de
; si la ficha puede ser colocada y si la jugada realizada implica que el jugador gano. Como el canvas es frecuente-
; mente actualizado, se puede alternar el turno dentro de si mismo. Cuando el jugador termina su turno, se lo cede a
; la computadora y esta hace lo mismo al finalizar.
(define my-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
    (define colum -1)
    (if (equal? iniciaComputadora #t)
        ;aqui iria la funcion de IA y se elimina la mayoria de lo de abajo. Ahorita solo esta para probar dos jugadores
          (begin
            (set! colum (minimax (lista gameBoard) -10000 0))
            (placeTile colum)
            (cond
              [(gano (lista gameBoard) 1)
               (begin
                 (showBoard gameBoard)
                 (set! winner #t)
                 (send winFrame show #t))]
              [#t (begin
                    (if (equal? color #t)
                        (set! color #f)
                        (set! color #t))
                    (send msgGameBoard set-label "Turno del jugador")
                    (set! iniciaComputadora #f))]))

           (cond ([send event get-left-down]
                  (let* ([col (floor (/ (send event get-x) 100))])
                    (begin
                      (cond
                        [(validCol col)
                         (begin
                           (placeTile col)
                           (cond
                             [(gano (lista gameBoard) 2)
                              (begin
                                (showBoard gameBoard)
                                (set! winner #t)
                                (send winFrame show #t))]
                             [#t (begin
                                   (if (equal? color #t)
                                       (set! color #f)
                                       (set! color #t))
                                   (send msgGameBoard set-label "Turno de la computadora")
                                   (set! iniciaComputadora #t))]))]
                        [#t (send msgGameBoard set-label "Columna llena, intente en otro lugar!")]))))))))
    )


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
                          (send optionsFrame show #t))])


(new button% [parent frame]
             [label "Cerrar"]
             [callback (lambda (button event)
                         (send frame show #f)
                         (startConnect4 1))])

(new button% [parent optionsFrame]
             [label "Usuario"]
             [callback (lambda (button event)
                         (send optionsFrame show #f)
                         (send colorPickFrame show #t)
                         )])

(new button% [parent optionsFrame]
             [label "Computadora"]
             [callback (lambda (button event)
                         (send optionsFrame show #f)
                         (set! iniciaComputadora #t)
                         (send colorPickFrame show #t)
                         )])

(new button% [parent colorPickFrame]
             [label "Azul"]
             [callback (lambda (button event)
                         (send colorPickFrame show #f)
                         (set! color #t)
                         (send gameFrame show #t)
                         (startConnect4 0)
                         )])

(new button% [parent colorPickFrame]
             [label "Rojo"]
             [callback (lambda (button event)
                         (send colorPickFrame show #f)
                         (send gameFrame show #t)
                         (startConnect4 0)
                         )])


(new button% [parent loseFrame]
             [label "Cerrar"]
             [callback (lambda (button event)
                         (send gameFrame show #f)
                         (send loseFrame show #f)
                         (startConnect4 1))])

(new button% [parent winFrame]
             [label "Cerrar"]
             [callback (lambda (button event)
                         (send gameFrame show #f)
                         (send winFrame show #f)
                         (startConnect4 1))])

; ==============================================================================================

; FUNCIONES ====================================================================================

; función encargada de setear un número en un campo específico del tablero
; @Param row: fila donde ingresar el valor
; @Param col: columna donde ingresar el valor
; @Param value: valor por ingresar
(define (setGameBoardInt row column value)
  (vector-set! (vector-ref gameBoard row) column value))

; Función encargada de retornar el número en un campo específico del tablero
; @Param row: fila por consultar
; @Param col: columna por consultar
(define (getGameBoardInt row column)
  (vector-ref (vector-ref gameBoard row) column))

; Creación del tablero
(define gameBoard
  (for/vector ([i (in-range 6)])
    (for/vector ([j (in-range 7)])
      (random 1))))

; Función encargada de mantener actualizado y mandar a dibujar sobre el frame de juego
; @Param gameBoard: vector de vectores que contiene el tablero de juego
(define (showBoard gameBoard)
  (begin
    (send bitmap-dc clear)
    (drawBoard)
    (addTiles)
    (send canvas refresh)
    (sleep/yield 0.01)))

; Función encargada de dibujar el tablero de juego
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

; Función encarga de establecer los caminos para dibujar las lineas horizontales
; @Param row: cantidad de lineas por dibujar
(define (horizontalLines row)
   (define (whileLoopRow j)
     (send dc-path move-to 0 (* j (/ 600 row)))
     (send dc-path line-to 700 (* j (/ 600 row)))
     (cond ((not (= j row)) (whileLoopRow (+ j 1)))))
   (whileLoopRow 1))

; Función encarga de establecer los caminos para dibujar las lineas verticales
; @Param row: cantidad de lineas por dibujar
(define (verticalLines col)
  (define (whileLoopCol i)
    (send dc-path move-to (* i (/ 700 col)) 0)
    (send dc-path line-to (* i (/ 700 col)) 600)
    (cond ((not (= i col)) (whileLoopCol (+ i 1)))))
  (whileLoopCol 1))

; Función que coloca la ficha en el campo correcto en la columna elegida
; @Param col: columna donde se coloca la ficha
(define (placeTile col)
  (cond
    [(equal? color #f)
     (setGameBoardInt (vector-ref firstSlotAvailable col) col 2)
     (vector-set! firstSlotAvailable col (- (vector-ref firstSlotAvailable col) 1))]
    [else
     (setGameBoardInt (vector-ref firstSlotAvailable col) col 1)
     (vector-set! firstSlotAvailable col (- (vector-ref firstSlotAvailable col) 1))]))

; Función encargada de pintar las fichas en el bitmap
(define (addTiles)
  (begin
    (for/vector ([i (in-range 6)])
      (for/vector ([j (in-range 7)])
        (cond
          [(= (getGameBoardInt i j) 0)
           (begin
             (send bitmap-dc set-brush (make-object brush% "WHITE" 'solid))
             (send bitmap-dc set-pen (make-object pen% "BLACK" 1 'solid))
             (send bitmap-dc draw-ellipse (+ (* j 100) 10) (+ (* i 100) 10) 80 80))]
          [(= (getGameBoardInt i j) 1)
           (begin
             (send bitmap-dc set-brush (make-object brush% "BLUE" 'solid))
             (send bitmap-dc set-pen (make-object pen% "BLACK" 1 'solid))
             (send bitmap-dc draw-ellipse (+ (* j 100) 10) (+ (* i 100) 10) 80 80))]
          [(= (getGameBoardInt i j) 2)
           (begin
             (send bitmap-dc set-brush (make-object brush% "RED" 'solid))
             (send bitmap-dc set-pen (make-object pen% "BLACK" 1 'solid))
             (send bitmap-dc draw-ellipse (+ (* j 100) 10) (+ (* i 100) 10) 80 80))])))))


; Función para validar si la columna donde se quiere ingresar una ficha aun tiene campo
; @Param col: columna a evaluar
(define (validCol col)
  (= (vector-ref (vector-ref gameBoard 0) col) 0))


; Función para evaluar si un jugador realizó una jugada de gane
; @Param row: fila donde se ingreso la última ficha
; @Param col: columna donde se ingreso la última ficha
(define (validateWin row col)
  (begin
    (define player (getGameBoardInt row col))
    (define vertValidation (validateVert row col player))
    (define hrztValidation (validateHorizontal row col player))
    (define diagValidation (validateDiag player))
    (cond [(or vertValidation hrztValidation diagValidation) #t]
          [#t #f])))


; Función encargada de evaluar si un jugador ganó con una jugada vertical. Evalua todas las posibles jugadas verticales
; para la columna de la última jugada y de acuerdo a la fila dentro de la misma linea.
; @Param row: fila de la ultima ficha agregada
; @Param col: columna de la ultima ficha agregada
; @Param player: color del jugador que ingresó la última ficha
(define (validateVert row col player)
  (cond
    [(< row 2)
        (cond
          [(and (= (getGameBoardInt row col) player) (= (getGameBoardInt (+ row 1) col) player) (= (getGameBoardInt (+ row 2) col) player) (= (getGameBoardInt (+ row 3) col) player)) #t]
          [(and (= (getGameBoardInt 3 col) player) (= (getGameBoardInt 2 col) player) (= (getGameBoardInt 1 col) player) (= (getGameBoardInt 0 col) player)) #t]
          [#t #f])]
    [(or (= row 3) (= row 4))
         (cond
           [(and (= (getGameBoardInt 3 col) player) (= (getGameBoardInt 2 col) player) (= (getGameBoardInt 1 col) player) (= (getGameBoardInt 0 col) player)) #t]
           [(and (= (getGameBoardInt 1 col) player) (= (getGameBoardInt 2 col) player) (= (getGameBoardInt 3 col) player) (= (getGameBoardInt 4 col) player)) #t]
           [(and (= (getGameBoardInt 2 col) player) (= (getGameBoardInt 3 col) player) (= (getGameBoardInt 4 col) player) (= (getGameBoardInt 5 col) player)) #t]
           [#t #f])]
    [(> row 4)
         (cond
           [(and (= (getGameBoardInt row col) player) (= (getGameBoardInt (- row 1) col) player) (= (getGameBoardInt (- row 2) col) player) (= (getGameBoardInt (- row 3) col) player)) #t]
           [(and (= (getGameBoardInt 2 col) player) (= (getGameBoardInt 3 col) player) (= (getGameBoardInt 4 col) player) (= (getGameBoardInt 5 col) player)) #t]
           [#t #f])]))


; Función encargada de evaluar si un jugador ganó con una jugada horizontal. Evalua todas las posibles jugadas horizontales
; para la linea de la última jugada y de acuerdo a la columna dentro de la misma linea.
; @Param row: fila de la ultima ficha agregada
; @Param col: columna de la ultima ficha agregada
; @Param player: color del jugador que ingresó la última ficha
(define (validateHorizontal row col player)
  (cond
    [(< col 2)
     (cond
       [(and (= (getGameBoardInt row col) player) (= (getGameBoardInt row (+ col 1)) player) (= (getGameBoardInt row (+ col 2)) player) (= (getGameBoardInt row (+ col 3)) player)) #t]
       [(and (= (getGameBoardInt row 0) player) (= (getGameBoardInt row 1) player) (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player)) #t]
       [#t #f])]
    [(= col 2)
     (cond
       [(and (= (getGameBoardInt row 0) player) (= (getGameBoardInt row 1) player) (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player)) #t]
       [(and (= (getGameBoardInt row 1) player) (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player)) #t]
       [(and (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player) (= (getGameBoardInt row 5) player)) #t]
       [#t #f])]
    [(= col 3)
     (cond
       [(and (= (getGameBoardInt row 0) player) (= (getGameBoardInt row 1) player) (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player)) #t]
       [(and (= (getGameBoardInt row 1) player) (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player)) #t]
       [(and (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player) (= (getGameBoardInt row 5) player)) #t]
       [(and (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player) (= (getGameBoardInt row 5) player) (= (getGameBoardInt row 6) player)) #t]
       [#t #f])]
    [(= col 4)
     (cond
       [(and (= (getGameBoardInt row 1) player) (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player)) #t]
       [(and (= (getGameBoardInt row 2) player) (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player) (= (getGameBoardInt row 5) player)) #t]
       [(and (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player) (= (getGameBoardInt row 5) player) (= (getGameBoardInt row 6) player)) #t]
       [#t #f])]
    [(> col 4)
     (cond
       [(and (= (getGameBoardInt row col) player) (= (getGameBoardInt row (- col 1)) player) (= (getGameBoardInt row (- col 2)) player) (= (getGameBoardInt row (- col 3)) player)) #t]
       [(and (= (getGameBoardInt row 3) player) (= (getGameBoardInt row 4) player) (= (getGameBoardInt row 5) player) (= (getGameBoardInt row 6) player)) #t]
       [#t #f])]))


; Función que valida si un jugador ha ganado con una jugada diagonal. Comienza desde la fila de arriba,
; evaluando todas las posibles jugadas diagonales
; @Param player: color de ficha del jugador actual
(define (validateDiag player)
  (define win 0)
  (begin
    (for/vector ([i (in-range 3)])
     (for/vector ([j (in-range 7)])
       (cond
         [(< j 3)
          (cond
            [(and (= (getGameBoardInt i j) player) (= (getGameBoardInt (+ i 1) (+ j 1)) player) (= (getGameBoardInt (+ i 2) (+ j 2)) player) (= (getGameBoardInt (+ i 3) (+ j 3)) player)) (set! win 1)])]
         [(= j 3)
           (cond
             [(and (= (getGameBoardInt i j) player) (= (getGameBoardInt (+ i 1) (+ j 1)) player) (= (getGameBoardInt (+ i 2) (+ j 2)) player) (= (getGameBoardInt (+ i 3) (+ j 3)) player)) (set! win 1)]
             [(and (= (getGameBoardInt i j) player) (= (getGameBoardInt (+ i 1) (- j 1)) player) (= (getGameBoardInt (+ i 2) (- j 2)) player) (= (getGameBoardInt (+ i 3) (- j 3)) player)) (set! win 1)])]
         [(> j 3)
           (cond
             [(and (= (getGameBoardInt i j) player) (= (getGameBoardInt (+ i 1) (- j 1)) player) (= (getGameBoardInt (+ i 2) (- j 2)) player) (= (getGameBoardInt (+ i 3) (- j 3)) player)) (set! win 1)])])))
    (cond
      [(= win 1) #t]
      [(= win 0) #f])))


; Función que reestablece la configuración original del tablero
; @Param gameBoard: el tablero de juego
(define (resetBoard gameBoard)
  (for/vector ([i (in-range 6)])
    (for/vector ([j (in-range 7)])
      (setGameBoardInt i j 0))))


; Función encargada de iterar a la espera de una respuesta de parte del jugador
; @Param option: opción de juego, ya sea jugar o cerrar el juego
(define (startConnect4 option)
  (cond [(equal? option 0)
         (define (iterator i)
           (cond
             [(and (equal? winner #f) (equal? tie #f) (< i 1000))
                (cond
                  [(equal? iniciaComputadora #f)
                     (showBoard gameBoard)
                     (iterator (+ i 1))]
                  [else
                     (showBoard gameBoard)
                     (iterator (+ i 1))])]
           ))
         (iterator 0)]
        [(equal? option 1)
         (send frame show #f)
         (exit)]
        ))


; Función encargada de establecer el bitmap encima del canvas utilizado
; @param dc: el drawing context donde se va a dibujar 
(define (paint dc)
  (send dc draw-bitmap bitmap 0 0))


; Función que muestra el menú principal
(define (main)
  (send frame show #t))

;  ==================================================================================================

; MAIN CALL =========================================================================================

(main)
(showBoard gameBoard)