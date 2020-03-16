#lang racket

(provide minimax)
(provide lista)
(provide gano)

;Funcion que pasa el tablero del juego
;de vectores a listas
(define (lista gameBoard)
  (define milista '())
  (for ([i '(0 1 2 3 4 5)])
    (set! milista (append milista (list (vector->list (vector-ref gameBoard i))))))
  milista)


;Funcion que inserta una ficha en la columna deseada
(define (jugada tablero jugador columna)
  (define bandera #f)
  (for ([i '(5 4 3 2 1 0)])
    #:break (equal? bandera #t)
    (cond [(= (list-ref (list-ref tablero i) columna) 0)
           (set! tablero (list-set tablero i (list-set (list-ref tablero i) columna jugador)))
           (set! bandera #t)]))
  tablero)

(define mejor-columna 3)
(define maximo-actual -10000)

;Funcion principal que realiza el algoritmo de minimax
(define (minimax tablero maximo columna)
  (cond
    [(> columna 6) mejor-columna]
    [(verificar-columna tablero columna)
     (set! maximo-actual (valor-min (jugada tablero 1 columna) 1 -10000 10000))
     (cond
       [(> maximo-actual maximo)
        (set! maximo maximo-actual)
        (set! mejor-columna columna)
        (minimax tablero maximo (add1 columna))]
       [else (minimax tablero maximo (add1 columna))])]
    [else (minimax tablero maximo (add1 columna))]))

;Funcion del valormax del minimax que jugara por la IA
(define (valor-max tablero altura alfa beta)
  (cond
    [(or (estado-terminal tablero) (= altura 6)) (eval tablero)]
    [else
     (for ([i '(0 1 2 3 4 5 6)])
       (cond
         [(verificar-columna tablero i)
          (set! alfa (max alfa (valor-min (jugada tablero 1 i) (add1 altura) alfa beta)))
          (cond [(>= alfa beta) beta])]))
     alfa]))

;Funcion del valormin del minimax que jugara por el humano
(define (valor-min tablero altura alfa beta)
  (cond
    [(or (estado-terminal tablero) (= altura 6)) (eval tablero)]
    [else
     (for ([i '(0 1 2 3 4 5 6)])
       (cond
         [(verificar-columna tablero i)
          (set! beta (min beta (valor-max (jugada tablero 2 i) (add1 altura) alfa beta)))
          (cond [(>= alfa beta) alfa])]))
     beta]))

;Funcion que verifica si el tablero ya se encuentra lleno
(define (estado-terminal tablero)
  (not (list? (member 0 (list-ref tablero 0)))))

;Funcion que verifica si una ficha se puede insertar en una columna
(define (verificar-columna tablero columna)
  (cond
    [(= (list-ref (list-ref tablero 0) columna) 0) #t]
    [else #f]))

;Funcion eval de la heuristica del minimax
(define (eval tablero)
  (cond
    [(gano tablero 1) 10000]
    [(gano tablero 2) -10000]
    [else (- (costo tablero 1) (costo tablero 2))]))

;Funcion que cuenta las lineas posibles para ganar de un jugador particular
(define (costo tablero jugador)
  (define total 0)
  (for ([i '(5 4 3 2 1 0)])
    (for ([j '(0 1 2 3 4 5 6)])
      (cond
        [(and (> i 2) (< j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (- i 1)) j) jugador)
               (= (list-ref (list-ref tablero (- i 1)) j) 0))
              (or
               (= (list-ref (list-ref tablero (- i 2)) j) jugador)
               (= (list-ref (list-ref tablero (- i 2)) j) 0))
              (or
               (= (list-ref (list-ref tablero (- i 3)) j) jugador)
               (= (list-ref (list-ref tablero (- i 3)) j) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (- i 1)) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero (- i 1)) (+ j 1)) 0))
              (or
               (= (list-ref (list-ref tablero (- i 2)) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero (- i 2)) (+ j 2)) 0))
              (or
               (= (list-ref (list-ref tablero (- i 3)) (+ j 3)) jugador)
               (= (list-ref (list-ref tablero (- i 3)) (+ j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero i) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 1)) 0))
              (or
               (= (list-ref (list-ref tablero i) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 2)) 0))
              (or
               (= (list-ref (list-ref tablero i) (+ j 3)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 3)) 0)))
             (set! total (add1 total)) empty)]
        [(and (> i 2) (= j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero i) (- j 1)) jugador)
               (= (list-ref (list-ref tablero i) (- j 1)) 0))
              (or
               (= (list-ref (list-ref tablero i) (- j 2)) jugador)
               (= (list-ref (list-ref tablero i) (- j 2)) 0))
              (or
               (= (list-ref (list-ref tablero i) (- j 3)) jugador)
               (= (list-ref (list-ref tablero i) (- j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (- i 1)) (- j 1)) jugador)
               (= (list-ref (list-ref tablero (- i 1)) (- j 1)) 0))
              (or
               (= (list-ref (list-ref tablero (- i 2)) (- j 2)) jugador)
               (= (list-ref (list-ref tablero (- i 2)) (- j 2)) 0))
              (or
               (= (list-ref (list-ref tablero (- i 3)) (- j 3)) jugador)
               (= (list-ref (list-ref tablero (- i 3)) (- j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (- i 1)) j) jugador)
               (= (list-ref (list-ref tablero (- i 1)) j) 0))
              (or
               (= (list-ref (list-ref tablero (- i 2)) j) jugador)
               (= (list-ref (list-ref tablero (- i 2)) j) 0))
              (or
               (= (list-ref (list-ref tablero (- i 3)) j) jugador)
               (= (list-ref (list-ref tablero (- i 3)) j) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (- i 1)) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero (- i 1)) (+ j 1)) 0))
              (or
               (= (list-ref (list-ref tablero (- i 2)) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero (- i 2)) (+ j 2)) 0))
              (or
               (= (list-ref (list-ref tablero (- i 3)) (+ j 3)) jugador)
               (= (list-ref (list-ref tablero (- i 3)) (+ j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero i) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 1)) 0))
              (or
               (= (list-ref (list-ref tablero i) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 2)) 0))
              (or
               (= (list-ref (list-ref tablero i) (+ j 3)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 3)) 0)))
             (set! total (add1 total)) empty)]
        [(and (> i 2) (> j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero i) (- j 1)) jugador)
               (= (list-ref (list-ref tablero i) (- j 1)) 0))
              (or
               (= (list-ref (list-ref tablero i) (- j 2)) jugador)
               (= (list-ref (list-ref tablero i) (- j 2)) 0))
              (or
               (= (list-ref (list-ref tablero i) (- j 3)) jugador)
               (= (list-ref (list-ref tablero i) (- j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (- i 1)) (- j 1)) jugador)
               (= (list-ref (list-ref tablero (- i 1)) (- j 1)) 0))
              (or
               (= (list-ref (list-ref tablero (- i 2)) (- j 2)) jugador)
               (= (list-ref (list-ref tablero (- i 2)) (- j 2)) 0))
              (or
               (= (list-ref (list-ref tablero (- i 3)) (- j 3)) jugador)
               (= (list-ref (list-ref tablero (- i 3)) (- j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (- i 1)) j) jugador)
               (= (list-ref (list-ref tablero (- i 1)) j) 0))
              (or
               (= (list-ref (list-ref tablero (- i 2)) j) jugador)
               (= (list-ref (list-ref tablero (- i 2)) j) 0))
              (or
               (= (list-ref (list-ref tablero (- i 3)) j) jugador)
               (= (list-ref (list-ref tablero (- i 3)) j) 0)))
             (set! total (add1 total)) empty)]
        [(and (< i 3) (< j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (+ i 1)) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero (+ i 1)) (+ j 1)) 0))
              (or
               (= (list-ref (list-ref tablero (+ i 2)) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero (+ i 2)) (+ j 2)) 0))
              (or
               (= (list-ref (list-ref tablero (+ i 3)) (+ j 3)) jugador)
               (= (list-ref (list-ref tablero (+ i 3)) (+ j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero i) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 1)) 0))
              (or
               (= (list-ref (list-ref tablero i) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 2)) 0))
              (or
               (= (list-ref (list-ref tablero i) (+ j 3)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 3)) 0)))
             (set! total (add1 total)) empty)]
        [(and (< i 3) (= j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero i) (- j 1)) jugador)
               (= (list-ref (list-ref tablero i) (- j 1)) 0))
              (or
               (= (list-ref (list-ref tablero i) (- j 2)) jugador)
               (= (list-ref (list-ref tablero i) (- j 2)) 0))
              (or
               (= (list-ref (list-ref tablero i) (- j 3)) jugador)
               (= (list-ref (list-ref tablero i) (- j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (+ i 1)) (- j 1)) jugador)
               (= (list-ref (list-ref tablero (+ i 1)) (- j 1)) 0))
              (or
               (= (list-ref (list-ref tablero (+ i 2)) (- j 2)) jugador)
               (= (list-ref (list-ref tablero (+ i 2)) (- j 2)) 0))
              (or
               (= (list-ref (list-ref tablero (+ i 3)) (- j 3)) jugador)
               (= (list-ref (list-ref tablero (+ i 3)) (- j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (+ i 1)) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero (+ i 1)) (+ j 1)) 0))
              (or
               (= (list-ref (list-ref tablero (+ i 2)) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero (+ i 2)) (+ j 2)) 0))
              (or
               (= (list-ref (list-ref tablero (+ i 3)) (+ j 3)) jugador)
               (= (list-ref (list-ref tablero (+ i 3)) (+ j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero i) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 1)) 0))
              (or
               (= (list-ref (list-ref tablero i) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 2)) 0))
              (or
               (= (list-ref (list-ref tablero i) (+ j 3)) jugador)
               (= (list-ref (list-ref tablero i) (+ j 3)) 0)))
             (set! total (add1 total)) empty)]
        [(and (< i 3) (> j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero i) (- j 1)) jugador)
               (= (list-ref (list-ref tablero i) (- j 1)) 0))
              (or
               (= (list-ref (list-ref tablero i) (- j 2)) jugador)
               (= (list-ref (list-ref tablero i) (- j 2)) 0))
              (or
               (= (list-ref (list-ref tablero i) (- j 3)) jugador)
               (= (list-ref (list-ref tablero i) (- j 3)) 0)))
             (set! total (add1 total)) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (or
               (= (list-ref (list-ref tablero (+ i 1)) (- j 1)) jugador)
               (= (list-ref (list-ref tablero (+ i 1)) (- j 1)) 0))
              (or
               (= (list-ref (list-ref tablero (+ i 2)) (- j 2)) jugador)
               (= (list-ref (list-ref tablero (+ i 2)) (- j 2)) 0))
              (or
               (= (list-ref (list-ref tablero (+ i 3)) (- j 3)) jugador)
               (= (list-ref (list-ref tablero (+ i 3)) (- j 3)) 0)))
             (set! total (add1 total)) empty)])))
  total)


;Funcion que verifica si el jugador que le entra de parametro gano el juego
(define (gano tablero jugador)
  (define win #f)
  (for ([i '(5 4 3 2 1 0)])
    (for ([j '(0 1 2 3 4 5 6)])
      (cond
        [(and (> i 2) (< j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (- i 1)) j) jugador)
              (= (list-ref (list-ref tablero (- i 2)) j) jugador)
              (= (list-ref (list-ref tablero (- i 3)) j) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (- i 1)) (+ j 1)) jugador)
              (= (list-ref (list-ref tablero (- i 2)) (+ j 2)) jugador)
              (= (list-ref (list-ref tablero (- i 3)) (+ j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero i) (+ j 1)) jugador)
              (= (list-ref (list-ref tablero i) (+ j 2)) jugador)
              (= (list-ref (list-ref tablero i) (+ j 3)) jugador))
             (set! win #t) empty)]
        [(and (> i 2) (= j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero i) (- j 1)) jugador)
              (= (list-ref (list-ref tablero i) (- j 2)) jugador)
              (= (list-ref (list-ref tablero i) (- j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (- i 1)) (- j 1)) jugador)
              (= (list-ref (list-ref tablero (- i 2)) (- j 2)) jugador)
              (= (list-ref (list-ref tablero (- i 3)) (- j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (- i 1)) j) jugador)
              (= (list-ref (list-ref tablero (- i 2)) j) jugador)
              (= (list-ref (list-ref tablero (- i 3)) j) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
               (= (list-ref (list-ref tablero (- i 1)) (+ j 1)) jugador)
               (= (list-ref (list-ref tablero (- i 2)) (+ j 2)) jugador)
               (= (list-ref (list-ref tablero (- i 3)) (+ j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero i) (+ j 1)) jugador)
              (= (list-ref (list-ref tablero i) (+ j 2)) jugador)
              (= (list-ref (list-ref tablero i) (+ j 3)) jugador))
             (set! win #t) empty)]
        [(and (> i 2) (> j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero i) (- j 1)) jugador)
              (= (list-ref (list-ref tablero i) (- j 2)) jugador)
              (= (list-ref (list-ref tablero i) (- j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (- i 1)) (- j 1)) jugador)
              (= (list-ref (list-ref tablero (- i 2)) (- j 2)) jugador)
              (= (list-ref (list-ref tablero (- i 3)) (- j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (- i 1)) j) jugador)
              (= (list-ref (list-ref tablero (- i 2)) j) jugador)
              (= (list-ref (list-ref tablero (- i 3)) j) jugador))
             (set! win #t) empty)]
        [(and (< i 3) (< j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (+ i 1)) (+ j 1)) jugador)
              (= (list-ref (list-ref tablero (+ i 2)) (+ j 2)) jugador)
              (= (list-ref (list-ref tablero (+ i 3)) (+ j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero i) (+ j 1)) jugador)
              (= (list-ref (list-ref tablero i) (+ j 2)) jugador)
              (= (list-ref (list-ref tablero i) (+ j 3)) jugador))
             (set! win #t) empty)]
        [(and (< i 3) (= j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero i) (- j 1)) jugador)
              (= (list-ref (list-ref tablero i) (- j 2)) jugador)
              (= (list-ref (list-ref tablero i) (- j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (+ i 1)) (- j 1)) jugador)
              (= (list-ref (list-ref tablero (+ i 2)) (- j 2)) jugador)
              (= (list-ref (list-ref tablero (+ i 3)) (- j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (+ i 1)) (+ j 1)) jugador)
              (= (list-ref (list-ref tablero (+ i 2)) (+ j 2)) jugador)
              (= (list-ref (list-ref tablero (+ i 3)) (+ j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero i) (+ j 1)) jugador)
              (= (list-ref (list-ref tablero i) (+ j 2)) jugador)
              (= (list-ref (list-ref tablero i) (+ j 3)) jugador))
             (set! win #t) empty)]
        [(and (< i 3) (> j 3))
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero i) (- j 1)) jugador)
              (= (list-ref (list-ref tablero i) (- j 2)) jugador)
              (= (list-ref (list-ref tablero i) (- j 3)) jugador))
             (set! win #t) empty)
         (if (and
              (= (list-ref (list-ref tablero i) j) jugador)
              (= (list-ref (list-ref tablero (+ i 1)) (- j 1)) jugador)
              (= (list-ref (list-ref tablero (+ i 2)) (- j 2)) jugador)
              (= (list-ref (list-ref tablero (+ i 3)) (- j 3)) jugador))
             (set! win #t) empty)])))
  win)







 