#lang racket

(provide minimax)
(provide lista)
(provide gano)

(define (lista gameBoard)
  (define milista '())
  (for ([i '(0 1 2 3 4 5)])
    (set! milista (append milista (list (vector->list (vector-ref gameBoard i))))))
  milista)


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

(define (minimax tablero maximo columna)
  (cond
    [(> columna 6) mejor-columna]
    [else (set! maximo-actual (valor-min (jugada tablero 1 columna) 1 -10000 10000))
          (cond
            [(> maximo-actual maximo)
             (set! maximo maximo-actual)
             (set! mejor-columna columna)
             (minimax tablero maximo (add1 columna))]
            [else (minimax tablero maximo (add1 columna))])]))

(define (valor-max tablero altura alfa beta)
  (cond
    [(or (estado-terminal tablero) (= altura 6)) (eval tablero)]
    [else
     (for ([i '(0 1 2 3 4 5 6)])
       (set! alfa (max alfa (valor-min (jugada tablero 1 i) (add1 altura) alfa beta)))
       (cond [(>= alfa beta) beta]))
     alfa]))

(define (valor-min tablero altura alfa beta)
  (cond
    [(or (estado-terminal tablero) (= altura 6)) (eval tablero)]
    [else
     (for ([i '(0 1 2 3 4 5 6)])
       (set! beta (min beta (valor-max (jugada tablero 2 i) (add1 altura) alfa beta)))
       (cond [(>= alfa beta) alfa]))
     beta]))

(define (estado-terminal tablero)
  (not (list? (member 0 (list-ref tablero 0)))))

(define (eval tablero)
  (cond
    [(gano tablero 1) 10000]
    [(gano tablero 2) -10000]
    [else (- (costo tablero 1) (costo tablero 2))]))

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







 