#lang racket

(define matriz (list (list 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0)
                     (list 0 0 0 0 0 0 0)
                     (list 0 0 0 2 0 0 0)))


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
    [else (set! maximo-actual (valor-max tablero 1 -10000 10000 columna))
          (cond
            [(> maximo-actual maximo)
             (set! maximo maximo-actual)
             (set! mejor-columna columna)
             (minimax tablero maximo (add1 columna))]
            [else (minimax tablero maximo (add1 columna))])]))

(define (valor-max tablero altura alfa beta columna)
  (cond
    [(or (estado-terminal tablero) (= altura 16)) (eval tablero)]
    [else
     (cond
       [(= columna 6)
        (set! alfa (max alfa (valor-min (jugada tablero 1 columna) (add1 altura) alfa beta 0)))]
       [(< columna 6)
        (set! alfa (max alfa (valor-min (jugada tablero 1 columna) (add1 altura) alfa beta (add1 columna))))]
       [(>= alfa beta) beta])
     alfa]))

(define (valor-min tablero altura alfa beta columna)
  (cond
    [(or (estado-terminal tablero) (= altura 16)) (eval tablero)]
    [else
     (cond
       [(= columna 6)
        (set! beta (min beta (valor-max (jugada tablero 2 columna) (add1 altura) alfa beta 0)))]
       [(< columna 6)
        (set! beta (min beta (valor-max (jugada tablero 2 columna) (add1 altura) alfa beta (add1 columna))))]
       [(>= alfa beta) alfa])
     beta]))

(define (estado-terminal tablero)
  (not (list? (member 0 (list-ref tablero 0)))))

(define (eval tablero)
  (- (costo tablero 1) (costo tablero 2)))
  

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







 