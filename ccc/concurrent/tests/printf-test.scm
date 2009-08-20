#lang scheme



(define (f i)
  (if (= i 0)
    '()
    (begin
    (printf "Startin place:~a~n" i)
    (f (- i 1)))))


(f 200)
