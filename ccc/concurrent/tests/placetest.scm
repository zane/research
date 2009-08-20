#lang scheme

(define (test x)
  (let ((x (place (lambda () (+ 1 x)))))
        (place? x )))

(test 2)
