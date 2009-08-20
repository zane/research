#lang scheme

(define (thunk x)
  (lambda () (if (= x 15)
               (begin
                 (printf "exiting...~n")
                 (flush-output)
                 (exit))
               '())))




(build-list 2 (lambda (x) (lambda () '())))



