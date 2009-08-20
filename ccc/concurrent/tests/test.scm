#lang scheme

(define (count-down n)
  (if (= n 0)
    42
    (count-down (- n 1))))


(define (is-prime-stupid x)
   (letrec ((is-prime-stupid-aux
              (lambda (x y)
                (if (= y 1)
                   #t
                   (if (= (modulo x y) 0)
                     #f 
                    (is-prime-stupid-aux x (- y 1)))))))
    (is-prime-stupid-aux x (- x 1))))

(define (next-prime-stupid x)
    (if (is-prime-stupid (+ x 1))
      (+ x 1)
      (next-prime-stupid (+ x 1))))


(define (is-odd x)
 (not (= (modulo x 2) 0)))

(define (prime-test-seq x times)
    (if (is-odd x)
      (let ((result (is-prime-stupid x)))
        (if (is-odd x)
          (if  (= times 1) 
            result
            (prime-test-seq  x  (- times 1)))
          (begin
            (printf "contract violation:next-prime-stupid~n")
            (exit))))
      (begin
        (printf "contract violation:prime-test-seq~n")
        (exit))))

(define (test from to)
  (printf "iter: ~a~n" from)
  (time (prime-test-seq 15485863 from))
  (flush-output)
  (if (= from to)
    (void)
    (test (+ from 1) to)))

(test 1 30)


                    
