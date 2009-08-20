#lang scheme

(require "../../contract.ss")
(require "./create-n.ss")
(require "./iterative-div2.ss")
(require "./recursive-div2.ss")

(define *ll* (create-n 1000))
 
(define (test-1 l)
  (do ((i 3000 (- i 1)))
      ((= i 0))
     (iterative-div2 l)
     (iterative-div2 l)
     (iterative-div2 l)
     (iterative-div2 l)))
 
(define (test-2 l)
  (do ((i 3000 (- i 1)))
      ((= i 0))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))
 
;;; for the iterative test call: (test-1 *ll*)
;;; for the recursive test call: (test-2 *ll*)
 
(time
  (master-start
    (lambda ()
  (begin
    ;(place (lambda () #t))
    (let loop ((n 10) (v 0))
      (if (zero? n)
       v
        (loop (- n 1)
            (cons
              (test-1 *ll*)
              (test-2 *ll*)))))))))
