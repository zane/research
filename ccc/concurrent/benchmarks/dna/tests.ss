#lang scheme
  
 (require "../../contract.ss")

 (require "./life-circle.ss")

(time
  (master-start
    (lambda ()
       (let ((virus (build-list 49 (lambda (i) 
                          (make-virus
                            (build-list 200 (lambda (j) (random-dna-base j)))
                            (build-list 200 (lambda (j) (random-m-dna-base 
                                                          (+ j 1))))))))
       (cells (build-list 200 (lambda (i)
                           (let ((dna-chain
                                   (build-list 500 (lambda (j) 
                                                     (random-dna-base j)))))
                             (make-cell dna-chain (complement-chain dna-chain)))))))
   (map (lambda (x) (map (lambda (y) (mutate x y)) virus)) cells)
   (void)
   ))))

