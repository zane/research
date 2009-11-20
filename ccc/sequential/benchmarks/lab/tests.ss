#lang scheme
  
 (require "../../contract.ss")

 (require "./life-circle.ss")

(time
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
   (let* ((cells*  (map (lambda (x) (map (lambda (y) (mutate x y)) virus)) cells))
          (cells1  (flatten (map reproduce cells)))
          (cells2  (flatten (map reproduce cells1)))
          (cells3  (append cells2 (flatten (map (lambda (x)
                                                  (map (lambda (y) 
                                                         (x-mutate x y)) 
                                                       virus)) 
                                                cells2))))
          (cells4 (map die cells3))
          (cells5 (map (lambda (x) 
                         (map (lambda (y) (x-mutate x y)) virus)) cells4)))
     (void)
   )))
