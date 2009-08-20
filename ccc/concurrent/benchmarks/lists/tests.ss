#lang scheme


  (require "../../contract.ss")
  (require "./incr.ss")



  (let ((sample (build-list 5000000 (lambda (x) (+ x 1)))))
   (time (master-start (lambda () (incr sample))))
   (void))

