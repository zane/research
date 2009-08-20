#lang scheme

 (require "./future-prelude.ss")
 (require "../contract.ss") 

 (master-start (lambda () (f (lambda (x) x)))) 
