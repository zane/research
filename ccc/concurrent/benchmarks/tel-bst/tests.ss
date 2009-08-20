#lang scheme

  (require "../../contract.ss")

  (require "./telephone-catalogue-lib.ss")
  

  (define (loop i tc)
    (if (= i 0)
      'done
    (begin
      (search tc
              (create-telephone-entry
                 "aa"
                 "aa"
                 (list (cons "h" 1000000000))
                 (list (cons "u" "b@c.u"))))

      (add tc
              (create-telephone-entry
                 "aa"
                 "aa"
                 (list (cons "h" 1000000000))
                 (list (cons "u" "b@c.u"))))
      (loop (- i 1) tc))))


(time
 (master-start
   (lambda()
  (let 
    ((initial
       (build-list 1000 (lambda (i) 
                      (let ((name (list->string 
                                    (build-list (+ i 1) (lambda (j) #\a)))))
                        (create-telephone-entry 
                          name
                          name
                          (list (cons "h" 1000000000))
                          (list 
                            (cons "h"  "x@y.z"))))))))
 (loop 2000 (init initial)))))) 
