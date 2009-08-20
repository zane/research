#lang scheme

  (require "../../contract.ss")

  (require "./telephone-catalogue-lib.ss")
  

  (define (loop i tc)
    (if (= i 0)
      (begin
        (search tc
              (create-telephone-entry
                 "aa"
                 "aa"
                 (list (cons "h" 1000000000))
                 (list (cons "u" "b@c.u"))))
        (telephone-catalogue->string tc)
        (void))
      (begin 
         (remove
           (add (init tc)
               (create-telephone-entry
                 "aa"
                 "aa"
                 (list (cons "h" 1000000000))
                 (list (cons "u" "b@c.u"))))
            (create-spine-telephone-entry "aa" "aa"))
          (loop (- i 1) tc))))


(time
  (master-start 
    (lambda ()
  (let 
    ((initial
       (build-list 100 (lambda (i) 
                      (let ((name (list->string 
                                    (build-list (+ i 1) (lambda (j) #\a)))))
                        (create-telephone-entry 
                          "a"
                          "a"
                          (list (cons "h" 1000000000))
                          (list 
                            (cons "h"  "x@y.z"))))))))
 (loop 2000 initial)))))  
