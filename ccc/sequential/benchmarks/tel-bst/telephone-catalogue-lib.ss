(module telephone-catalogue-lib scheme
         
        (require "../../contract.ss")
        
        (require "./telephone-catalogue.ss")

        (provide create-telephone-entry
                 create-spine-telephone-entry
                 init)
  


       (define (create-spine-telephone-entry last-name first-name)
         (create-telephone-entry last-name first-name null null)) 


        (define (init tel)
          (cond ((null? tel) null) 
                (else (add (init (cdr tel)) (car tel)))))

        
        
        (provide/contract
          (add (-> 
                 valid-telephone-catalogue? 
                 any/c
                 any)))

        (define (add tel te)
                 (let ((tmp-catalogue
                  (cond ((null? tel) (make-telephone-entry-node null te null))
                        (else
                          (cond ((gtq-telephone-entry (telephone-entry-node-entry tel) te)
                                 (make-telephone-entry-node
                                   (telephone-entry-node-left tel)
                                   (telephone-entry-node-entry tel)
                                   (add (telephone-entry-node-right tel) te)))
                                (else
                                  (make-telephone-entry-node
                                    (add (telephone-entry-node-left tel) te)
                                    (telephone-entry-node-entry tel)
                                    (telephone-entry-node-right tel)
                                    )))))))
            (cond
              ((= (telephone-entry-node-factor tmp-catalogue) 2)
               (cond ((= (telephone-entry-node-factor (telephone-entry-node-right tmp-catalogue)) 1)
                     (left-rotation tmp-catalogue))
                    ((= (telephone-entry-node-factor (telephone-entry-node-right tmp-catalogue)) -1)
                     (left-rotation
                       (make-telephone-entry-node
                         (telephone-entry-node-entry tmp-catalogue)
                         (telephone-entry-node-left tmp-catalogue)
                         (right-rotation (telephone-entry-node-right tmp-catalogue)))))))
              ((= (telephone-entry-node-factor tmp-catalogue) -2)
               (cond ((= (telephone-entry-node-factor (telephone-entry-node-left tmp-catalogue)) 1)
                       (right-rotation
                        (make-telephone-entry-node
                          (telephone-entry-node-left tmp-catalogue)
                          (telephone-entry-node-entry tmp-catalogue)
                          (left-rotation (telephone-entry-node-left tmp-catalogue)))))
                     ((= (telephone-entry-node-factor (telephone-entry-node-left tmp-catalogue)) -1)
                      (right-rotation tmp-catalogue))))
              (else tmp-catalogue))))






   
        (provide/contract
              (search (-> 
                   valid-telephone-catalogue? 
                   any/c
                   any)))

        (define (search tel te)
          (cond ((null? tel) 'unknown)
                (else
                 (cond
                   ((eq-telephone-entry (telephone-entry-node-entry tel) te) te)
                   ((gtq-telephone-entry (telephone-entry-node-entry tel) te)
                    (search (telephone-entry-node-right tel) te))
                   (else
                    (search (telephone-entry-node-left tel) te))))))


   
)
