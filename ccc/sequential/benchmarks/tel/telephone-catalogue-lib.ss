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

        
        (provide add)
        
       ;; (provide/contract
       ;;   (add (-> 
       ;;          valid-telephone-catalogue? 
       ;;          any/c
       ;;          any)))

        (define (add tel te)
          (cond ((null? tel) (list te)) 
                (else 
                  (cond ((gtq-telephone-entry (car tel) te)
                         (cons (car tel) (add (cdr tel) te)))
                        (else (cons te tel))))))


       (provide remove)

      ;;  (provide/contract
      ;;    (remove (-> 
      ;;             valid-telephone-catalogue? 
      ;;            any/c
      ;;             any)))

         (define (remove tel te)
          (cond ((null? tel) (list te)) 
                (else 
                  (cond ((gtq-telephone-entry (car tel) te)
                         (cons (car tel) (remove (cdr tel) te)))
                        ((eq-telephone-entry (car tel) te)
                         (cdr tel))
                        (else tel)))))


       (provide search)

       ;;  (provide/contract
       ;;    (search (-> 
       ;;            valid-telephone-catalogue? 
       ;;            any/c
       ;;            any)))

        (define (search tel te)
          (cond ((null? tel) (list te)) 
                (else 
                  (cond ((gtq-telephone-entry (car tel) te)
                         (search (cdr tel) te))
                        ((eq-telephone-entry (car tel) te)
                         (telephone-entry->string (car tel)))
                        (else 'unknown)))))


       (provide telephone-catalogue->string)


       ;;  (provide/contract
       ;;    (telephone-catalogue->string (-> 
       ;;                                   valid-telephone-catalogue? 
       ;;                                   any)))

         (define (telephone-catalogue->string tc)
            (reverse tc)
            (string-append
              "Telephone catalogue:~n"
              (foldr string-append 
                     ""
                     (map (lambda (te) (telephone-entry->string te)) tc))))

)
