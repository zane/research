(module master-slave scheme
        (provide master-start)
        (provide enqueue)
        (provide slave-start)
        (provide ccdisplay)
        (provide ccnewline)
        (provide synchronize)

        (require "./queue3.scm")

       (define (synchronize thunk)
         (if (is-queue-mt?)
           (thunk)
           (synchronize thunk)))
        
       (define (ccdisplay arg)
         (if (is-queue-mt?)
           (display arg)
           (ccdisplay arg)))

       (define (ccnewline)
         (if (is-queue-mt?)
           (newline)
           (ccnewline)))



        (define (master-start thunk)
          (slave-start)
          (let ((result (thunk)))
            (master-loop result)))
      

        (define (master-loop result)
          (if (is-queue-mt?)
            result
            (begin
            (master-loop result))))

      (define (slave-start)
        (place slave-loop))

      (define slave-loop
          (lambda ()
            (let* ((thunk (peek-queue)))
              (cond ((symbol? thunk) (slave-loop))
                    (else
                      (begin
                        ;;(printf "new contract ~n")
                        (thunk)
                        ;;(printf "contract checked~n")
                        (dequeue)
                        (slave-loop)))))))
) 
