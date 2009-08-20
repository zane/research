(module queue scheme
        (provide is-queue-mt?
                 enqueue 
                 dequeue
                 peek-queue
                 print-queue)

        (require scheme/mpair)
       
        (define-struct node (content [done? #:mutable])) 

        (define QUEUE  null)

        (define (is-queue-mt?)
          (or (null? QUEUE) (node-done? (mcar QUEUE))))

        (define (dequeue)
          (letrec ((dequeue-aux
                     (lambda (pointer)
                       (cond ((null? pointer) (void))
                             ((null? (mcdr pointer))
                              (set-node-done?! (mcar pointer) #t))
                             ((node-done? (mcar (mcdr pointer)))
                              (begin
                                (set-node-done?! (mcar pointer) #t)
                                (set-mcdr! pointer null)))
                             (else (dequeue-aux (mcdr pointer)))))))
            (dequeue-aux QUEUE)))

        (define (enqueue closure)
          (set! QUEUE (mcons (make-node closure #f) QUEUE)))

        (define (peek-queue)
          (letrec ((peek-queue-aux
                     (lambda (pointer)
                       (cond ((null? pointer) 'empty)
                             ((null? (mcdr pointer))
                              (let ((current-node (mcar pointer)))
                                (if (node-done? current-node)
                                  'empty
                                  (node-content current-node))))
                             ((node-done? (mcar (mcdr pointer))) 
                              (node-content (mcar pointer)))
                             (else (peek-queue-aux (mcdr pointer)))))))
            (peek-queue-aux QUEUE)))

        (define (print-queue)
          (printf "queue:~a~n" QUEUE))
)


