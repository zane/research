(module queue2 scheme
        (provide is-queue-mt?
                 enqueue 
                 dequeue
                 peek-queue
                 print-queue)

        
        (require scheme/mpair)

        (define-struct queue (head tail master slave)
                       #:mutable)

        
        (define QUEUE (make-queue null null #f #f))

        (define (is-queue-mt?)
          (and (null? (queue-head QUEUE)) (null? (queue-tail QUEUE))))

        (define (dequeue)
          (cond ((null? (queue-head QUEUE)) (void))
                (else 
                  (begin 
                    (set-queue-slave! QUEUE #t)
                    (cond ((queue-master QUEUE)
                           (begin (set-queue-slave! QUEUE #f)
                                  (dequeue)))
                          (else
                            (let ((new-head (mcdr (queue-head QUEUE))))
                              (set-queue-head! QUEUE new-head)
                              (if (null? new-head)
                                (set-queue-tail! QUEUE new-head)
                                (void))
                                (set-queue-slave! QUEUE #f))))
                    ))))

        (define (enqueue closure)
          (set-queue-master! QUEUE  #t)
          (cond ((queue-slave QUEUE)
                 (begin
                   (set-queue-master! QUEUE #f)
                   (enqueue closure)))
                ((null? (queue-tail QUEUE))
                 (let ((closure-list (mlist closure)))
                   (set-queue-head! QUEUE closure-list)
                   (set-queue-tail! QUEUE closure-list)
                   (set-queue-master! QUEUE #f)))
                (else 
                  (let ((closure-list (mlist closure)))
                    (set-mcdr! (queue-tail QUEUE) closure-list)
                    (set-queue-tail! QUEUE closure-list)
                    (set-queue-master! QUEUE #f)))))

        (define (peek-queue)
          (cond ((null? (queue-head QUEUE)) 'empty)
                (else
                  (begin
                    (set-queue-slave! QUEUE #t)
                    (if (queue-master QUEUE)
                      (begin
                        (set-queue-slave! QUEUE #f)
                        (peek-queue))
                      (let ((closure (mcar (queue-head QUEUE))))
                        (set-queue-slave! QUEUE #f)
                        closure))))))

        (define (print-queue)
          (printf "queue:~a~n" QUEUE))
)


