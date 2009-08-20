(module queue3 scheme
        (provide is-queue-mt?
                 enqueue
                 dequeue
                 peek-queue)


        (require scheme/mpair)
       
        (define-struct node (content [done? #:mutable])) 

        (define-struct queue (head tail) 
                       #:mutable) 

        (define QUEUE  (make-queue null null))

        (define (is-queue-mt?)
          (or (null? (queue-tail QUEUE)) 
              (node-done? (mcar (queue-tail QUEUE)))))

        (define (dequeue)
          (cond ((null? (mcdr (queue-head QUEUE)))
                 (set-node-done?! (mcar (queue-head QUEUE)) #t))
                (else
                  (set-queue-head! QUEUE (mcdr (queue-head QUEUE))))))

        (define (enqueue closure)
          (let ((tail (queue-tail QUEUE)))
            (cond ((null? tail)
                   (let ((head-tail (mcons (make-node closure #f) null)))
                     (set-queue-head! QUEUE head-tail)
                     (set-queue-tail! QUEUE head-tail)))
                  (else
                    (begin
                      (set-mcdr! tail (mcons (make-node closure #f) null))
                      (set-queue-tail! QUEUE (mcdr tail)))))))

        (define (peek-queue)
          (cond ((null? (queue-head QUEUE)) 'empty)
                ((null? (mcdr (queue-head QUEUE)))
                 (if (node-done? (mcar (queue-head QUEUE)))
                   'emtpy
                   (node-content (mcar (queue-head QUEUE)))))
                (else 
                  (if (node-done? (mcar (queue-head QUEUE)))
                    (begin 
                      (set-queue-head! QUEUE (mcdr (queue-head QUEUE)))
                      'emtpy)
                    (node-content (mcar (queue-head QUEUE)))))))


)


