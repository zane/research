(module middle-man scheme
       (provide  is-queue-mt?
                 enqueue 
                 dequeue
                 peek-queue
                 middle-man-loop
                 )

        (require scheme/async-channel)

        (define-struct queue (channel 
                              [buffer-length #:mutable]
                              [pending-slave #:mutable]
                              [pending-master #:mutable]))
        ;; channel: channel
        ;; buffer-length: integer
        ;; pending: boolean

        (define (init-queue)
          (make-queue (make-async-channel #f) 0 #f #f))

        (define QUEUE (init-queue))

        (define (pending-slave?)
          (queue-pending-slave QUEUE))

        (define (set-pending-slave)
          (set-queue-pending-slave! QUEUE #t))

        (define (unset-pending-slave)
          (set-queue-pending-slave! QUEUE #f))

        (define (pending-master?)
          (queue-pending-master QUEUE))

        (define (set-pending-master)
          (set-queue-pending-master! QUEUE #t))

        (define (unset-pending-master)
          (set-queue-pending-master! QUEUE #f))

        
        (define (i-is-queue-mt?)
          (= (queue-buffer-length QUEUE) 0))

        (define (i-enqueue content)
          (async-channel-put (queue-channel QUEUE) content)
          (set-queue-buffer-length! QUEUE (+ (queue-buffer-length QUEUE) 1)))

        (define (i-dequeue)
          (set-queue-buffer-length! 
            QUEUE 
            (- (queue-buffer-length QUEUE) 1)))

        (define (i-peek-queue)
           (async-channel-get (queue-channel QUEUE)))


        (define input-channel  (make-async-channel #f))

        (define-struct e-msg (datum))
        ;; datum: any thunk

        ;; messages in input-channel --> action
        ;;   -- 'is-queue-mt? --> reply in master-channel #t/#f
        ;;   -- 'peek-queue --> reply in slave-channel 
        ;;   -- 'dequeue --> no reply, decrease of queue length counter
        ;;   -- (make e-msg value) --> no reply, queue update


        (define slave-channel  (make-channel))

        (define master-channel (make-channel))

        (define (is-queue-mt?)
          (async-channel-put input-channel 'is-queue-mt?)
          (channel-get master-channel))

       (define (peek-queue)
         (flush-output)
         (async-channel-put input-channel 'peek-queue)
         (channel-get slave-channel))
        
        (define (enqueue content)
          (async-channel-put input-channel (make-e-msg content)))

        (define (dequeue)
          (async-channel-put input-channel 'dequeue))
        
        (define (middle-man-body)
          (printf "entering middle-man body~n")
          (let ((request (async-channel-get input-channel)))
            (printf "middle-man: request=~a(length:~a,slaves pending:~a,masters pending:~a)~n" 
                    request (queue-buffer-length QUEUE) (queue-pending-slave QUEUE) (queue-pending-master QUEUE))
            (flush-output)
            (cond ((symbol? request)
                   (cond ((symbol=? request 'is-queue-mt?)
                          (if  (i-is-queue-mt?)
                            (channel-put master-channel #t)
                            (set-pending-master)))
                         ((symbol=? request 'peek-queue)
                          (if (i-is-queue-mt?)
                            (set-pending-slave)
                            (channel-put slave-channel (i-peek-queue))))
                         ((symbol=? request 'dequeue)
                          (begin 
                            (i-dequeue)
                            (if (and (pending-master?) (i-is-queue-mt?))
                              (begin
                                (channel-put master-channel #t)
                                (unset-pending-master))
                              (void))))))
                  (else
                    (if (pending-slave?)
                      (begin
                        (i-enqueue (e-msg-datum request))
                        (channel-put slave-channel (i-peek-queue))
                        (unset-pending-slave))
                      (i-enqueue (e-msg-datum request)))))))

        (define middle-man-loop
          (lambda ()
              (flush-output)
              (middle-man-body)
              (middle-man-loop)))
)
                          

                              


