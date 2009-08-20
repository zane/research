  (module queue6 typed-scheme
       
        (require/typed scheme/mpair 
                      [mcar      ((Listof node) -> Void)]
                      [mcdr      ((Listof node) -> Void)]
                      [mcons     ((node (Listof node)) -> (Listof node))]
                      [set-mcdr! ((Listof node) (Listof node) -> Void)])

        (define-struct: node ([content : ( -> Any)] [done? : Boolean])
                        #:mutable)                                      ;; a node contains the thunk content that 
                                                                        ;; returns any kind of scheme values (the
                                                                        ;; closure of a contract projection)and the 
                                                                        ;; boolean flag done?
                                                                        ;; the done? flag is #t if the node has been
                                                                        ;; already visited

        (define-struct: queue ([head : (Listof node)] [tail : (Listof node)])
                       #:mutable)                                       ;; a queue is a list of nodes with  mutable 
                                                                        ;; pointers at the first and last element

        (define QUEUE  (make-queue null null))                          ;; initially the queue does not contain any
                                                                        ;; nodes 
                                                                        ;; this holds only before the first enqueue

        (: is-queue-mt? (  -> Boolean))
        (define (is-queue-mt?)                                          ;; a queue is empty
          (or (null? (queue-tail QUEUE))                                ;; if it has no nodes
              (node-done? (mcar (queue-tail QUEUE)))))                  ;; or it only has a sentinal node

        (: dequeue (  -> Void))
        (define (dequeue)                                               ;; the tail node is removed if the queue has 
          (cond ((null? (mcdr (queue-head QUEUE)))                      ;; more than one nodes   
                 (set-node-done?! (mcar (queue-head QUEUE)) #t))        ;; if the queue has only one node then the node 
                (else                                                   ;; is marked as visited but it is not removed
                 (set-queue-head! QUEUE (mcdr (queue-head QUEUE))))))

        (: enqueue ((  -> Any) -> Void))
        (define (enqueue closure)                                       ;; new unvisited nodes are added to the tail
          (let ((tail (queue-tail QUEUE)))
            (cond ((null? tail)
                   (let ((head-tail (mcons (make-node closure #f) null)))
                     (set-queue-head! QUEUE head-tail)
                     (set-queue-tail! QUEUE head-tail)))
                  (else
                    (begin
                      (set-mcdr! tail (mcons (make-node closure #f) null))
                      (set-queue-tail! QUEUE (mcdr tail)))))))

        (: peek-queue (  -> (U Symbol (  -> Any))))
        (define (peek-queue)                                            ;; the contents of the unvisited head node  
          (cond ((null? (queue-head QUEUE)) 'empty)                     ;; are returned but the node is not removed 
                ((null? (mcdr (queue-head QUEUE)))                      ;; from the queue
                 (if (node-done? (mcar (queue-head QUEUE)))
                   'emtpy
                   (node-content (mcar (queue-head QUEUE)))))
                (else 
                  (if (node-done? (mcar (queue-head QUEUE)))            ;; enqueue may push a visited node at the 
                    (begin                                              ;; head position 
                      (set-queue-head! QUEUE (mcdr (queue-head QUEUE))) ;; the visited node is removed if the queue 
                      'emtpy)                                           ;; has more than one nodes 
                    (node-content (mcar (queue-head QUEUE)))))))

               
  )

