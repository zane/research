#lang scheme

(require "../private/queue3.scm")

  
  (define slave-loop 
    (lambda ()
      (printf "slave~n")
      (printf "peeking ~a~n" (peek-queue))
      (print-queue)
      (dequeue)
      (print-queue)
      (slave-loop)))


    (define master-loop 
     (lambda (i limit)
      (if (= i limit)
        (if (is-queue-mt?)
          (void)
          (master-loop i limit))
        (begin
          (printf "master~n")
          (enqueue i)
          (print-queue)
          (master-loop (+ i 1) limit)))))



  (place slave-loop)
  (sleep 5)
  (master-loop 0 100)

