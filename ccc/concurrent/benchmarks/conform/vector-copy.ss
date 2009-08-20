
(module vector-copy scheme

        (require "../../contract.ss")

        (provide vector-copy)


(define (vector-copy v)
  (let* ((length (vector-length v))
	 (result (make-vector length)))
    (let loop ((n 0))
      (vector-set! result n (vector-ref v n))
      (if (= n length)
	  v
	  (loop (+ n 1))))))

)
