(module create-n scheme

        (require "../../contract.ss")

        (provide create-n)

        (define ispositive
          (lambda (n)
            (and (number? n) (>= n 0))))

        (define length-n
          (lambda (n)
            (lambda (l)
              (and (list? l) (= (length l) n)))))
        
        (define (create-n n)
          (do ((n n (- n 1))
               (a '() (cons '() a)))
            ((= n 0) a)))

        
)

