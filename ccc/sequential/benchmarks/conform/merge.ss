(module merge scheme

        (require "../../contract.ss")
        (require scheme/mpair)

        (provide merge)
        
        ;;(provide/contract 
        ;;  [merge (->d ([one any/c] 
        ;;              [two (lambda (arg) 
        ;;                     (or (= (mlength arg) (mlength one))
        ;;                         (=  (mlength arg)  (+ 1 (mlength one)))))] 
        ;;              [pred any/c])
        ;;             ()
        ;;             [result any/c])])


  (define (merge one two pred)
    (cond ((null? one) two)
          ((pred (mcar two) (mcar one))
           (mcons (mcar two) (merge (mcdr two) one pred)))
          (else (mcons (mcar one) (merge (mcdr one) two pred)))))


)

