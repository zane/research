(module adjoin scheme
   

       (require "../../contract.ss")
       (require scheme/mpair)

       (provide distinct?)
       
       (define distinct? 
          (lambda (set)
            (cond ((null? set) #t)
                  (else 
                    (and (not (mmemq (mcar set) (mcdr set)))
                         (distinct? (mcdr set)))))))

       
       (provide/contract
         [adjoin (-> any/c distinct? any)])

       
       (define (adjoin element set)
         (if (mmemq element set) set (mcons element set)))
)


