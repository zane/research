(module incr scheme

        (require "../../contract.ss")
        (require "./smin.ss")

        (provide incr)
        
        ;;(provide/contract 
        ;;  (incr (->   
        ;;                (lambda (x)
        ;;                  (and
        ;;                    (andmap (lambda (x)
        ;;                              (and (number? x)
        ;;                                   (> x 0)))
        ;;                            x)
        ;;                    (is-sorted x <))) 
        ;;             any)))

        (define (incr l)
          (let ((pivot (smin l)))
            (map (lambda (x) (+ pivot x)) l)))
        )
