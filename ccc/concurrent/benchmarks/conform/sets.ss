(module sets scheme

;; SET OPERATIONS
; (representation as lists with distinct elements)

   (require "../../contract.ss")
   (require "./adjoin.ss")
   (require scheme/mpair)

(provide/contract
         [eliminate (-> any/c (future/c distinct?) any)])

(define (eliminate element set)
  (cond ((null? set) set)
	((eq? element (mcar set)) (mcdr set))
	(else (mcons (mcar set) (eliminate element (mcdr set))))))


(provide/contract
         [intersect (-> distinct? (future/c distinct?) any)])



(define (intersect list1 list2)
  (let loop ((l list1))
    (cond ((null? l) '())
	  ((mmemq (mcar l) list2) (mcons (mcar l) (loop (mcdr l))))
	  (else (loop (mcdr l))))))


(provide/contract
         [union (->  distinct? distinct? any)])


(define (union list1 list2)
  (if (null? list1)
      list2
      (union (mcdr list1)
	     (adjoin (mcar list1) list2))))
)


