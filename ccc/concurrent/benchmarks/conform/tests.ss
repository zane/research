#lang scheme

 (require scheme/mpair)
 (require "../../contract.ss")
 (require "./conform.ss")



;; DEBUG and TEST

(define a null)
(define b null)
(define c null)
(define d null)

(define (reset)
  (set! a (make-node 'a null))
  (set! b (make-node 'b null))
  (set-blue-edges! a (mlist (make-blue-edge 'phi any-node b)))
  (set-blue-edges! b (mlist (make-blue-edge 'phi any-node a)
			   (make-blue-edge 'theta any-node b)))
  (set! c (make-node "c" null))
  (set! d (make-node "d" null))
  (set-blue-edges! c (mlist (make-blue-edge 'theta any-node b)))
  (set-blue-edges! d (mlist (make-blue-edge 'phi any-node c)
			   (make-blue-edge 'theta any-node d)))
  '(made a b c d))

(define (test)
  (reset)
  (mmap name
       (graph-nodes
	(make-lattice (make-graph (mlist a b c d any-node none-node)) #t))))

(define (go)
  (reset)
  (let ((result (mlist "(((b v d) ^ a) v c)"
		  "(c ^ d)"
		  "(b v (a ^ d))"
		  "((a v d) ^ b)"
		  "(b v d)"
		  "(b ^ (a v c))"
		  "(a v (c ^ d))"
		  "((b v d) ^ a)"
		  "(c v (a v d))"
		  "(a v c)"
		  "(d v (b ^ (a v c)))"
		  "(d ^ (a v c))"
		  "((a ^ d) v c)"
		  "((a ^ b) v d)"
		  "(((a v d) ^ b) v (a ^ d))"
		  "(b ^ d)"
		  "(b v (a v d))"
		  "(a ^ c)"
		  "(b ^ (c v d))"
		  "(a ^ b)"
		  "(a v b)"
		  "((a ^ d) ^ b)"
		  "(a ^ d)"
		  "(a v d)"
		  "d"
		  "(c v d)"
		  "a"
		  "b"
		  "c"
		  "any"
		  "none")))

    (if (equal? (test) result)
	(ccdisplay " ok.")
	(ccdisplay " um."))
    (ccnewline)))

(time 
  (master-start
    (lambda ()
  (let loop ((n 10))
        (if (zero? n)
            'done
            (begin
              (go)
              (loop (- n 1))))))))



