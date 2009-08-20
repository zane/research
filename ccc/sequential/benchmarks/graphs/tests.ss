; Modified 2 March 1997 by Will Clinger to add graphs-benchmark
; and to expand the four macros below.
; Modified 11 June 1997 by Will Clinger to eliminate assertions
; and to replace a use of "recur" with a named let.
;
; Performance note: (graphs-benchmark 7) allocates
;   34509143 pairs
;     389625 vectors with 2551590 elements
;   56653504 closures (not counting top level and known procedures)

; End of new code.

#lang scheme

(require "../../contract.ss")
(require "./util.ss")
(require "./ptfold.ss")
(require "./minimal.ss")
(require "./rdg.ss")

;;; ==== test input ====

;; Produces all directed graphs with N vertices, distinguished root,
;; and out-degree bounded by 2, upto isomprphism (there are 44).

  (time
    (let loop ((n 3) (v 0))
     (if (zero? n)
         'done
         (loop (- n 1)
               (fold-over-rdg 6
                              2 
                              cons
                              '())))))
