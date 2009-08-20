;;; ==== rdg.ss ====

(module rdg scheme 

        (require "../../contract.ss")
        (require "./util.ss")
        (require "./ptfold.ss")
        (require "./minimal.ss")


; Fold over rooted directed graphs with bounded out-degree.
; Size is the number of verticies (including the root).  Max-out is the
; maximum out-degree for any vertex.  Folder is called via
;	(folder edges state)
; where edges is a list of length size.  The ith element of the list is
; a list of the verticies j for which there is an edge from i to j.
; The last vertex is the root.

(provide/contract [fold-over-rdg 
                    (->
                      integer?-and-exact?-and-positive?
                      integer?-and-exact?-and-positive?
                      procedure?
                      any/c
                      any)])


(define fold-over-rdg
    (lambda (size max-out folder state)
 (let* ((root
		    (- size 1))
		(edge?
		    (proc->vector size
			(lambda (from)
			    (make-vector size #f))))
		(edges
		    (make-vector size '()))
		(out-degrees
		    (make-vector size 0))
		(minimal-folder
		    (make-minimal? root))
		(non-root-minimal?
		    (let ((cont
				(lambda (perm state accross)
				    '(assert (eq? state #t)
					state)
				    (accross #t))))
			(lambda (size)
			    (minimal-folder size
				edge?
				cont
				#t))))
		(root-minimal?
		    (let ((cont
				(lambda (perm state accross)
				    '(assert (eq? state #t)
					state)
				    (case (cmp-next-vertex edge? perm root root)
					((less)
					    #f)
					((equal more)
					    (accross #t))
					;(else
					;    (assert #f))
                                        ))))
			(lambda ()
			    (minimal-folder root
				edge?
				cont
				#t)))))
	    (let _-*-
		((vertex
			0)
		    (state
			state))
		(cond ((not (non-root-minimal? vertex))
			state)
		    ((= vertex root)
			'(assert
			    (begin
				(gnatural-for-each root
				    (lambda (v)
					'(assert (= (vector-ref out-degrees v)
						(length (vector-ref edges v)))
					    v
					    (vector-ref out-degrees v)
					    (vector-ref edges v))))
				#t))
			(let ((reach?
				    (make-reach? root edges))
				(from-root
				    (vector-ref edge? root)))
			    (let _-*-
				((v
					0)
				    (outs
					0)
				    (efr
					'())
				    (efrr
					'())
				    (state
					state))
				(cond ((not (or (= v root)
						(= outs max-out)))
					(vector-set! from-root v #t)
					(let ((state
						    (_-*- (+ v 1)
							(+ outs 1)
							(cons v efr)
							(cons (vector-ref reach? v)
							    efrr)
							state)))
					    (vector-set! from-root v #f)
					    (_-*- (+ v 1)
						outs
						efr
						efrr
						state)))
				    ((and (natural-for-all? root
						(lambda (v)
						    (there-exists? efrr
							(lambda (r)
							    (vector-ref r v)))))
					    (root-minimal?))
					(vector-set! edges root efr)
					(folder
					    (proc->vector size
						(lambda (i)
						    (vector-ref edges i)))
					    state))
				    (else
					state)))))
		    (else
			(let ((from-vertex
				    (vector-ref edge? vertex)))
			    (let _-**-
				((sv
					0)
				    (outs
					0)
				    (state
					state))
				(if (= sv vertex)
				    (begin
					(vector-set! out-degrees vertex outs)
					(_-*- (+ vertex 1)
					    state))
				    (let* ((state
						; no sv->vertex, no vertex->sv
						(_-**- (+ sv 1)
						    outs
						    state))
					    (from-sv
						(vector-ref edge? sv))
					    (sv-out
						(vector-ref out-degrees sv))
					    (state
						(if (= sv-out max-out)
						    state
						    (begin
							(vector-set! edges
							    sv
							    (cons vertex
								(vector-ref edges sv)))
							(vector-set! from-sv vertex #t)
							(vector-set! out-degrees sv (+ sv-out 1))
							(let* ((state
								    ; sv->vertex, no vertex->sv
								    (_-**- (+ sv 1)
									outs
									state))
								(state
								    (if (= outs max-out)
									state
									(begin
									    (vector-set! from-vertex sv #t)
									    (vector-set! edges
										vertex
										(cons sv
										    (vector-ref edges vertex)))
									    (let ((state
											; sv->vertex, vertex->sv
											(_-**- (+ sv 1)
											    (+ outs 1)
											    state)))
										(vector-set! edges
										    vertex
										    (cdr (vector-ref edges vertex)))
										(vector-set! from-vertex sv #f)
										state)))))
							    (vector-set! out-degrees sv sv-out)
							    (vector-set! from-sv vertex #f)
							    (vector-set! edges
								sv
								(cdr (vector-ref edges sv)))
							    state)))))
					(if (= outs max-out)
					    state
					    (begin
						(vector-set! edges
						    vertex
						    (cons sv
							(vector-ref edges vertex)))
						(vector-set! from-vertex sv #t)
						(let ((state
							    ; no sv->vertex, vertex->sv
							    (_-**- (+ sv 1)
								(+ outs 1)
								state)))
						    (vector-set! from-vertex sv #f)
						    (vector-set! edges
							vertex
							(cdr (vector-ref edges vertex)))
						    state)))))))))))))

; Given a vector which maps vertex to out-going-edge list,
; return a vector  which gives reachability.

(provide/contract [make-reach? 
                    (->
                      integer?-and-exact?-and-positive?
                      (future/c vector?)
                      (future/c vector?))])


(define make-reach?
    (lambda (size vertex->out)
	(let ((res
		    (proc->vector size
			(lambda (v)
			    (let ((from-v
					(make-vector size #f)))
				(vector-set! from-v v #t)
				(for-each
				    (lambda (x)
					(vector-set! from-v x #t))
				    (vector-ref vertex->out v))
				from-v)))))
	    (gnatural-for-each size
		(lambda (m)
		    (let ((from-m
				(vector-ref res m)))
			(gnatural-for-each size
			    (lambda (f)
				(let ((from-f
					    (vector-ref res f)))
				    (if (vector-ref from-f m); [wdc - was when]
              (begin
                (gnatural-for-each size
                                   (lambda (t)
                                     (if (vector-ref from-m t)
                                       (begin ; [wdc - was when]
                                         (vector-set! from-f t #t))
                                       '()))))
              '())))))))
	    res)))

)


