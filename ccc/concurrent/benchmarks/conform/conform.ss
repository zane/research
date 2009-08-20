(module conform scheme

 (require scheme/mpair)       

 (require "../../contract.ss")
 (require "./sort.ss")
 (require "./adjoin.ss")
 (require "./sets.ss")
 (require "./tables.ss")


 (provide (all-defined-out))

(define make-internal-node vector)
(define (internal-node-name node) (vector-ref node 0))
(define (internal-node-green-edges node) (vector-ref node 1))
(define (internal-node-red-edges node) (vector-ref node 2))
(define (internal-node-blue-edges node) (vector-ref node 3))
(define (set-internal-node-name! node name) (vector-set! node 0 name))
(define (set-internal-node-green-edges! node edges) (vector-set! node 1 edges))
(define (set-internal-node-red-edges! node edges) (vector-set! node 2 edges))
(define (set-internal-node-blue-edges! node edges) (vector-set! node 3 edges))


(define (make-node name  blue-edges)	; User's constructor
  (let ((name (if (symbol? name) (symbol->string name) name))
        (blue-edges (if (null? blue-edges) 'NOT-A-NODE-YET (mcar blue-edges))))
    (make-internal-node name null null blue-edges)))

(define (copy-node node)
  (make-internal-node (name node) null null (blue-edges node)))

; Selectors

(define name internal-node-name)
(define (make-edge-getter selector)
  (lambda (node)
    (if (or (none-node? node) (any-node? node))
	(error "Can't get edges from the ANY or NONE nodes")
	(selector node))))
(define red-edges (make-edge-getter internal-node-red-edges))
(define green-edges (make-edge-getter internal-node-green-edges))
(define blue-edges (make-edge-getter internal-node-blue-edges))

; Mutators

(define (make-edge-setter mutator!)
  (lambda (node value)
    (cond ((any-node? node) (error "Can't set edges from the ANY node"))
	  ((none-node? node) 'OK)
	  (else (mutator! node value)))))
(define set-red-edges! (make-edge-setter set-internal-node-red-edges!))
(define set-green-edges! (make-edge-setter set-internal-node-green-edges!))
(define set-blue-edges! (make-edge-setter set-internal-node-blue-edges!))

(define make-blue-edge vector)
(define (blue-edge-operation edge) (vector-ref edge 0))
(define (blue-edge-arg-node edge) (vector-ref edge 1))
(define (blue-edge-res-node edge) (vector-ref edge 2))
(define (set-blue-edge-operation! edge value) (vector-set! edge 0 value))
(define (set-blue-edge-arg-node! edge value) (vector-set! edge 1 value))
(define (set-blue-edge-res-node! edge value) (vector-set! edge 2 value))


; Selectors
(define operation blue-edge-operation)
(define arg-node blue-edge-arg-node)
(define res-node blue-edge-res-node)

; Mutators
(define set-arg-node! set-blue-edge-arg-node!)
(define set-res-node! set-blue-edge-res-node!)

; Higher level operations on blue edges

(define (lookup-op op node)
  (let loop ((edges (blue-edges node)))
    (cond ((null? edges) null)
	  ((eq? op (operation (mcar edges))) (mcar edges))
	  (else (loop (mcdr edges))))))

(define (has-op? op node)
  (not (null? (lookup-op op node))))

;; GRAPHS

(define make-internal-graph vector)
(define (internal-graph-nodes graph) (vector-ref graph 0))
(define (internal-graph-already-met graph) (vector-ref graph 1))
(define (internal-graph-already-joined graph) (vector-ref graph 2))
(define (set-internal-graph-nodes! graph nodes) (vector-set! graph 0 nodes))


; Constructor

(define (make-graph nodes)
  (make-internal-graph nodes (make-empty-table) (make-empty-table)))

; Selectors

(define graph-nodes internal-graph-nodes)
(define already-met internal-graph-already-met)
(define already-joined internal-graph-already-joined)

; Higher level functions on graphs

(define (add-graph-nodes! graph nodes)
  (set-internal-graph-nodes! graph (mcons nodes (graph-nodes graph))))

(define (copy-graph g)
  (define (copy-list l) (list->mlist (mlist->list l)))
  (make-internal-graph
   (copy-list (graph-nodes g))
   (already-met g)
   (already-joined g)))

(define (clean-graph g)
  (define (clean-node node)
    (if (not (or (any-node? node) (none-node? node)))
	(begin
	  (set-green-edges! node null)
	  (set-red-edges! node null)) '()))
  (mfor-each clean-node (graph-nodes g))
  g)

(define (canonicalize-graph graph classes)
  (define (fix node)
    (define (fix-set object selector mutator)
      (mutator object 
	       (mmap (lambda (node)
		      (find-canonical-representative node classes))
		    (selector object))))
    (if (not (or (none-node? node) (any-node? node)))
	(begin
	  (fix-set node green-edges set-green-edges!)
	  (fix-set node red-edges set-red-edges!)
	  (mfor-each 
	   (lambda (blue-edge)
	     (set-arg-node! blue-edge
			    (find-canonical-representative (arg-node blue-edge) classes))
	     (set-res-node! blue-edge
			    (find-canonical-representative (res-node blue-edge) classes)))
	   (blue-edges node))) '())
    node)
  (define (fix-table table)
    (define (canonical? node) (eq? node (find-canonical-representative node classes)))
    (define (filter-and-fix predicate-fn update-fn mlist)
      (let loop ((mlist mlist))
	(cond ((null? mlist) null)
	      ((predicate-fn (mcar mlist))
	       (mcons (update-fn (mcar mlist)) (loop (mcdr mlist))))
	      (else (loop (mcdr mlist))))))
    (define (fix-line line)
      (filter-and-fix
       (lambda (entry) (canonical? (mcar entry)))
       (lambda (entry) (mcons (mcar entry)
			     (find-canonical-representative (mcdr entry) classes)))
       line))
    (if (null? table)
	null
	(mcons (mcar table)
	      (filter-and-fix
	       (lambda (entry) (canonical? (mcar entry)))
	       (lambda (entry) (mcons (mcar entry) (fix-line (mcdr entry))))
	       (mcdr table)))))
  (make-internal-graph
   (mmap (lambda (class) (fix (mcar class))) classes)
   (fix-table (already-met graph))
   (fix-table (already-joined graph))))

;; USEFUL NODES

(define none-node (make-node 'none (mlist #t)))
(define (none-node? node) (eq? node none-node))

(define any-node (make-node 'any null))
(define (any-node? node) (eq? node any-node))

;; COLORED EDGE TESTS


(define (green-edge? from-node to-node)
  (cond ((any-node? from-node) #f)
	((none-node? from-node) #t)
	((mmemq to-node (green-edges from-node)) #t)
	(else #f)))

(define (red-edge? from-node to-node)
  (cond ((any-node? from-node) #f)
	((none-node? from-node) #t)
	((mmemq to-node (red-edges from-node)) #t)
	(else #f)))

;; SIGNATURE

; Return signature (i.e. <arg, res>) given an operation and a node

(define sig
  (let ((none-comma-any (mcons none-node any-node)))
    (lambda (op node)			; Returns (arg, res)
      (let ((the-edge (lookup-op op node)))
	(if (not (null? the-edge))
	    (mcons (arg-node the-edge) (res-node the-edge))
	    none-comma-any)))))

; Selectors from signature

(define (arg pair) (mcar pair))
(define (res pair) (mcdr pair))

;; CONFORMITY

(define (conforms? t1 t2)
  (define nodes-with-red-edges-out null)
  (define (add-red-edge! from-node to-node)
    (set-red-edges! from-node (adjoin to-node (red-edges from-node)))
    (set! nodes-with-red-edges-out
	  (adjoin from-node nodes-with-red-edges-out)))
  (define (greenify-red-edges! from-node)
    (set-green-edges! from-node
		      (mappend (red-edges from-node) (green-edges from-node)))
    (set-red-edges! from-node null))
  (define (delete-red-edges! from-node)
    (set-red-edges! from-node null))
  (define (does-conform t1 t2)
    (cond ((or (none-node? t1) (any-node? t2)) #t)
	  ((or (any-node? t1) (none-node? t2)) #f)
	  ((green-edge? t1 t2) #t)
	  ((red-edge? t1 t2) #t)
	  (else
	   (add-red-edge! t1 t2)
	   (let loop ((blues (blue-edges t2)))
	     (if (null? blues)
		 #t
		 (let* ((current-edge (mcar blues))
			(phi (operation current-edge)))
		   (and (has-op? phi t1)
			(does-conform
			 (res (sig phi t1))
			 (res (sig phi t2)))
			(does-conform
			 (arg (sig phi t2))
			 (arg (sig phi t1)))
			(loop (mcdr blues)))))))))
  (let ((result (does-conform t1 t2)))
    (mfor-each (if result greenify-red-edges! delete-red-edges!)
	      nodes-with-red-edges-out)
    result))

(define (equivalent? a b)
  (and (conforms? a b) (conforms? b a)))

;; EQUIVALENCE CLASSIFICATION
; Given a list of nodes, return a list of equivalence classes

(define (classify nodes)
  (let node-loop ((classes null)
		  (nodes nodes))
    (if (null? nodes)
	(mmap (lambda (class)
	       (sort class
		     (lambda (node1 node2)
		       (< (string-length (name node1))
			  (string-length (name node2))))))
	     classes)
	(let ((this-node (mcar nodes)))
	  (define (add-node classes)
	    (cond ((null? classes) (mlist (mlist this-node)))
		  ((equivalent? this-node (mcar (mcar classes)))
		   (mcons (mcons this-node (mcar classes))
			 (mcdr classes)))
		  (else (mcons (mcar classes)
			      (add-node (mcdr classes))))))
	  (node-loop (add-node classes)
		     (mcdr nodes))))))

; Given a node N and a classified set of nodes,
; find the canonical member corresponding to N

(define (find-canonical-representative element classification)
  (let loop ((classes classification))
    (cond ((null? classes) (error "Can't classify" element)) 
	  ((mmemq element (mcar classes)) (mcar (mcar classes)))
	  (else (loop (mcdr classes))))))

; Reduce a graph by taking only one member of each equivalence 
; class and canonicalizing all outbound pointers

(define (reduce graph)
  (let ((classes (classify (graph-nodes graph))))
    (canonicalize-graph graph classes)))


;; MEET/JOIN 
; These update the graph when computing the node for node1*node2

(define (blue-edge-operate arg-fn res-fn graph op sig1 sig2)
  (make-blue-edge op
		  (arg-fn graph (arg sig1) (arg sig2))
		  (res-fn graph (res sig1) (res sig2))))

(define (meet graph node1 node2)
  (cond ((eq? node1 node2) node1)
	((or (any-node? node1) (any-node? node2)) any-node) ; canonicalize
	((none-node? node1) node2)
	((none-node? node2) node1)
	((lookup (already-met graph) node1 node2)) ; return it if found
	((conforms? node1 node2) node2)
	((conforms? node2 node1) node1)
	(else
	 (let ((result
		(make-node (string-append "(" (name node1) " ^ " (name node2) ")") null)))
	   (add-graph-nodes! graph result)
	   (insert! (already-met graph) node1 node2 result)
	   (set-blue-edges! result
	     (mmap
	      (lambda (op)
		(blue-edge-operate join meet graph op (sig op node1) (sig op node2)))
	      (intersect (mmap operation (blue-edges node1))
			 (mmap operation (blue-edges node2)))))
	   result))))

(define (join graph node1 node2)
  (cond ((eq? node1 node2) node1)
	((any-node? node1) node2)
	((any-node? node2) node1)
	((or (none-node? node1) (none-node? node2)) none-node) ; canonicalize
	((lookup (already-joined graph) node1 node2)) ; return it if found
	((conforms? node1 node2) node1)
	((conforms? node2 node1) node2)
	(else
	 (let ((result
		(make-node (string-append "(" (name node1) " v " (name node2) ")") null)))
	   (add-graph-nodes! graph result)
	   (insert! (already-joined graph) node1 node2 result)
	   (set-blue-edges! result
             (mmap
	      (lambda (op)
		(blue-edge-operate meet join graph op (sig op node1) (sig op node2)))
	      (union (mmap operation (blue-edges node1))
		     (mmap operation (blue-edges node2)))))
	   result))))

;; MAKE A LATTICE FROM A GRAPH

(define (make-lattice g print?)
  (define (step g)
    (let* ((copy (copy-graph g))
	   (nodes (graph-nodes copy)))
      (mfor-each (lambda (first)
		  (mfor-each (lambda (second)
			      (meet copy first second)
			      (join copy first second))
			    nodes))
		nodes)
      copy))
  (define (loop g count)
    (if print? (ccdisplay count) '())
    (let ((lattice (step g)))
      (if print? (begin (ccdisplay " -> ") 
			(ccdisplay (mlength (graph-nodes lattice)))) '())
      (let* ((new-g (reduce lattice))
	     (new-count (mlength (graph-nodes new-g))))
	(if (= new-count count)
	    (begin
	      (if print? (ccnewline) '())
	      new-g)
	    (begin
	      (if print? (begin (ccdisplay " -> ")
				(ccdisplay new-count) (ccnewline)) '())
	      (loop new-g new-count))))))
  (let ((graph
	 (make-graph
		(adjoin any-node (adjoin none-node (graph-nodes (clean-graph g)))))))
    (loop graph (mlength (graph-nodes graph)))))


)
