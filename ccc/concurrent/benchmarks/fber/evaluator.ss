(module evaluator scheme

        (require "../../contract.ss")

        (require "./ast.ss")
        (require "./substitution.ss")

        (provide/contract 
          (evaluator (->d
                       ([expr    (future/c (valid-expr? null defs))]
                        [defs     (future/c valid-defs?)])
                       ()
                       [result any/c])))

        (define (evaluator expr defs)
          (cond ((boolean? expr) expr)
                ((nt? expr) (not (evaluator (nt-arg expr) defs)))
                ((nd? expr) (and 
                              (evaluator (nd-larg expr) defs)
                              (evaluator (nd-rarg expr) defs)))
                ((appl? expr) 
                 (let ((vars-body (flookup (appl-fun-name expr) defs)))
                   (evaluator (substitution
                                (cdr vars-body)
                                (car vars-body)
                                (map (lambda (e) (evaluator e defs))
                                     (appl-args expr))
                                defs)
                              defs)))))            

)
