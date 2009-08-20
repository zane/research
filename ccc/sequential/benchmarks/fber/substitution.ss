(module substitution scheme
        
       (require "../../contract.ss")

       (require "./ast.ss")
        
        
       (provide substitution)

      ;;  (provide/contract 
      ;;    (substitution (->d
      ;;                    ([expr   (valid-expr? env-vars defs)]
      ;;                     [env-vars   (lambda (e) 
      ;;                                   (and (= (length e) (length env-values))
      ;;                                        (andmap symbol? e)))]
      ;;                     [env-values (listof value?)]
      ;;                     [defs   valid-defs?])
      ;;                    ()
      ;;                    [result any/c])))


    
        (define (substitution expr env-vars env-values defs)
          (cond ((symbol? expr) (lookup expr env-vars env-values))
                ((boolean? expr) expr)
                ((nt? expr) (make-nt
                              (substitution
                                (nt-arg expr) 
                                 env-vars
                                 env-values
                                 defs)))
                ((nd? expr) (make-nd
                              (substitution
                                (nd-larg expr) 
                                 env-vars
                                 env-values
                                 defs)
                               (substitution
                                 (nd-rarg expr)
                                 env-vars
                                 env-values
                                 defs)))
                ((appl? expr) (make-appl
                                (appl-fun-name expr)
                                (map 
                                  (lambda (e)
                                    (substitution e env-vars env-values defs))
                                  (appl-args expr))))))



)

