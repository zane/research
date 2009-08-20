(module ast scheme
       
        (require "../../contract.ss")

        (provide (all-defined-out))

        (define-struct nt (arg))
        (define-struct nd (larg rarg))
        (define-struct appl (fun-name args))

        ;;an expr is one of
        ;;-- symbol
        ;;-- boolean
        ;;-- (make-nt expr?)
        ;;-- (make-nd expr? expr?)
        ;;-- (make-appl symbol? [listof expr?])
       
        (define-struct def (name vars body))
        ;;a definition is (make-def symbol? [listof symbol?] expr?)

        ;;get-vars: symbol? [listof def?] -> [listof symbol?]
        (define (get-vars f defs) 
          (cond ((null? defs) null)
                (else (if (symbol=? (def-name (car defs)) f)
                        (def-vars (car defs))
                        (get-vars f (cdr defs))))))
      
        ;;valid-expr?: [listof symbol] [listof def?] -> expr? -> boolean? 
        (define (valid-expr? env defs)
          (lambda (e)
            (cond ((symbol? e) (member e env))
                  ((boolean? e) #t)
                  ((nt? e) ((valid-expr? env defs) (nt-arg e)))
                  ((nd? e) (and 
                             ((valid-expr? env defs) (nd-larg e))
                             ((valid-expr? env defs) (nd-rarg e))))
                   ((appl? e) (and (andmap 
                                     (lambda (x)
                                       ((valid-expr? env defs) x))
                                     (appl-args e))
                                   (member (appl-fun-name e)
                                           (map 
                                             (lambda (f) (def-name f)) 
                                             defs))
                                   (= (length (get-vars 
                                                (appl-fun-name e) 
                                                defs))
                                      (length (appl-args e))))))))

        ;;valid-defs?: [listof def?] -> boolean?
        (define valid-defs?
          (lambda (defs)
            (andmap (lambda (def)
                      ((valid-expr? (def-vars def) defs) (def-body def)))
                    defs)))

        ;;value?: expr? -> boolean?
        (define (value? e)
          (boolean? e))

        ;;lookup: symbol? [listof symbol?] [listof value?] -> value?
        (define (lookup var vars vals)
          (cond ((null? vars) 'undefined)
                (else (if (symbol=? var (car vars))
                        (car vals)
                        (lookup var (cdr vars) (cdr vals))))))

        ;;flookup: symbol? [listof def?] [listof value?] -> expr
        (define (flookup var defs)
          (cond ((null? defs) 'undefined)
                (else (if (symbol=? var (def-name (car defs)))
                        (cons (def-vars (car defs)) (def-body (car defs))) 
                        (flookup var (cdr defs))))))



)
