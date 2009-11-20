(module midtgaard2008 scheme
  (require redex "redex-util.ss")
  
  (define-language caek-lang-core
    [P S]
    
    [T X 
       V]
    
    [S T
       (let (X S)
         S)
       (S S)]
    
    [V C
       (λ X S)]
    
    [X variable-not-otherwise-mentioned]
    
    [C number])
  
  (test-match caek-lang-core C (term 1))
  (test-match caek-lang-core V (term 1))
  (test-match caek-lang-core V (term (λ X 1)))
  (test-match caek-lang-core V (term (λ X X)))
  (test-match caek-lang-core S
              (term (let (x 3)
                      ((λ y y) x))))
  (test-match caek-lang-core P
              (term (let (x 3)
                      ((λ y y) x))))
  
  (define-extended-language caek-lang
    caek-lang-core
    
    [M (make-machine S E K)]
    
    [W C
       (make-closure (λ X S) E)]
    
    [E (make-env B ...)]
    [B (make-binding X W)]
    
    [K stop
       (make-stack F ...)]
    [F (make-frame X S E)])
  
  (test-match caek-lang M
              (term (make-machine 7 (make-env) (make-stack))))
  (test-match caek-lang E (term (make-env)))
  (test-match caek-lang E (term (make-env (make-binding x 3))))
  (test-match caek-lang E (term (make-env (make-binding x 3) 
                                          (make-binding y 4))))
  (test-match caek-lang E 
              (term (make-env (make-binding x 3)
                              (make-binding y 4)
                              (make-binding z 
                                            (make-closure (λ x x) 
                                                          (make-env))))))
  (test-match caek-lang K (term stop))
  (test-match caek-lang K (term (make-stack)))
  (test-match caek-lang K (term (make-stack (make-frame x x (make-env)))))
  
  (define-metafunction caek-lang
    [(μ C E) E]
    [(μ X E) ,(apply-env (term E) (term X))]
    [(μ (λ X S) E) (make-closure (λ X S) E)])
  
  (define (apply-env env x)
    (cond [(findf (λ (binding)
                    (equal? (second binding)
                            x))
                  (rest env))
           => third]
          [else (error "not found")]))
  
  (test-equal (apply-env (term (make-env (make-binding x 3)
                                         (make-binding y 2)))
                         (term x))
              3)
  (test-equal (apply-env (term (make-env (make-binding x 3)
                                         (make-binding y 2)))
                         (term y))
              2)
  
  (test-equal (term (μ 1 (make-env)))
              (term (make-env)))
  (test-equal (term (μ x (make-env (make-binding x 2))))
              (term 2))
  (test-equal (term (μ (λ x x) (make-env)))
              (term (make-closure (λ x x) (make-env))))
  
  #;
  (define caek-abstract
    (reduction-relation
     caek-lang
     (--> (make-machine T 
                        (make-env B ...)
                        (make-stack (make-frame X S_1 E_1)
                                    F ...))
          (make-machine S_1 
                        (make-env (make-binding X 666) 
                                  B ...)
                        (make-stack F ...)))))
  )