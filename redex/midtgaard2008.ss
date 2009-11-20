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
    
    [E (make-env (make-binding X W) ...)]
    
    [K stop
       (make-stack (X S E) ...)])
  
  (test-match caek-lang E (term (make-env)))
  (test-match caek-lang E (term (make-env (make-binding x 3))))
  (test-match caek-lang E (term (make-env (make-binding x 3) 
                                          (make-binding y 4))))
  (test-match caek-lang K (term stop))
  (test-match caek-lang E 
              (term (make-env (make-binding x 3)
                              (make-binding y 4)
                              (make-binding z 
                                            (make-closure (λ x x) 
                                                          (make-env))))))
  (test-match caek-lang K (term (make-stack)))
  (test-match caek-lang K (term (make-stack (x x (make-env)))))

  )