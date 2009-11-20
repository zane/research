(module midtgaard2008 scheme
  (require redex "redex-util.ss")
  
  (define-language caek-lang-core
    ;; Programs
    [P S]
    
    ;; Trivial Expressions
    [T X 
       V]
    
    ;; Serious Expressions
    [S T
       (let (X S)
         S)
       (S S)]
    
    ;; Values
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
    
    ;; Val - values
    [W C
       ((λ X S) E)]
    
    ;; Env - environments
    [E ((X W) ...)]
    
    ;; - stacks
    [K stop
       ((X S E) ... k)])
  
  )