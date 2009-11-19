(module midtgaard2008 scheme
  (require redex)
  
  (define-language caek-lang
    ;; Programs
    (P S)
    
    ;; Trivial Expressions
    (T C
       X
       (λ X S))
    
    ;; Serious Expressions
    (S (let (X S)
         S)
       (S S))))