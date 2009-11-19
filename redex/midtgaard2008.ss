(module midtgaard2008 scheme
  (require redex)
  
  (define-language caek-lang-core
    ;; Programs
    [P S]
    
    ;; Trivial Expressions
    [T X 
       V]
    
    ;; Serious Expressions
    [S (let (X S)
         S)
       (S S)]
    
    ;; Values
    [V C
       (λ X S)]
    
    [C number])
  
  (define-extended-language caek-lang
    caek-lang-core
    
    ;; Machine State
    [M (S E K)]
    
    ;; Control Stack
    [K (X S E)]
    
    ;;
    [E (B ...)]
    
    ;; Binding
    [B (X C)]
  
  )