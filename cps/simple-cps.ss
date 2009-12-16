(module simple-cps scheme
  (require redex)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LANGUAGE
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-language λlang
    [E X
       Val
       (E E)
       (if E E E)] ; SrcExp
    [Val (λ (X ...) E)]
    [X variable-not-otherwise-mentioned]) ; SrcVar
  
  (define-extended-language λsim
    λlang
    
    [(V L) variable-not-otherwise-mentioned] ; ValVar
    [W variable-not-otherwise-mentioned] ; ContVar
    [A V
       (λ (V W) C)]
    [K W
       (κ (V) C)]
    [C (A A K) ; call
       (K A) ; return
       (if A C C)] ; necessary?
    
    ;; Environments
    [Env [B ...]]
    [B (X V)])
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; METAFUNCTIONS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-metafunction λsim
    lookup : X Env -> V
    [(lookup X (B_0 ... (X V) B_1 ...))
     V])
  
  (define-metafunction λsim
    [(T X 
        Env 
        K)
     (K (lookup x Env))]
    [(T (λ (X) E) 
        (B ...) 
        K) 
     (K (λ (V_new W_new)
          (T E ((X V_new) B ...) W_new)))
     (where V_new ,(variable-not-in (term (X E B ... K))
                                    (term v)))
     (where W_new ,(variable-not-in (term (X E B ... K))
                                    (term w)))]
    [(T (E_1 E_2) 
        Env 
        K)
     (T E_1 Env (κ (V_new)
                   (T E_2 Env (κ (L_new)
                                 (V_new L_new K)))))
     (where V_new ,(variable-not-in (term (E_1 E_2 Env K))
                                    (term v)))
     (where L_new ,(variable-not-in (term (E_1 E_2 Env K))
                                    (term l)))])
  
  )