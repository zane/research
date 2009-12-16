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
    [A V ; Arg
       (λ (V W) C)]
    [K W ; Cont
       (κ (V) C)]
    [C (A A K) ; call
       (K A) ; return
       (if A C C)]
    
    ;; Environments
    [Env [B ...]]
    [B (X V)])
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CPS conversion, w/a symbol table
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
     (where W_new ,(variable-not-in (term (X E B ... K V_new))
                                    (term w)))]
    [(T (E_1 E_2) 
        Env 
        K)
     (T E_1 Env (κ (V_new)
                   (T E_2 Env (κ (L_new)
                                 (V_new L_new K)))))
     (where V_new ,(variable-not-in (term (E_1 E_2 Env K))
                                    (term v)))
     (where L_new ,(variable-not-in (term (E_1 E_2 Env K V_new))
                                    (term l)))])
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Introduce app, ret & bind funs
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-metafunction λsim
    ret : K A -> C
    [(ret K A) (K A)])
  
  (define-metafunction λsim
    app : A A W -> C
    [(app A_1 A_2 K) (A_1 A_2 K)])
  
  (define-metafunction λsim
    T_1 : E Env K -> any
    [(T_1 X Env K) (ret K (lookup X Env))]
    [(T_1 (λ (X) E)
          (B ...)
          K)
     (ret K (λ (V W) (T_1 E ((X V) B ...) W)))
     (where V ,(variable-not-in (term (X E B ... K))
                                (term v)))
     (where W ,(variable-not-in (term (X E B ... K V))
                                (term w)))]
    [(T_1 (E_1 E_2) Env K)
     (T_1 E_1 Env (κ (V_1)
                     (T_1 E_2 Env (κ (V_2)
                                     (app V_1 V_2 K)))))
     (where V_1 ,(variable-not-in (term (E_1 E_2 Env K))
                                  (term v)))
     (where V_2 ,(variable-not-in (term (E_1 E_2 Env K V_1))
                                  (term v)))])
  
  )