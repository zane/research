(module neis2009 scheme
  (require redex "redex-util.ss")
  
  (define-language cesd-lang
    ;; instructions
    [i Swap
       Dup
       (PushV N)
       (Op O)
       (PushC C)
       (PushRC C)
       App
       Ret
       (Sel C C)
       Join
       MkPair
       Fst
       Snd
       Eq]
    
    ;; values
    [V N 
       (CL E C) ;; closure
       (RCL E C) ;; recursive closure
       (PR V V) ;; pair
       ]
    
    [E (env V ...)] ;; environment
    [C (code i ...)] ;; code
    [S (stack V ...)] ;; stack
    [D (dump C E S)] ;; dump
    [CESD (cesd C E S D)] ;; CESD
    
    ;; binary operators
    [O +
       -
       *
       /]
    
    ;; numbers
    [N number])
  
  (define (secd-lang-test-suite)
    (test-match cesd-lang CESD 
                (term (cesd (code) (env) (stack) 
                            (dump (code) (env) (stack))))))
  
  (define secd-rr
    (reduction-relation
     cesd-lang
     
     (--> (cesd (code Swap i ...) 
                E 
                (stack V_1 V_2 V_3 ...)
                D)
          (cesd (code i ...)
                E
                (stack V_2 V_1 V_3 ...)
                D))
     (--> (cesd (code Dup i ...) 
                E 
                (stack V_1 V_2 ...)
                D)
          (cesd (code i ...)
                E
                (stack V_1 V_1 V_2 ...)
                D))
     (--> (cesd (code (PushV N) i ...)
                E
                (stack V_0 ...)
                D)
          (cesd (code i ...)
                E
                (stack V_0 ... N)
                D))))
  
  (define (secd-rr-test-suite)
    (test--> secd-rr
             (term (cesd (code Swap) 
                         (env) 
                         (stack 1 2 3) 
                         (dump (code) (env) (stack))))
             (term (cesd (code)
                         (env)
                         (stack 2 1 3) 
                         (dump (code) (env) (stack)))))
    (test--> secd-rr
             (term (cesd (code Dup) 
                         (env) 
                         (stack 1 2 3) 
                         (dump (code) (env) (stack))))
             (term (cesd (code)
                         (env)
                         (stack 1 1 2 3) 
                         (dump (code) (env) (stack)))))
    (test--> secd-rr
             (term (cesd (code (PushV 4)) 
                         (env) 
                         (stack 1 2 3) 
                         (dump (code) (env) (stack))))
             (term (cesd (code)
                         (env)
                         (stack 1 2 3 4)
                         (dump (code) (env) (stack)))))
    (test-results))
  
  (define (test)
    (secd-lang-test-suite)
    (secd-rr-test-suite)
    (test-results))
  
  (test)
  
  )
