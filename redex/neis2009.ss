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
    [D (dumps U ...)] ;; dump
    [U (dump C E S)]
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
                (term (cesd (code)
                            (env)
                            (stack) 
                            (dumps))))
    (test-match cesd-lang S
                (term (stack 1)))
    (test-match cesd-lang CESD
                (term (cesd (code)
                            (env)
                            (stack (CL (env 1) (code)))
                            (dumps)))))
  
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
                D))
     (--> (cesd (code (PushN N) i ...)
                E
                (stack V_0 ...)
                D)
          (cesd (code i ...)
                E
                (stack N V_0 ...)
                D))
     (--> (cesd (code (PushC C) i ...)
                E
                (stack V_0 ...)
                D)
          (cesd (code i ...)
                E
                (stack (CL E C) V_0 ...)
                D))
     (--> (cesd (code (PushRC C) i ...)
                E
                (stack V_0 ...)
                D)
          (cesd (code i ...)
                E
                (stack (RCL E C) V_0 ...)
                D))
     (--> (cesd (code App i ...)
                E
                (stack V (CL (env V_1 ...) C) V_0 ...)
                (dumps U ...))
          (cesd C
                (env V V_1 ...)
                (stack)
                (dumps (dump (code i ...) 
                             E 
                             (stack V_0 ...))
                       U ...)))
     (--> (cesd (code App i ...)
                E
                (stack V (RCL (env V_1 ...) C) V_0 ...)
                (dumps U ...))
          (cesd C
                (env V (RCL (env V_1 ...) C) V_1 ...)
                (stack)
                (dumps (dump (code i ...) 
                             E 
                             (stack V_0 ...))
                       U ...)))
     (--> (cesd (code O i ...)
                E
                (stack N_1 N_2 V ...)
                D)
          (cesd (code i ...)
                E
                (stack (APPLY * N_1 N_2)
                       V ...)
                D))))
  
  (define-metafunction cesd-lang
    [(APPLY O N_1 N_2) ,(cond [(equal? (term +) (term O))
                               (+ (term N_1)
                                  (term N_2))]
                              [(equal? (term -) (term O))
                               (- (term N_1)
                                  (term N_2))]
                              [(equal? (term *) (term O))
                               (* (term N_1)
                                  (term N_2))]
                              [(equal? (term /) (term O))
                               (/ (term N_1)
                                  (term N_2))])])
  
  (define (apply-test-suite)
    (test-equal (term (APPLY + 1 2))
                (term 3))
    (test-equal (term (APPLY - 3 2))
                (term 1))
    (test-equal (term (APPLY * 3 4))
                (term 12))
    (test-equal (term (APPLY / 16 2))
                (term 8))
    (test-results))
  
  (define (secd-rr-test-suite)
    (test--> secd-rr
             (term (cesd (code Swap) 
                         (env) 
                         (stack 1 2 3) 
                         (dumps)))
             (term (cesd (code)
                         (env)
                         (stack 2 1 3) 
                         (dumps))))
    (test--> secd-rr
             (term (cesd (code Dup) 
                         (env) 
                         (stack 1 2 3) 
                         (dumps)))
             (term (cesd (code)
                         (env)
                         (stack 1 1 2 3) 
                         (dumps))))
    (test--> secd-rr
             (term (cesd (code (PushV 4)) 
                         (env) 
                         (stack 1 2 3) 
                         (dumps)))
             (term (cesd (code)
                         (env)
                         (stack 1 2 3 4)
                         (dumps))))
    (test--> secd-rr
             (term (cesd (code (PushN 4))
                         (env)
                         (stack 1 2 3)
                         (dumps)))
             (term (cesd (code)
                         (env)
                         (stack 4 1 2 3)
                         (dumps))))
    (test--> secd-rr
             (term (cesd (code (PushC (code)))
                         (env)
                         (stack 1
                                2
                                3)
                         (dumps)))
             (term (cesd (code)
                         (env)
                         (stack (CL (env) (code))
                                1
                                2
                                3)
                         (dumps))))
    (test--> secd-rr
             (term (cesd (code (PushRC (code)))
                         (env)
                         (stack 1
                                2
                                3)
                         (dumps)))
             (term (cesd (code)
                         (env)
                         (stack (RCL (env) 
                                     (code))
                                1 
                                2
                                3)
                         (dumps))))
    (test--> secd-rr
             (term (cesd (code App Ret)
                         (env 0)
                         (stack 1 
                                (CL (env 2) 
                                    (code Swap))
                                3)
                         (dumps)))
             (term (cesd (code Swap)
                         (env 1 2)
                         (stack)
                         (dumps (dump (code Ret)
                                      (env 0)
                                      (stack 3))))))
    (test--> secd-rr
             (term (cesd (code App Ret)
                         (env 0)
                         (stack 1 
                                (RCL (env 2) 
                                     (code Swap))
                                3)
                         (dumps)))
             (term (cesd (code Swap)
                         (env 1
                              (RCL (env 2) 
                                   (code Swap))
                              2)
                         (stack)
                         (dumps (dump (code Ret)
                                      (env 0)
                                      (stack 3))))))
    (test-results))
  
  (define (test)
    (secd-lang-test-suite)
    (apply-test-suite)
    (secd-rr-test-suite)
    (test-results))
  
  (test)
  
  )
