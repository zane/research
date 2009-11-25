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
    
    [(X Y) variable-not-otherwise-mentioned]
    
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
    
    [M (machine S E K)]
    
    [W C
       (closure (λ X S) E)]
    
    [E (env B ...)]
    [B (binding X W)]
    
    [K (stack F ... stop)]
    [F (frame X S E)])
  
  (define (caek-lang-test-suite)
    (test-match caek-lang M
                (term (machine 7 (env) (stack stop))))
    (test-match caek-lang E (term (env)))
    (test-match caek-lang E (term (env (binding x 3))))
    (test-match caek-lang E (term (env (binding x 3) 
                                       (binding y 4))))
    (test-match caek-lang E 
                (term (env (binding x 3)
                           (binding y 4)
                           (binding z 
                                    (closure (λ x x) 
                                             (env))))))
    (test-match caek-lang K (term stop))
    (test-match caek-lang K (term (stack stop)))
    (test-match caek-lang K (term (stack (frame x x (env))
                                         stop)))
    (test-results))
  
  (define-metafunction caek-lang
    [(μ C E) C]
    [(μ X E) ,(apply-env (term E) (term X))]
    [(μ (λ X S) E) (closure (λ X S) E)])
  
  (define (apply-env env x)
    (cond [(findf (λ (binding)
                    (equal? (second binding)
                            x))
                  (rest env))
           => third]
          [else (term error)]))
  
  (define (apply-env-test-suite)
    (test-equal (apply-env (term (env (binding x 3)
                                      (binding y 2)))
                           (term x))
                3)
    (test-equal (apply-env (term (env (binding x 3)
                                      (binding y 2)))
                           (term y))
                2)
    (test-results))
  
  (define (μ-test-suite)
    (test-equal (term (μ 1 (env)))
                (term 1))
    (test-equal (term (μ x (env (binding x 2))))
                (term 2))
    (test-equal (term (μ (λ x x) (env)))
                (term (closure (λ x x) (env))))
    (test-results))
  
  (define caek-abstract
    (reduction-relation
     caek-lang
     (--> (machine T 
                   E
                   (stack (frame X S_1 (env B ...))
                          F ... stop))
          (machine S_1 
                   (env (binding X (μ T E))
                        B ...)
                   (stack F ... stop)))
     (--> (machine (let (X T)
                     S)
                   (env B ...)
                   K)
          (machine S 
                   (env (binding X (μ T (env B ...)))
                        B ...)
                   K))
     (--> (machine (T_0 T_1)
                   E
                   K)
          (machine S_1 
                   (env (binding X W)
                        B_1 ...)
                   K)
          (where (term (closure (λ X S_1) (env B_1 ...)))
                 (term (μ T_0 E)))
          (where (term W)
                 (term (μ T_1 E))))
     (--> (machine (let (X (T_0 T_1))
                     S)
                   E
                   (stack F ... stop))
          (machine S_1
                   (env (binding Y W)
                        B_1 ...)
                   (stack (frame X S E)
                          F ... stop))
          (where (term (closure (λ Y S_1) (env B_1 ...)))
                 (term (μ T_0 E)))
          (where (term W)
                 (term (μ T_1 E))))))
  
  (define (caek-abstract-test-suite)
    (test--> caek-abstract
             (term (machine 7 
                            (env)
                            (stack (frame x x (env))
                                   stop)))
             (term (machine x
                            (env (binding x 7))
                            (stack stop))))
    (test--> caek-abstract
             (term (machine (let (x 3)
                              x)
                            (env)
                            (stack stop)))
             (term (machine x
                            (env (binding x 3))
                            (stack stop))))
    (test--> caek-abstract
             (term (machine ((λ x x) 1)
                            (env)
                            (stack stop)))
             (term (machine x
                            (env (binding x 1))
                            (stack stop))))
    (test--> caek-abstract
             (term (machine (let (x ((λ y y) 1))
                              x)
                            (env)
                            (stack stop)))
             (term (machine y
                            (env (binding y 1))
                            (stack (frame x x (env))
                                   stop))))
    (test-results))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Collecting Semantics
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-extended-language caek-cs
    caek-lang
    
    [MSet (set M ...)]
    
    [ESet (set E ...)])
  
  (define (caek-cs-test-suite)
    (test-match caek-cs A
                (term (set (machine 7 
                                    (env)
                                    (stack stop)))))
    (test-results))
  
  (define-metafunction caek-cs
    [(μ_c C ESet) (set C)]
    [(μ_c X ESet) (set ,@(filter (λ (W) W)
                                 (map (λ (E)
                                        (apply-env E
                                                   (term X)))
                                      (rest (term ESet)))))]
    [(μ_c (λ X S) ESet) (set ,@(map (λ (E)
                                      (term (closure (λ X S) ,E)))
                                    (rest (term ESet))))])
  
  (define (μ_c-test-suite)
    (test-equal (term (μ_c 1 (set (env))))
                (term (set 1)))
    (test-equal (term (μ_c 3 (set (env (binding x 1)
                                       (binding y 2)))))
                (term (set 3)))
    (test-equal (term (μ_c x (set (env (binding x 1))
                                  (env (binding x 2))
                                  (env (binding x 3)))))
                (term (set 1 2 3)))
    (test-equal (term (μ_c (λ x x) (set (env))))
                (term (set (closure (λ x x) (env)))))
    (test-results))
  
  (define-metafunction caek-cs
    [(∪ (set M_0 ...)
        (set M_1 ...))
     ,(sort (remove-duplicates (term (set M_0 ... M_1 ...)))
            sexp<?)])
  
  ;; A SEXP is one of
  ;; - number?
  ;; - symbol?
  ;; - empty?
  ;; - (cons SEXP SEXP)
  
  (define (symbol<? s1 s2)
    (string<? (symbol->string s1)
              (symbol->string s2)))
  
  (define (sexp<? s1 s2)
    (cond [(and (number? s1)
                (number? s2))
           (< s1 s2)]
          [(and (number? s1)
                (symbol? s2))
           #t]
          [(and (number? s1)
                (list? s2))
           #t]
          [(and (symbol? s1)
                (symbol? s2))
           (symbol<? s1 s2)]
          [(and (symbol? s1)
                (number? s2))
           #f]
          [(and (symbol? s1)
                (list? s2))
           #t]
          [(and (list? s1)
                (number? s2))
           #f]
          [(and (list? s1)
                (symbol? s2))
           #f]
          [(and (empty? s1)
                (empty? s2))
           #f]
          [(and (empty? s1)
                (cons? s2))
           #f]
          [(and (cons? s1)
                (empty? s2))
           #t]
          [else (let ([i (sexp<? (first s1)
                                 (first s2))])
                  (if (not (equal? (first s1)
                                   (first s2)))
                      i
                      (sexp<? (rest s1)
                              (rest s2))))]))
  
  (define (sexp<?-test-suite)
    (test-equal (sexp<? 0 0) #f)
    (test-equal (sexp<? 0 1) #t)
    (test-equal (sexp<? 1 0) #f)
    (test-equal (sexp<? 0 'x) #t)
    (test-equal (sexp<? 'x 0) #f)
    (test-equal (sexp<? 0 empty) #t)
    (test-equal (sexp<? empty 0) #f)
    (test-equal (sexp<? 'x empty) #t)
    (test-equal (sexp<? empty 'x) #f)
    (test-equal (sexp<? empty empty) #f)
    (test-equal (sexp<? '(0) '(1)) #t)
    (test-equal (sexp<? '(0) '(0)) #f)
    (test-equal (sexp<? '(0 0) '(0 1)) #t)
    (test-equal (sexp<? '(1 0) '(0 1)) #f)
    (test-equal (sexp<? '(0 (0 0)) '(0 (0 1))) #t)
    (test-results))
  
  (define caek-f
    (reduction-relation
     caek-cs
     (--> (set M_0 ...
               M
               M_1 ...)
          (∪ (set M_0 ...
                  M
                  M_1 ...)
             (set M_3))
          (where (M_2 ... M_3 M_4 ...)
                 ,(apply-reduction-relation caek-abstract (term M))))))
  
  (define caek-f_c
    (reduction-relation
     caek-cs
     (--> (set M_0 ...
               (machine T 
                        E
                        (stack (frame X S_1 (env B ...))
                               F ... stop))
               M_1 ...)
          (∪ (set M_0 ...  
                  (machine T 
                           E
                           (stack (frame X S_1 (env B ...))
                                  F ... stop))
                  M_1 ...)
             (set (machine S_1 
                           (env (binding X (μ T E))
                                B ...)
                           (stack F ... stop)))))
     (--> (set M_0 ...
               (machine (let (X T)
                          S)
                        (env B ...)
                        K)
               M_1 ...)
          (∪ (set M_0 ...
                  (machine (let (X T)
                             S)
                           (env B ...)
                           K)
                  M_1 ...)
             (set (machine S 
                           (env (binding X (μ T (env B ...)))
                                B ...)
                           K))))
     (--> (set M_0 ...
               (machine (T_0 T_1)
                        E
                        K)
               M_1 ...)
          (∪ (set M_0 ...
                  (machine (T_0 T_1)
                           E
                           K)
                  M_1 ...)
             (set (machine S_1 
                           (env (binding X W)
                                B_1 ...)
                           K)))
          (where (term (closure (λ X S_1) (env B_1 ...)))
                 (term (μ T_0 E)))
          (where (term W)
                 (term (μ T_1 E))))
     (--> (set M_0 ...
               (machine (let (X (T_0 T_1))
                          S)
                        E
                        (stack F ... stop))
               M_1 ...)
          (∪ (set M_0 ...
                  (machine (let (X (T_0 T_1))
                             S)
                           E
                           (stack F ... stop))
                  M_1 ...)
             (set (machine S_1
                           (env (binding Y W)
                                B_1 ...)
                           (stack (frame X S E)
                                  F ... stop))))
          (where (term (closure (λ Y S_1) (env B_1 ...)))
                 (term (μ T_0 E)))
          (where (term W)
                 (term (μ T_1 E))))))
  
  (define (caek-f_c-test-suite)
    (test--> caek-f_c
             (term (set (machine 7 
                                 (env)
                                 (stack (frame x x (env))
                                        stop))))
             (term (set (machine 7 
                                 (env)
                                 (stack (frame x x (env))
                                        stop))
                        (machine x
                                 (env (binding x 7))
                                 (stack stop)))))
    (test--> caek-f_c
             (term (set (machine (let (x 3)
                                   x)
                                 (env)
                                 (stack stop))))
             (term (set (machine x
                                 (env (binding x 3))
                                 (stack stop))
                        (machine (let (x 3)
                                   x)
                                 (env)
                                 (stack stop)))))
    (test--> caek-f_c
             (term (set (machine ((λ x x) 1)
                                 (env)
                                 (stack stop))))
             (term (set (machine x
                                 (env (binding x 1))
                                 (stack stop))
                        (machine ((λ x x) 1)
                                 (env)
                                 (stack stop)))))
    (test--> caek-f_c
             (term (set (machine (let (x ((λ y y) 1))
                                   x)
                                 (env)
                                 (stack stop))))
             (term (set (machine y
                                 (env (binding y 1))
                                 (stack (frame x x (env))
                                        stop))
                        (machine (let (x ((λ y y) 1))
                                   x)
                                 (env)
                                 (stack stop)))))
    (test-results))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Lemma Testing
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (lemma4.1-test-suite n)
    (redex-check caek-lang
                 (T E)
                 (equal? (term (set (μ T E)))
                         (term (μ_c T (set E))))
                 #:attempts n))
  
  (define (lemma4.2-test-suite n)
    (redex-check caek-cs
                 (set S 
                      (env)
                      (stack stop))
                 (equal? (apply-reduction-relation* caek-f (term A))
                         (apply-reduction-relation* caek-f_c (term A)))
                 #:attempts n))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Traces
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (t1)
    (traces caek-abstract
            (term (machine (let (x 1)
                             (let (y 2)
                               (let (z (λ x (λ y x)))
                                 (let (q (z x))
                                   (let (r (q y))
                                     r)))))
                           (env (binding x 3))
                           (stack stop)))))
  
  (define (t2)
    (traces caek-abstract
            (term (machine (let (g (λ z z))
                             (let (f (λ k (k 2)))
                               (let (y (f (λ x x)))
                                 (g y))))
                           (env)
                           (stack stop)))))
  
  (define (t3)
    (traces caek-f
            (term (set (machine (let (g (λ z z))
                                  (let (f (λ k (k 2)))
                                    (let (y (f (λ x x)))
                                      (g y))))
                                (env)
                                (stack stop))))))
  
  (define (t4)
    (traces caek-abstract
            (term (machine (let (g (λ z z))
                             (let (f (λ k (k 2)))
                               (let (y (f (λ x x)))
                                 (g y))))
                           (env)
                           (stack stop)))))
  
  (define (t5)
    (traces caek-abstract
            (term (machine (let (i (λ x x))
                             (let (g i)
                               (let (t ((λ x 3) 1))
                                 (i t))))
                           (env)
                           (stack stop)))))
  
  )
