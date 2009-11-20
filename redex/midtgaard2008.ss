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
    
    [K stop
       (stack F ... stop)]
    [F (frame X S E)])
  
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
          [else #f]))
  
  (test-equal (apply-env (term (env (binding x 3)
                                    (binding y 2)))
                         (term x))
              3)
  (test-equal (apply-env (term (env (binding x 3)
                                    (binding y 2)))
                         (term y))
              2)
  
  (test-equal (term (μ 1 (env)))
              (term 1))
  (test-equal (term (μ x (env (binding x 2))))
              (term 2))
  (test-equal (term (μ (λ x x) (env)))
              (term (closure (λ x x) (env))))
  
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
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Collecting Semantics
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-extended-language caek-cs
    caek-lang
    
    [A (set M ...)]
    
    [ESet (set E ...)]
    
    [WSet (set W ...)]
    
    [Any P
         M])
  
  (test-match caek-cs A
              (term (set (machine 7 
                                  (env)
                                  (stack stop)))))
  
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
  
  (define-metafunction caek-cs
    [(∪ (set Any_0 ...)
        (set Any_1 ...))
     ,(sort (remove-duplicates (term (set Any_0 ... Any_1 ...)))
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
  
  (define caek-collecting
    (reduction-relation
     caek-cs
     (--> (set M_0 ...
               M
               M_1 ...)
          (∪ (set M_0 ... )
             (∪ (set M)
                (set M_1 ...))))))
  
  )