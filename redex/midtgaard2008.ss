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
    
    [M (make-machine S E K)]
    
    [W C
       (make-closure (λ X S) E)]
    
    [E (make-env B ...)]
    [B (make-binding X W)]
    
    [K stop
       (make-stack F ... stop)]
    [F (make-frame X S E)])
  
  (test-match caek-lang M
              (term (make-machine 7 (make-env) (make-stack stop))))
  (test-match caek-lang E (term (make-env)))
  (test-match caek-lang E (term (make-env (make-binding x 3))))
  (test-match caek-lang E (term (make-env (make-binding x 3) 
                                          (make-binding y 4))))
  (test-match caek-lang E 
              (term (make-env (make-binding x 3)
                              (make-binding y 4)
                              (make-binding z 
                                            (make-closure (λ x x) 
                                                          (make-env))))))
  (test-match caek-lang K (term stop))
  (test-match caek-lang K (term (make-stack stop)))
  (test-match caek-lang K (term (make-stack (make-frame x x (make-env))
                                            stop)))
  
  (define-metafunction caek-lang
    [(μ C E) C]
    [(μ X E) ,(apply-env (term E) (term X))]
    [(μ (λ X S) E) (make-closure (λ X S) E)])
  
  (define (apply-env env x)
    (cond [(findf (λ (binding)
                    (equal? (second binding)
                            x))
                  (rest env))
           => third]
          [else #f]))
  
  (test-equal (apply-env (term (make-env (make-binding x 3)
                                         (make-binding y 2)))
                         (term x))
              3)
  (test-equal (apply-env (term (make-env (make-binding x 3)
                                         (make-binding y 2)))
                         (term y))
              2)
  
  (test-equal (term (μ 1 (make-env)))
              (term 1))
  (test-equal (term (μ x (make-env (make-binding x 2))))
              (term 2))
  (test-equal (term (μ (λ x x) (make-env)))
              (term (make-closure (λ x x) (make-env))))
  
  (define caek-abstract
    (reduction-relation
     caek-lang
     (--> (make-machine T 
                        E
                        (make-stack (make-frame X S_1 (make-env B ...))
                                    F ... stop))
          (make-machine S_1 
                        (make-env (make-binding X (μ T E))
                                  B ...)
                        (make-stack F ... stop)))
     (--> (make-machine (let (X T)
                          S)
                        (make-env B ...)
                        K)
          (make-machine S 
                        (make-env (make-binding X (μ T (make-env B ...)))
                                  B ...)
                        K))
     (--> (make-machine (T_0 T_1)
                        E
                        K)
          (make-machine S_1 
                        (make-env (make-binding X W)
                                  B_1 ...)
                        K)
          (where (term (make-closure (λ X S_1) (make-env B_1 ...)))
                 (term (μ T_0 E)))
          (where (term W)
                 (term (μ T_1 E))))
     (--> (make-machine (let (X (T_0 T_1))
                          S)
                        E
                        (make-stack F ... stop))
          (make-machine S_1
                        (make-env (make-binding Y W)
                                  B_1 ...)
                        (make-stack (make-frame X S E)
                                    F ... stop))
          (where (term (make-closure (λ Y S_1) (make-env B_1 ...)))
                 (term (μ T_0 E)))
          (where (term W)
                 (term (μ T_1 E))))))
  
  (test--> caek-abstract
           (term (make-machine 7 
                               (make-env)
                               (make-stack (make-frame x x (make-env))
                                           stop)))
           (term (make-machine x
                               (make-env (make-binding x 7))
                               (make-stack stop))))
  (test--> caek-abstract
           (term (make-machine (let (x 3)
                                 x)
                               (make-env)
                               (make-stack stop)))
           (term (make-machine x
                               (make-env (make-binding x 3))
                               (make-stack stop))))
  (test--> caek-abstract
           (term (make-machine ((λ x x) 1)
                               (make-env)
                               (make-stack stop)))
           (term (make-machine x
                               (make-env (make-binding x 1))
                               (make-stack stop))))
  (test--> caek-abstract
           (term (make-machine (let (x ((λ y y) 1))
                                 x)
                               (make-env)
                               (make-stack stop)))
           (term (make-machine y
                               (make-env (make-binding y 1))
                               (make-stack (make-frame x x (make-env))
                                           stop))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Collecting Semantics
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-extended-language caek-cs
    caek-lang
    
    [A (make-set M ...)]
    
    [ESet (make-set E ...)]
    
    [WSet (make-set W ...)]
    
    [Any P
         M])
  
  (test-match caek-cs A
              (term (make-set (make-machine 7 
                                            (make-env)
                                            (make-stack stop)))))
  
  (define-metafunction caek-cs
    [(μ_c C ESet) (make-set C)]
    [(μ_c X ESet) (make-set ,@(filter (λ (W) W)
                                      (map (λ (E)
                                             (apply-env E
                                                        (term X)))
                                           (rest (term ESet)))))]
    [(μ_c (λ X S) ESet) (make-set ,@(map (λ (E)
                                           (term (make-closure (λ X S) ,E)))
                                         (rest (term ESet))))])
  
  (test-equal (term (μ_c 1 (make-set (make-env))))
              (term (make-set 1)))
  (test-equal (term (μ_c 3 (make-set (make-env (make-binding x 1)
                                               (make-binding y 2)))))
              (term (make-set 3)))
  (test-equal (term (μ_c x (make-set (make-env (make-binding x 1))
                                     (make-env (make-binding x 2))
                                     (make-env (make-binding x 3)))))
              (term (make-set 1 2 3)))
  (test-equal (term (μ_c (λ x x) (make-set (make-env))))
              (term (make-set (make-closure (λ x x) (make-env)))))
  
  #;
  (define-metafunction caek-cs
    [(∪ (make-set Any_0 ...)
        (make-set Any_1 ...))
     ,(sort (remove-duplicates (term (make-set Any_0 Any_1)))
            (sexp<?))])
  
  ;; A SEXP is one of
  ;; - number?
  ;; - symbol?
  ;; - [listof SEXP]
  
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
  
  #;
  (define caek-collecting
    (reduction-relation
     caek-cs
     (--> (make-set A ...
                    (make-machine S E K)))))
  
  )