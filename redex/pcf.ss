(module pcf scheme
  (require redex "redex-util.ss")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LANGUAGE
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-language pcf-lang
    [M X 
       L
       (M M) 
       N
       B
       (O2 M M)
       (O1 M)
       (fix M)
       (if M M M)] ;; expressions
    
    [V N B X L] ;; values
    [L (λ X T M)] ;; functions
    
    [O O1 O2] ;; operators
    [O1 add1 sub1 zero?] ;; unary
    [O2 + - * ^] ;; binary
    
    [N number] ;; numbers
    [B true false] ;; booleans
    
    [X variable-not-otherwise-mentioned] ;; variables
    
    ;; types
    [T Int
       Bool
       (-> T T)]
    
    ;; contexts
    [E hole 
       (V E)
       (E M)
       (O V ... E M ...)
       (if E M M)
       (fix E)])
  
  (define (pcf-lang-test-suite)
    (test-match pcf-lang T
                (term Int))
    (test-match pcf-lang T
                (term (-> Int Int)))
    (test-match pcf-lang V
                (term (λ x (-> Int Int)
                        x)))
    (test-match pcf-lang M
                (term ((λ x (-> Int Int)
                         x)
                       0)))
    (test-match pcf-lang V
                0)
    (test-match pcf-lang M
                (term (λ f (-> Int Int)
                        (λ n Int
                          (if (zero? n)
                              1 
                              (* n (f (- n 1))))))))
    (test-results))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; REDUCTION RELATION
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define pcf-rr
    (reduction-relation
     pcf-lang
     
     (v ((λ X T M) V)
        (subst M X V) 
        β-v)
     (v (O N ...) 
        (δ (O N ...)) 
        δ)
     (v (fix (λ X T M)) 
        (subst M X (fix (λ X T M))) 
        Fix)
     (v (if false M_0 M_1)
        M_1
        if-false)
     (v (if V M_0 M_1)
        M_0
        (side-condition (not (equal? (term V)
                                     (term false))))
        if-true)
     
     with
     ((--> (in-hole E M) 
           (in-hole E M_1)) 
      (v M M_1))))
  
  (define (pcf-rr-test-suite)
    (test-->> pcf-rr
              (term ((λ x Int x) 0))
              (term 0))
    (test-->> pcf-rr
              (term (if true 0 1))
              (term 0))
    (test-->> pcf-rr
              (term (if false 0 1))
              (term 1))
    (test-->> pcf-rr
              (term (if (λ x Int x) 0 1))
              (term 0))
    (test-->> pcf-rr
              (term ((fix (λ f (-> Int Int)
                            (λ n Int
                              (if (zero? n)
                                  1 
                                  (* n (f (- n 1)))))))
                     4))
              24))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; METAFUNCTIONS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-metafunction pcf-lang
    ((δ (zero? 0))
     true)
    ((δ (zero? N))
     false)
    ((δ (sub1 N))
     ,(sub1 (term N)))
    ((δ (add1 N))
     ,(add1 (term N)))
    ((δ (+ N_1 
           N_2))
     ,(+ (term N_1)
         (term N_2)))
    ((δ (- N_1 
           N_2)) 
     ,(- (term N_1) 
         (term N_2)))
    ((δ (* N_1
           N_2)) 
     ,(* (term N_1)
         (term N_2)))
    ((δ (^ N_1
           N_2)) 
     ,(expt (term N_1) 
            (term N_2))))
  
  (define-metafunction pcf-lang
    ((subst (λ X_1 T any_2) 
            X_1 
            any_1)
     (λ X_1 T 
       any_2))
    ((subst (λ X_2 T 
              any_2) 
            X_1 
            any_1)
     (λ X_new T
       (subst (subst-var any_2 X_2 X_new) 
              X_1
              any_1))
     (where X_new ,(variable-not-in (term (X_1
                                           any_1
                                           any_2))
                                    (term X_2))))
    ((subst X_1 
            X_1 
            any_1) 
     any_1)
    ((subst (any_2 ...) 
            X_1
            any_1)
     ((subst any_2 
             X_1 
             any_1)
      ...))
    ((subst any_2
            X_1
            any_1)
     any_2))
  
  (define-metafunction pcf-lang
    ((subst-var (any_1 ...)
                variable_1 
                variable_2)
     ((subst-var any_1
                 variable_1 
                 variable_2) 
      ...))
    ((subst-var variable_1
                variable_1 
                variable_2)
     variable_2)
    ((subst-var any_1
                variable_1
                variable_2)
     any_1))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TRACES
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (t0)
    (traces pcf-rr
            (term ((λ x (-> Int Int)
                     x)
                   0))))
  
  (define (t1)
    (traces pcf-rr 
            (term ((λ x (-> (-> Int Int) 
                            (-> Int Int)) 
                     x)
                   (λ x Int x)))))
  
  (define (t2)
    (traces pcf-rr
            (term (if false 0 1))))
  
  (define (t3)
    (traces pcf-rr 
            (term ((fix (λ x Int x))
                   0))))
  
  (define (t4)
    (traces pcf-rr
            (term ((fix (λ f (-> Int Int)
                          (λ n Int
                            (if (zero? n)
                                1 
                                (* n (f (- n 1)))))))
                   4))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MISC
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (test)
    (pcf-lang-test-suite)
    (pcf-rr-test-suite)
    (test-results))
  
  )