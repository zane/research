(module pcf scheme
  (require redex)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; LANGUAGE
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-language pcf-lang
    ((M N L K) X (λ X M) (M M) b (o2 M M) (o1 M) (fix M))
    (o o1 o2)
    (o1 add1 sub1 iszero)
    (o2 + - * ^)
    (b number)
    ((V U W) b X (λ X M))
    (E hole (V E) (E M) (o V ... E M ...))
    ;(E hole (E M) (o E M ...))
    ((X Y Z) variable-not-otherwise-mentioned))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; REDUCTION RELATION
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define pcf-rr
    (reduction-relation
     pcf-lang
     (v ((λ X M) V) (subst M X V) β-v)
     (v (o b ...) (δ (o b ...)) δ)
     (v (fix (λ X M)) (subst M X (fix (λ X M))) Fix)
     with
     ((--> (in-hole E M) (in-hole E N)) (v M N))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; METAFUNCTIONS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-metafunction pcf-lang
    ((δ (iszero 0)) (λ X (λ Y X)))
    ((δ (iszero b)) (λ X (λ Y Y)))
    ((δ (sub1 b)) ,(sub1 (term b)))
    ((δ (add1 b)) ,(add1 (term b)))
    ((δ (+ b_1 b_2)) ,(+ (term b_1) (term b_2)))
    ((δ (- b_1 b_2)) ,(- (term b_1) (term b_2)))
    ((δ (* b_1 b_2)) ,(* (term b_1) (term b_2)))
    ((δ (^ b_1 b_2)) ,(expt (term b_1) (term b_2))))
  (define-metafunction pcf-lang
    ((subst (λ X_1 any_2) X_1 any_1)
     (λ X_1 any_2))
    ((subst (λ X_2 any_2) X_1 any_1)
     (λ X_new
       (subst (subst-var any_2 X_2 X_new) X_1 any_1))
     (where X_new ,(variable-not-in (term (X_1 any_1 any_2))
                                    (term X_2))))
    ((subst X_1 X_1 any_1) any_1)
    ((subst (any_2 ...) X_1 any_1)
     ((subst any_2 X_1 any_1) ...))
    ((subst any_2 X_1 any_1) any_2))
  
  (define-metafunction pcf-lang
    ((subst-var (any_1 ...) variable_1 variable_2)
     ((subst-var any_1 variable_1 variable_2) ...))
    ((subst-var variable_1 variable_1 variable_2)
     variable_2)
    ((subst-var any_1 variable_1 variable_2) any_1))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TRACES
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (t1)
    (traces pcf-rr 
            (term ((λ X X) (λ Y Y)))))
  
  (define (t2)
    (traces pcf-rr
            (term ((λ w 6) 
                   (((λ X X) (λ Z Z))
                    ((λ Y Y) 5))))))
  
  (define (t3) 
    (traces pcf-rr (term (((λ X X) (λ Z Z)) 
                          ((λ Y Y) 5)))))
  
  (define (t4)
    (traces pcf-rr (term (((λ X (λ Y Y)) 
                           5) 
                          6 ))))
  
  (define (t5)
    (traces pcf-rr 
            (term (((λ X (λ Y Y)) (λ z ((λ X (X X))(λ X (X X))))) 2))))
  
  (define (t6)
    (traces pcf-rr 
            (term (((λ X (λ Y Y)) ((λ X (X X))(λ X (X X)))) 6 ))))
  
  (define (t7)
    (traces pcf-rr
            (term (iszero ((λ X 0) 5)))))
  
  (define (t8)
    (traces pcf-rr 
            (term (+ ((λ X 7) 5) ((λ Y 8) 3)))))
  
  )