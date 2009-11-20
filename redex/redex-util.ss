(module redex-util scheme
  (require redex)
  (provide test-match)
  
  (define-syntax test-match
    (syntax-rules ()
      [(test-match lang id t)
       (test-equal (cond [(redex-match lang id t) 
                          => (λ (x) (andmap match? x))]
                         [else #f])
                   #t)])))