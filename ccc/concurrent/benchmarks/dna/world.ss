(module world scheme

        (require "../../contract.ss")

        (provide (all-defined-out))


        ;; a base is one of
        ;; -- 'A
        ;; -- 'T
        ;; -- 'C
        ;; -- 'G
        
        ;;an m-base is one of
        ;; -- 'X
        ;; -- 'Y
        
        ;;base?: symbol? -> boolean?
        (define (base? b)
            (or (symbol=? 'A b)
                (symbol=? 'T b)
                (symbol=? 'C b)
                (symbol=? 'G b)))

        ;;m-base?: symbol? -> boolean?
        (define (m-base? b)
          (or (symbol=? 'X b)
              (symbol=? 'Y b)))


        ;;complement: symbol? symbol? -> boolean?
        (define (complement? b1 b2)
          (and (base? b1)
               (base? b2)
               (or (and (symbol=? b1 'A) (symbol=? b2 'T))
                   (and (symbol=? b1 'T) (symbol=? b2 'A))
                   (and (symbol=? b1 'C) (symbol=? b2 'G))
                   (and (symbol=? b1 'G) (symbol=? b2 'C))
                   (and (symbol=? b1 'X) (symbol=? b2 'Y))
                   (and (symbol=? b1 'Y) (symbol=? b2 'X)))))

        ;;a dna-chain is a [listof base?]
        
        ;;dna-chain?: [listof symbol?] --> boolean?
        (define dna-chain?
          (lambda (chain)
            (andmap base? chain)))

        ;;m-dna-chain?: [listof symbol?] --> boolean?
        (define m-dna-chain?
          (lambda (chain)
            (andmap (lambda (b) 
                      (or (base? b) (m-base? b))) chain)))



        ;;complement-dna?: dna-chain? dna-chain? --> boolean?
        (define (complement-dna? d1 d2)
          (andmap complement? d1 d2))
    

        (define-struct cell (rchain lchain))
        ;;a cell is (make-cell dna-chain? dna-chain?)
        ;; where (complement-dna? rchain lchain)
        
        

        ;;valid-cell?: cell? --> boolean?
        (define valid-cell?
          (lambda (c)
            (and (dna-chain? (cell-lchain c))
                 (complement-dna? (cell-lchain c) (cell-rchain c)))))

        ;;mutated-cell?: cell? --> boolean?
        (define mutated-cell?
          (lambda (c)
            (and (m-dna-chain? (cell-lchain c))
                 (complement-dna? (cell-lchain c) (cell-rchain c)))))

        ;;dead-cell?: cell? --> boolean?
        (define dead-cell?
          (lambda (c)
            (and 
              (null? (cell-lchain c))
              (null? (cell-rchain c)))))


        (define-struct virus (tdna ddna))
        ;; a virus is a (make-virus dna-chain?)

        ;;valid-virus?: virus? --> boolean?
        (define valid-virus?
          (lambda (v)
            (and
              (dna-chain? (virus-tdna v))
              (m-dna-chain? (virus-ddna v)))))
   
        ;;complement-chain: dna-chain? -> dna-chain?
        (define (complement-chain chain)
          (cond ((null? chain) chain)
                (else 
                  (let ((base (car chain)))
                    (cond ((symbol=? base 'A) (cons 'T 
                                                    (complement-chain
                                                      (rest chain))))
                          ((symbol=? base 'T) (cons 'A 
                                                    (complement-chain
                                                      (rest chain))))
                          ((symbol=? base 'C) (cons 'G 
                                                    (complement-chain
                                                      (rest chain))))
                          ((symbol=? base 'G) (cons 'C 
                                                    (complement-chain
                                                      (rest chain))))
                          ((symbol=? base 'X) (cons 'Y
                                                     (complement-chain
                                                       (rest chain))))
                          ((symbol=? base 'Y) (cons 'X
                                                     (complement-chain
                                                       (rest chain)))))))))

        ;;random-dna-base: () -> base?
        (define (random-dna-base j)
          (let ((base  (modulo j 4)))
            (cond ((= base 0) 'A)
                  ((= base 1) 'T)
                  ((= base 2) 'G)
                  (else 'C))))

         ;;random-m-dna-base: () -> base?
        (define (random-m-dna-base j)
          (let ((base  (modulo j 6)))
            (cond ((= base 0) 'A)
                  ((= base 1) 'T)
                  ((= base 2) 'G)
                  ((= base 3) 'C)
                  ((= base 4) 'X)
                  (else 'Y))))


              )
