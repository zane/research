(module life-circle scheme

        (require "../../contract.ss")

        (require "./world.ss")

        (provide random-dna-base
                 random-m-dna-base
                 make-virus
                 make-cell
                 complement-chain)

        (provide/contract 
          (reproduce  (-> (lambda (x)
                            (or (mutated-cell? x) (valid-cell? x)))
                          any)))

        (define (reproduce c)
          (if (or (dead-cell? c) (mutated-cell? c))
            c
            (list (make-cell (complement-chain (cell-rchain c))
                             (complement-chain (cell-lchain c))))))

        (provide/contract 
          (die (-> (lambda (x)
                     (or (mutated-cell? x) (valid-cell? x)))
                    any)))

        (define (die c)
          (if (mutated-cell? c)
            (make-cell null null)
            c))

        (provide/contract 
          (mutate (-> (lambda (x)
                        (or (mutated-cell? x) (valid-cell? x)))
                      (future/c valid-virus?)
                      any)))

        (define (mutate cell virus)
          (let ((new-ldna (mutate-dna (cell-lchain cell) virus)))
            (make-cell new-ldna (complement-chain new-ldna))))

 ;;mutate helper functions

        ;;mutate-dna: m-dna-chain? virus -> m-dna-chain?
        (define (mutate-dna dna v)
          (cond ((null? dna) null)
                (else 
                  (if (symbol=? (car dna) (car (virus-tdna v)))
                    (let ((dna-rest (transform dna 
                                               (virus-tdna v) 
                                               (virus-ddna v)
                                               null 
                                               null)))
                      (append (car dna-rest) 
                              (mutate-dna (cdr dna-rest) v)))
                    (cons (car dna) (mutate-dna (cdr dna) v))))))


        ;;transform: m-dna-chai? m-dna-chain? m-dna-chain? m-dna-chain?
        ;;                     -> m-dna-chain? x m-dna-chai?
        (define (transform source target dest init-acc changed-acc)
          (cond ((null? source) (cons changed-acc  source))
                ((null? target ) (cons init-acc source))
                (else (if (symbol=? (car source) (car target))
                        (transform (cdr source)
                                   (cdr target)
                                   (cdr dest)
                                   (cons (car source) init-acc)
                                   (cons (car dest) changed-acc))
                        (cons init-acc source)))))


        
        
)
