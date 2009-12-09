;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname neis2009) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp")))))
(module neis2009 scheme
  (require redex "redex-util.ss")
  
  (define-language g-lang
    [τ α 
       β 
       (→ τ τ)
       (× τ τ)
       (∀ α τ)
       (∃ α τ)]
    [v x
       c
       (λ (: x τ) e)
       (cons v v)
       (λ α e)
       (pack τ v τ)]
    [e v
       (e e)
       (cons e e)
       (. e 1)
       (. e 2)
       (e τ)
       (pack τ e τ)
       (unpack α x e e)
       (cast τ τ)
       (new (≈ α τ) e)]
    [σ ((≈ α τ) ...)]
    [ζ (config σ e)]
    [Δ (O ...)]
    [O α
       (≈ α τ)]
    [Τ ((: x τ) ...)])
  
  (define-language secd-lang
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
    [N number]
    
    )
  