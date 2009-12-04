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
  
  )
