(module lib scheme
  
  (define (pretty-printer s)
    (printf (string-append s "~n")))
  
  (provide pretty-printer))
