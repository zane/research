(module future mzscheme
  (require mzlib/etc)
  (provide (all-from-except mzscheme #%module-begin))
  
  (define-syntax (future-module-begin stx)
    (syntax-case stx ()
      ((future-module-begin form ...)
       #'(#%module-begin
          (let ([result             (begin-with-definitions
              form ...)])

            (display "start")
            (display "end"))))))
  
  (provide (rename future-module-begin #%module-begin)))


