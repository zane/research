(module future mzscheme
  (require mzlib/etc)
  (require-for-syntax syntax/stx)
  (require-for-syntax scheme/list)
  (provide (all-from-except mzscheme #%module-begin))
  (provide (rename future-module-begin #%module-begin))
  
  (define-syntax (future-module-begin stx)
    (syntax-case stx ()
      ((_ form ...)
       (begin
         (display (let-values ([(require-forms non-require-forms)
                                (partition (lambda (stx)
                                             (and (stx-pair? stx)
                                                  (equal? (syntax-object->datum (stx-car stx))
                                                          'require)))
                                           (syntax->list (syntax (form ...))))])
                    (list require-forms non-require-forms)))
         (syntax (#%module-begin
                  (printf "start~n")
                  form ...
                  (printf "end~n"))))))))

#|
          (let ([result (begin-with-definitions
                          form ...)])
            (display "start")
            result
            (display "end")))))))
|#



