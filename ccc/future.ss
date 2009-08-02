(module future mzscheme
  (require mzlib/etc)
  (require-for-syntax syntax/stx)
  (require-for-syntax scheme/list)
  (provide (all-from-except mzscheme #%module-begin))
  (provide (rename future-module-begin #%module-begin))
  
  (define-syntax (future-module-begin stx)
    (syntax-case stx ()
      ((_ form ...)
       (let-values ([(require-forms non-require-forms)
                     (partition (lambda (stx)
                                  (and (stx-pair? stx)
                                       (equal? (syntax-object->datum (stx-car stx))
                                               'require)))
                                (syntax->list (syntax (form ...))))])
         (quasisyntax (#%module-begin
                       #,@require-forms
                       (printf "start~n")
                       (let ([result (begin-with-definitions
                         #,@non-require-forms)])
                         (printf "result is: ~a~n" result))
                       (printf "end~n"))))))))



