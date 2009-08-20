(module minimal-aux scheme

        (require "./util.ss")
        (require "./ptfold.ss")


(provide t-folder-for-make-minimal)
        
;;(provide/contract 
;;  [t-folder-for-make-minimal
;;    (->d 
;;      ([size integer?-and-exact?-and-positive?]
;;       [perm vector?]
;;       [folder procedure?])
;;      ()
;;      [result
;;        (->d
;;          ([leaf-depth (lambda (x) (eqv? x size))]
;;           [state any/c]
;;           [accross any/c])
;;          ()
;;          [result-final any/c])])])
                     
        
(define t-folder-for-make-minimal
  (lambda (size perm folder)
    (lambda (leaf-depth state accross)
			 (folder perm state accross))))

)

