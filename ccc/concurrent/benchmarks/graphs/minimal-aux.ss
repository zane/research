(module minimal-aux scheme

        (require "../../contract.ss")
        (require "./util.ss")
        (require "./ptfold.ss")

      

(provide/contract 
  [t-folder-for-make-minimal
    (->d 
      ([size   integer?-and-exact?-and-positive?]
       [perm   (future/c vector?)]
       [folder (future/c procedure?)])
      ()
      [result
        (->d
          ([leaf-depth (future/c (lambda (x) (eqv? x size)))]
           [state any/c]
           [accross any/c])
          ()
          [result-final any/c])])])
                     
        
(define t-folder-for-make-minimal
  (lambda (size perm folder)
    (lambda (leaf-depth state accross)
			 (folder perm state accross))))

)

