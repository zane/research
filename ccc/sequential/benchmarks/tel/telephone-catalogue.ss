(module telephone-catalogue scheme


        (require "../../contract.ss")

         (provide 
                 valid-telephone-entry?
                 valid-telephone-catalogue?
                 spine-telephone-entry?
                 gtq-telephone-entry
                 eq-telephone-entry
                 create-telephone-entry)

         ;;+ create-telephone-entry, telephone-entry->string


         ;;a name-string is a String that consists of alphabetic chars
        
         ;name-string?: string? -> boolean?
         (define name-string?
           (lambda (s)
             (and (string? s)
                  (andmap
                    char-alphabetic?
                    (string->list s)))))
  
          (define-struct telephone-number (description tel-no))
          ;;a telephone-number is (make-telephone-number string? number?)
          ;;where 10000000 <= tel-no <= 9999999999
          
          ;;valid-telephone-no?: number? -> boolean?
          (define valid-telephone-no?
            (lambda (no)
              (and (<= 1000000000 no) (<= no 9999999999))))

          
          ;;valid-telephone-nuber?:telephone-number? -> bookean?
          (define valid-telephone-number? 
            (lambda (tn)
              (and (string? (telephone-number-description tn))
                   (number? (telephone-number-tel-no tn))
                   (valid-telephone-no? (telephone-number-tel-no tn)))))

          (define-struct email (description address))
          ;;an email is (make-email string? string?)
          ;;where address has form x@y.z where x,y,z
          ;;are nane-string
          
          ;;valid-address: string? -> boolean?
          ;;returns #t if str is in the form x@y.z where
          ;;x,y,y are name-string
          (define (valid-address? str)
            (letrec
              ((valid-address?-aux
                 (lambda (s level)
                   (cond ((null? s) (= level 3))
                          (else 
                            (cond ((= level 1)
                                   (cond ((char-alphabetic? (car s))
                                          (valid-address?-aux (cdr s) level))
                                         ((char=? (car s) #\@)
                                          (valid-address?-aux (cdr s) 
                                                             (+ 1 level)))
                                         (else #f)))
                                  ((= level 2)
                                   (cond ((char-alphabetic? (car s))
                                          (valid-address?-aux (cdr s) level))
                                         ((char=? (car s) #\.)
                                          (valid-address?-aux (cdr s) 
                                                             (+ 1 level)))
                                         (else #f)))
                                  ((= level 3)
                                   (cond ((char-alphabetic? (car s))
                                          (valid-address?-aux (cdr s) level))
                                         (else #f)))))))))
              (valid-address?-aux (string->list str) 1)))


          ;;valid-email?: email? -> boolean?
          (define valid-email?
            (lambda (e)
              (and (email? e)
                   (string? (email-description e))
                   (string? (email-address e))
                   (valid-address? (email-address e)))))


          (define-struct telephone-entry (last-name 
                                          first-name 
                                          telephone-numbers
                                          emails))
          ;; a telephone-entry is a (make-telephone-entry 
          ;;                              name-string?
          ;;                              name-string?
          ;;                              (listof valid-telephone-number?)
          ;;                              (listof valid-emails?))
          

          ;;valid-telephone-entry?: telephone-entry? -> boolean?
          (define valid-telephone-entry?
            (lambda (te)
              (and (name-string? (telephone-entry-last-name te))
                   (name-string? (telephone-entry-first-name te))
                   (andmap (lambda (x) (valid-telephone-number? x))
                           (telephone-entry-telephone-numbers te))
                   (andmap (lambda (x) (valid-email? x))
                           (telephone-entry-emails te)))))

          (define (gtq-telephone-entry te1 te2)
            (or (string<? (telephone-entry-last-name te1)
                          (telephone-entry-last-name te2))
                 (and (string=?
                        (telephone-entry-last-name te1)
                        (telephone-entry-last-name te2))
                      (string<=? 
                        (telephone-entry-first-name te1)
                        (telephone-entry-first-name te2)))))


          (define (eq-telephone-entry te1 te2)
            (and (string=?  (telephone-entry-last-name te1)
                            (telephone-entry-last-name te2))
                  (string=? (telephone-entry-first-name te1)
                            (telephone-entry-first-name te2))))

          (define (merge-telephone-entry te1 te2)
            (make-telephone-entry 
              (telephone-entry-last-name te1)
              (telephone-entry-first-name te1)
              (append
                (telephone-entry-telephone-numbers te1)
                (telephone-entry-telephone-numbers te2))
              (append
                (telephone-entry-emails te1)
                (telephone-entry-emails te2))))


          ;spine-telephone-entry: telephone-entry? -> boolean?
          (define (spine-telephone-entry? te)
            (and (null? (telephone-entry-telephone-numbers te))
                 (null? (telephone-entry-emails te))))

          ;;sorted-and-assoc-telephone-entry-list?: 
          ;;     [listof telephone-entry?] -> boolean?
          (define sorted-and-assoc-telephone-entry-list?
            (lambda (tel)
              (cond ((null? tel) #t)
                    ((null? (cdr tel)) #t)
                    (else 
                      (if (gtq-telephone-entry (car tel) (cadr tel))
                        (sorted-and-assoc-telephone-entry-list? (cdr tel))
                        #f)))))

                   
          ;;a telephone-catalogue is an assoc [listof telephone-entry?]
          ;;sorted in descending order by the last and first name of the
          ;;telephone entries

          ;;valid-telephone-catalogue?: 
          ;;     [listof valid-telephone-entry?] -> boolean
          (define valid-telephone-catalogue? 
            (lambda (tc)
                 (and  (andmap valid-telephone-entry? tc)
                       (sorted-and-assoc-telephone-entry-list? tc))))

          ;;create-telephone-numbers:
          ;;  [listof (string? x number?)] -> [listof telephone-number?]
          (define (create-telephone-numbers tel-nos)
            (map (lambda (tn)
                   (make-telephone-number (car tn) (cdr tn)))
                 tel-nos))

           ;;create-emails:
           ;;  [listof (string? x string?)] -> [listof email?]
           (define (create-emails emails)
             (map (lambda (e)
                    (make-email (car e) (cdr e)))
                   emails))
 
       
        (define (create-telephone-entry last-name first-name tel-nos emails)
         (make-telephone-entry
           last-name
           first-name
           (create-telephone-numbers tel-nos)
           (create-emails emails)))

        

       ;;telephone-numbers->string:
       ;;  [listof telephone-number?] -> string?
       (define (telephone-numbers->string tel-nums)
         (foldr
           string-append
           ""
           (map
             (lambda (tn)
               (string-append
                 (telephone-number-description tn)
                 "~n"
                 (number->string (telephone-number-tel-no tn))
                 "~n"))
             tel-nums)))

       ;;emails->string:
       ;;  [listof telephone-number?] -> string?
       (define (emails->string emails)
         (foldr
           string-append
           ""
           (map
             (lambda (e)
               (string-append
                 (email-description e)
                 "~n"
                 (email-address e)
                 "~n"))
             emails)))

        (provide telephone-entry->string)

        ;;(provide/contract
        ;;  (telephone-entry->string (-> valid-telephone-entry? string?)))


          (define (telephone-entry->string te)
            (string-append
              "Telephone-entry:~n"
              "last name:~n"
              (telephone-entry-last-name te)
              "first name:~n"
              (telephone-entry-last-name te)
              "telephone numbers:~n"
              (telephone-numbers->string (telephone-entry-telephone-numbers te))
              "emails:~n"
              (emails->string (telephone-entry-emails te)))) 

 )




