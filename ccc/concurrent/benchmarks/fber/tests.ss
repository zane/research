#lang scheme 

  (require "../../contract.ss")

  (require "./ast.ss")

  (require "./evaluator.ss")


  (define (build-expr l)
   (map 
     (lambda (x) (make-nt x))
     (cond ((null? l) null)
           ((null? (cdr l)) l)
           (else
            (build-expr 
             (cons 
               (make-nd (car l) (cadr l))
               (build-expr (cddr l))))))))

  
 (time
  (master-start 
   (lambda ()
    (let* ((e 
           (car (build-expr 
                  (build-list 99999 (lambda (i) (make-nt (make-nd #t #f)))))))
         (body
           (car (build-expr 
                  (build-list 99999 (lambda (i) (make-nt (make-nd 'x 'x)))))))
         (fun (make-def 'f (list 'x) body)))
    (evaluator (make-appl 'f (list e)) (list fun))))))

