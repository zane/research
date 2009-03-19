(require (lib "1.ss" "srfi"))
(require (lib "64.ss" "srfi"))

;; 
(define (peval exp)
  (cond ((self-evaluating? exp) exp)
        ((lambda-expression? exp) (peval-lambda exp))
        (else (error "Problem!"))))

(define (lambda-expression? exp)
  (and (eqv? 'lambda (car exp))
       (list? (car (cdr exp)))))

(define (peval-lambda lambda-exp)
  (make-lambda (car (cdr lambda-exp))
               (car (cdr (cdr lambda-exp)))))

;; any -> boolean?
;;
;; Returns true if the argument is a self-evaluating expression or
;; returns false otherwise.
(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))

(test-begin "primitives")
(test-equal 1 (peval '1))
(test-equal "string" (peval '"string"))
(test-equal #t (lambda? (peval '(lambda (x) x))))
(test-end "primitives")

;; -> Environment
;;
;; Creates a new empty environment.
(define (empty-environment) empty)

(define (empty-environment? exp)
  (empty? exp))

;; any any Environment ->
;;
;; Extends the provided environment with the new binding of the
;; provided variable to the provided value.
(define (extend-environment var val env)
  (list 'environment var val env))

;; Environment -> any
(define (environment-var env)
  (car (cdr env)))

;; Environment -> any
(define (environment-val env)
  (car (cdr (cdr env))))

;; Environment -> Environment
(define (environment-environment env)
  (car (cdr (cdr (cdr env)))))

;; any -> boolean?
(define (environment? x)
  (or (empty-environment? x)
      (tagged-list? 'environment x)))

;; any Environment -> any
;;
;; Looks up the value for the provided variable in the provided
;; environment.
(define (lookup-variable-value var env)
  (cond ((empty-environment? env) (error "Problem!"))
        ((equal? var (environment-var env)) (environment-val env))
        (else (lookup-variable-value var (environment-environment env)))))

(test-begin "environments")
(test-equal #t (empty-environment? (empty-environment)))
(test-equal #f (empty-environment? 'not-the-empty-environment))
(test-equal #t (environment? (empty-environment)))
(test-equal 'bar
            (lookup-variable-value
             'foo
             (extend-environment
              'foo
              'bar
              (empty-environment))))
(test-equal 'bar
            (lookup-variable-value
             'foo
             (extend-environment
              'baz
              'qux
              (extend-environment
               'foo
               'bar
               (empty-environment)))))
(test-end "environments")

;; list? any -> Lambda
(define (make-lambda parameters body)
  (list 'lambda parameters body))

;; any -> boolean?
(define (lambda? x)
  (and (tagged-list? 'lambda x)
       (eqv? (length x) 3)))

(test-begin "lambdas")
(test-equal #f (lambda? 'lambda))
(test-equal #f (lambda? '(lambda params)))
(test-equal #t (lambda? (make-lambda '(x) 'x)))
(test-end "lambdas")


;; any -> boolean?
(define (tagged-list? tag lst)
  (and (list? lst)
       (equal? tag (car lst))))
