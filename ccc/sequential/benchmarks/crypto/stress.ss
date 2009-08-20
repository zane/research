#lang scheme/load

;; specify some reasonably large text file below:  "crypt.lp"

;; with contracts: cpu time: 27348 real time: 27663 gc time: 604
;; without contracts: cpu time: 7762 real time: 7903 gc time: 480

;; =============================================================================
(module server scheme
  (require test-engine/scheme-tests)

  ; [Listof X] [Listof Nat] -> [Listof X]
  ; order list s according to indices in list i

  (check-expect (reorder '(a b c) '(1 2 0)) '(c a b))

  (define (reorder s i)
    (map second (sort (map list i s) <= #:key first)))

  (test)

  (require scheme/contract)

 (provide reorder))

;; (provide/contract
;;  [reorder (->d ((s (listof any/c))
;;                 (i (and/c (listof natural-number/c)
;;                           (curry andmap (curry > (length s))))))
;;                 ()
;;                 (r (and/c (listof any/c)
;;                           (curry andmap (lambda (t) (member t s))))))]))

;; =============================================================================
(module client scheme
  (require scheme
           test-engine/scheme-tests
           (only-in srfi/1 take drop)
           scheme/contract
           'server)


  (provide encrypt decrypt)
  
  ;;(provide/contract
  ;; [encrypt (-> string? (and/c natural-number/c positive?) string?)]
  ;; [decrypt (-> string? (and/c natural-number/c positive?) string?)])


  ;; String Positive -> String
  ;; encrypt @scheme[str] via waves of height @scheme[h]
  (check-expect (encrypt "diesisteinklartext" 6) "dkinleiasertittxse")
  (define (encrypt str h)
    (list->string (wave (string->list str) h)))

  ;; String Positive -> String
  ;; decrypt @scheme[str], which was created via waves of height @scheme[h]

  (check-expect (decrypt "dkinleiasertittxse" 6) "diesisteinklartext")

  (define (decrypt str h)
    (define w (wave (build-list (string-length str) (lambda (x) x)) h))
    (list->string (reorder (string->list str) w)))

  ;; [Listof X] Positive -> [Listof X]
  ;; move elements of list via intermediate waves of height @scheme[h]

  (check-expect (wave (list 1 2 3 4 5 6) 3)
                (list 1 5 2 4 6 3))
  (check-expect (wave (list 0 1 2 3 4 5 6 7 8 9) 6)
                (list 0 1 9 2 8 3 7 4 6 5))

  ;; the first "down wave" is all there is
  (check-expect (wave '(a b c) 3) '(a b c))

  ;; there is just one more item for the "up wave"
  (check-expect (wave '(a b c d) 3) '(a b d c))

  ;; ... and two more:
  (check-expect (wave '(a b c d e) 3) '(a e b d c))

  ;; finally: a complete "down wave" and a complete "up wave"
  (check-expect (wave '(a b c d e f) 3) '(a e b d f c))
  
  
  (define (make-list length el)
    (if (= length 0) null (cons el (make-list (- length 1) el))))

  (define (wave lox i)
    (define UNI '_)
    (define WVL (- (* 2 i) 2))
    ;; [Listof X] -> [Listof [Listof (U X UNI)]]
    ;; create lists of "down-and-up waves"
    ;; @bold{termination:} each recursion drops @scheme[h] items and @scheme[(> h 0)]
    (define (wave lox)
      (define L (length lox))
      (cond
        [(<= WVL L) (append (down-and-up lox) (wave (drop lox WVL)))]
        [else (down-and-up (append lox (make-list (- WVL L) UNI)))]))
    ;; [Listof X] -> (list [Listof (U X UNI)] [Listof (U X UNI)])
    ;; create one "down-and-up wave"
    (define (down-and-up lox)
      (list (take-and-pad lox) (reverse (take-and-pad (drop lox (- i 1))))))
    ;; [Listof X] -> [Listof (U X UNI)]
    (define (take-and-pad lox)
      (append (take lox (- i 1)) (list UNI)))
    ;; now transpose, concatenate, and remove the @scheme[UNI] item
    (filter (lambda (x) (not (eq? UNI x)))
            (apply append (transpose (wave lox)))))
 
  ;; [Listof [Listof X]] -> [Listof [Listof X]]
  ;; transpose the matrix

  (check-expect (transpose '((1 2 3) (a b c) (^ & *)))
                '((1 a ^) (2 b &) (3 c *)))


  (define (transpose m) (apply map list m))

  ;; Nat Nat -> [Listof Nat]

  (check-expect (range 2 4) '(2 3 4))
  (check-expect (range 4 2) '(4 3 2))

  (define (range lo hi)
    (if (>= hi lo)
        (build-list (+ (- hi lo) 1) (lambda (i) (+ lo i)))
        (build-list (+ (- lo hi) 1) (lambda (i) (- lo i)))))

  (test))

(module stress scheme
  (require 'client)

  (define (read-all)
    (apply string-append
           (with-input-from-file "stress.ss"
             (lambda ()
               (let L ()
                 (define nxt (read-line))
                 (if (eof-object? nxt) '() (cons nxt (L))))))))

  (define text (read-all))
  (define lin* (+ 1 3))

  (time
   (for ((i (in-range 100)))
     (string=? (decrypt (encrypt text lin*) lin*) text))))

(require 'stress)

 
  
