#lang scheme

(begin 
 (printf "hi1~n")
 (thread-wait (thread (lambda () (printf "hi2~n") (flush-output))))
 (printf "hi3~n")
 ((lambda () (printf "hi4~n") (flush-output))))

