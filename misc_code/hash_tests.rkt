#lang racket

(define h1 (hash "dog" '("friendly" 5) "cat" '("not friendly" 2))) ;; not mutable

(define cat (hash-ref h1 "cat"))

(define h2 (make-hash)) ;; mutable
(hash-set! h2 "snake" '("smooth" 4))

;; make-hash also accepts a second argument that is a list of pairs:
(define h3 (make-hash '(("dog" ("friendly" 5)) ("cat" ("not friendly" 2)))))
(hash-set! h3 "snake" '("smooth" 4))
(hash-set! h3 "cat" '("sometimes likeable" 3)) ;; overwrite key value

;; for can be used with hash tables:

(for ([(k v) h3]) (println (list "key:" k "value:" v)))
