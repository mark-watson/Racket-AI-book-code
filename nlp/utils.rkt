#lang racket

(require racket/logging)

(provide path-to-data)

(define (path-to-data)
  (string->path "../nlp/data"))

(define count-substring 
  (compose length regexp-match*))

(define (string-count-substrings substr str)
  (length (regexp-match* substr str)))

#|
(display (string-count-substrings "bb" "aa bb cc bb dd"))
|#
