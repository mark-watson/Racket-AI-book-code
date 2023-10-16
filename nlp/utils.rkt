#lang racket

(require racket/logging)
(require racket/runtime-path)

(provide path-to-data)

(define-runtime-path my-data-path "data")

(define (path-to-data) my-data-path)
;;  (string->path "../nlp/data"))

(define count-substring 
  (compose length regexp-match*))

#|
(display (count-substring "bb" "aa bb cc bb dd"))
|#
