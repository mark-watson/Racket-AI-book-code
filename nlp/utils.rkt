#lang racket

(require racket/logging)

(provide path-to-data)

(define (path-to-data)
  (string->path "../nlp/data"))

(define (log-info message data)
  (with-output-to-file "/Users/markw/racket_log.txt"
    (λ () (printf "~a: ~a\n" message data))
    #:exists 'append))
#|
(log-info "path to data" (path->string (path-to-data)))
(log-info "path of run file" (path->string (find-system-path 'run-file)))
|#

(define count-substring 
  (compose length regexp-match*))

(define (string-count-substrings substr str)
  (length (regexp-match* substr str)))
#|
(display (string-count-substrings "bb" "aa bb cc bb dd"))
|#
