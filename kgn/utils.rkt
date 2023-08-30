#lang racket

(require racket/logging)

(provide path-to-data)
(provide my-log-info)

(define (path-to-data)  ;; (path->string (find-system-path 'run-file))
  (let ((inside-dev-env? (member (string->path "data") (directory-list (current-directory)))))
    (if inside-dev-env?
        (string->path "data")
        ;; NOTE: if the application name "kgn" changes, you must edit the following line!!!!
        (string->path (string-replace (path->string (find-system-path 'run-file)) "MacOS/kgn" "Resources/data")))))

(define (my-log-info message data)
  (with-output-to-file "racket_log.txt"
    (λ () (printf "~a: ~a\n" message data))
    #:exists 'append))
#|
(my-log-info "path to data" (path->string (path-to-data)))
(my-log-info "path of run file" (path->string (find-system-path 'run-file)))
|#

(define count-substring 
  (compose length regexp-match*))

(define (string-count-substrings substr str)
  (length (regexp-match* substr str)))
#|
(display (string-count-substrings "bb" "aa bb cc bb dd"))
|#
