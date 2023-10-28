#lang racket

(require net/http-easy)
(require html-parsing)
(require net/url xml xml/path)
(require srfi/13) ;; for strings

(define (web-uri->xexp a-uri)
  (let* ((a-stream
          (get a-uri #:stream? #t))
         (lst (html->xexp (response-output a-stream))))
    (response-close! a-stream)
    lst))

(define (web-uri->text a-uri)
  (let* ((a-xexp
          (web-uri->xexp a-uri))
         (p-elements (se-path*/list '(p) a-xexp))
         (lst-strings
          (filter
           (lambda (s) (string? s))
           p-elements)))
    (string-normalize-spaces
     (string-join lst-strings "\n"))))

(define (web-uri->links a-uri)
  (let* ((a-xexp
          (web-uri->xexp a-uri)))
    ;; we want only external links so filter out local links:
    (filter
     (lambda (s) (string-prefix? "http" s))
     (se-path*/list '(href) a-xexp))))

;; (web-uri->xexp "https://knowledgebooks.com")
;; (web-uri->text "https://knowledgebooks.com")
;; (web-uri->links "https://knowledgebooks.com")