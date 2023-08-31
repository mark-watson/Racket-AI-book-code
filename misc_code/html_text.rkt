#lang racket

(require net/http-easy)
(require html-parsing)
(require net/url xml xml/path)
(require racket/pretty)

(define res-stream
  (get "https://markwatson.com" #:stream? #t))

(define lst
  (html->xexp (response-output res-stream)))

(response-close! res-stream)

(displayln "\nParagraph text:\n")

(define lst-p (se-path*/list '(p) lst))
(displayln "\nlst-p:\n")
(pretty-print lst-p)

(define lst-strings
  (filter
   (lambda (s) (string? s))
   lst-p))

(displayln "\nlst-strings:\n")
(pretty-print lst-strings)

(define one-string
  (string-normalize-spaces
   (string-join lst-strings "\n")))
