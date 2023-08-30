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

(pretty-print (take (se-path*/list '(p) lst) 8))

(displayln "\nLI text:\n")

(pretty-print (take (se-path*/list '(li) lst) 8))