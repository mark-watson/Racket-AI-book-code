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

;;(define lst-li (take (se-path*/list '(li) lst) 20))
;;(displayln "\nLI text:\n")
;;(pretty-print lst-li)

(define hrefs (se-path*/list '(href) lst))

(pretty-print hrefs)