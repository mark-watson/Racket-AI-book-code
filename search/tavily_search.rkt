#lang racket

(require net/http-easy)
(require racket/set)
 (require racket/pretty)
(require json)

(provide tavily-search)

(define (json-helper query)
  (format "{\"api_key\": \"~a\", \"query\": \"~a\", \"max_results\": 5}"
          (getenv "TAVILY_API_KEY")
          query))

(define (filter-response a-hash-eq)
  (list
   (hash-ref a-hash-eq 'url)
   (hash-ref a-hash-eq 'title)
   (hash-ref a-hash-eq 'content)))

(displayln (json-helper "1 + 2?"))
                       

(define (tavily-search query)
  (let* ((prompt-data (json-helper query))
         (p
          (post
           "https://api.tavily.com/search"
           #:data prompt-data))
         (r (response-json p)))
    ;;(displayln prompt-data)
    ;;(pretty-print r)
    (map filter-response (hash-ref r  'results))))

(pretty-print (tavily-search "Fun things to do in Sedona Arizona"))
