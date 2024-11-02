#lang racket

(require net/http-easy)
(require racket/set)
(require racket/pretty)
(require json)
(require net/url)
(require net/uri-codec)

(provide brave-search)

(define (brave-search query [num-results 3])
  (let* ([url "https://api.search.brave.com/res/v1/web/search"]
         [headers 
          (hash 'X-Subscription-Token (getenv "BRAVE_SEARCH_API_KEY")
                'Content-Type "application/json")]
         [params
          (list (cons 'q query)
                (cons 'count (number->string num-results)))]
         [response 
          (get url 
               #:headers headers
               #:params params)])
    
    (when (= (response-status-code response) 200)
      (let* ([json-response (response-json response)]
             [web-results (hash-ref (hash-ref json-response 'web) 'results)])
        (for/list ([result web-results])
          (hash
           'title (hash-ref result 'title)
           'url (hash-ref result 'url)
           'description (hash-ref result 'description)))))))

; Example usage
(pretty-print 
 (brave-search "Fun things to do in Sedona Arizona"))