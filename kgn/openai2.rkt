#lang racket

(require net/http-easy)
(require racket/set)

(let* ((p
        (post "https://api.openai.com/v1/engines/davinci/completions"
              #:auth (bearer-auth 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)
              #:headers 
              #:form '((
              
              #:auth (lambda (uri headers params)
                       (println headers)
                       (println params)
                       (values (hash-set* headers
                                          'authorization "Bearer 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"
                                          'content-type "application/json"
                                          'data "{\"prompt\": \"Once upon a time\", \"max_tokens\": 5}")))))
       (temp (println p))
       (r (response-json p)))
  (print r))

