#lang racket

(require net/http-easy)
(require racket/set)

(let* ((prompt "Mary is 30 and Harry is 25. Who is older?")
       (prompt-data (string-join (list "{\"prompt\": \"" prompt "\"}")))
       (auth (lambda (uri headers params)
                 (values
                  (hash-set* headers
                             'authorization "Bearer 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"
                             'content-type "application/json") params)))
       (p
        (post "https://api.openai.com/v1/engines/davinci/completions"
              #:auth auth
              ;;#:headers (make-hash '(("Content-Type" . "application/json")))
              ;;#:headers (list (hasheq "Content-Type" "application/json"))
              #:data prompt-data))
       (temp (println p))
       (r (response-json p)))
  (print r))