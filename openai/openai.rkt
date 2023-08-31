#lang racket

(require net/http-easy)
(require racket/set)

'(let* ((prompt "Mary is 30 and Harry is 25. Who is older?")
       (prompt-data (string-join (list "{\"prompt\": \"" prompt "\" ,\"max_tokens\": 120}")))
       (temp0 (displayln prompt-data))
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

(define (completion prompt)
  (let* ((prompt-data (string-join (list "{\"prompt\": \"" prompt "\" ,\"max_tokens\": 120}")))
         (temp0 (displayln prompt-data))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set* headers
                             'authorization "Bearer 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"
                             'content-type "application/json") params)))
         (p
          (post "https://api.openai.com/v1/engines/davinci/completions"
                #:auth auth
                #:data prompt-data))
         (temp (println p))
         (r (response-json p)))
    (println r)
    (hash-ref r 'choices)))

;;(println (completion "Frank bought a new sports car. Frank drove"))

(define (question prompt)
  (let* ((prompt-data (string-join (list "{\"messages\": [ {\"role\": \"user\", \"content\": \"Answer the question: "
                                         prompt
                                         "\"}], \"model\": \"gpt-3.5-turbo\"}")))
         (temp0 (displayln prompt-data))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set* headers
                             'authorization "Bearer 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"
                             'content-type "application/json") params)))
         (p
          (post "https://api.openai.com/v1/chat/completions" ;; "https://api.openai.com/v1/engines/davinci/completions"
                #:auth auth
                #:data prompt-data))
         (temp (println p))
         (r (response-json p)))
    (println r)
    (hash-ref (hash-ref (first (hash-ref r 'choices)) 'message) 'content)))

(define answer2 (question "Mary is 30 and Harry is 25. Who is older?"))
answer2
