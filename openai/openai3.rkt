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

;;(println (completion "Mary is 30 and Harry is 25. Who is older?"))

(define (question prompt)
  (let* ((prompt-data (string-join (list "{\"prompt\": \"Answer the question: " prompt "\" ,\"max_tokens\": 120, \"presence_penalty\": 0.0, \"top_p\": 1.0}")))
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

(println (question "Mary is 30 and Harry is 25. Who is older?"))
