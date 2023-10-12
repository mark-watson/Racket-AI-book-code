#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(provide question completion)

(define (question prompt max-tokens)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \"\\n\\nHuman: "
             prompt
             "\\n\\nAssistant: \", \"max_tokens_to_sample\": "
             (number->string  max-tokens)
             ", \"model\": \"claude-instant-1\" }"))))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set*
                   headers
                   'x-api-key
                     (getenv "ANTHROPIC_API_KEY")
                   'anthropic-version "2023-06-01"
                   'content-type "application/json")
                  params)))
         (p
          (post
           "https://api.anthropic.com/v1/complete"
           #:auth auth
           #:data prompt-data))
         (r (response-json p)))
    (string-trim (hash-ref r 'completion))))

(define (completion prompt max-tokens)
  (question
   (string-append
    "Continue writing from the following text: "
    prompt)
   max-tokens))


;; (displayln (question "Mary is 30 and Harry is 25. Who is older?" 20))
;; (displayln (completion "Frank bought a new sports car. Frank drove" 200))