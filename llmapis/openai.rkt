#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(provide question-openai completion-openai embeddings-openai)

(define (question-openai prompt)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"messages\": [ {\"role\": \"user\","
             " \"content\": \"Answer the question: "
             prompt
             "\"}], \"model\": \"gpt-3.5-turbo\"}"))))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set*
                   headers
                   'authorization
                   (string-join
                    (list
                     "Bearer "
                     (getenv "OPENAI_API_KEY")))
                   'content-type "application/json")
                  params)))
         (p
          (post
           "https://api.openai.com/v1/chat/completions"
           #:auth auth
           #:data prompt-data))
         (r (response-json p)))
    (hash-ref
     (hash-ref (first (hash-ref r 'choices)) 'message)
     'content)))

(define (completion-openai prompt)
  (question-openai
   (string-append
    "Continue writing from the following text: "
    prompt)))

(define (embeddings-openai text)
    (let* ((prompt-data
            (string-join
             (list
              (string-append
               "{\"input\": \"" text "\","
               " \"model\": \"text-embedding-ada-002\"}"))))
           (auth (lambda (uri headers params)
                 (values
                  (hash-set*
                   headers
                   'authorization
                   (string-join
                    (list
                     "Bearer "
                     (getenv "OPENAI_API_KEY")))
                   'content-type "application/json")
                  params)))
         (p
          (post
           "https://api.openai.com/v1/embeddings"
           #:auth auth
           #:data prompt-data))
         (r (response-json p)))
     (hash-ref
       (first (hash-ref r 'data))
       'embedding)))

;; (displayln (question-openai "Mary is 30 and Harry is 25. Who is older?"))
;; (displayln (completion-openai "Frank bought a new sports car. Frank drove"))
;; (displayln (embeddings-openai "Frank bought a new sports car. Frank drove"))
