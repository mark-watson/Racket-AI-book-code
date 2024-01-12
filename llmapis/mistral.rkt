#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(provide question-mistral completion-mistral embeddings-mistral)

;; Available models:
;;
;;   "mistral-tiny" powered by Mistral-7B-v0.2
;;   "mistral-small" powered Mixtral-8X7B-v0.1, a sparse mixture of experts model with 12B active parameters
;;   "mistral-medium" powered by a larger internal prototype model
;;

(define (question-mistral prompt)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"messages\": [ {\"role\": \"user\","
             " \"content\": \"Answer the question: "
             prompt
             "\"}], \"model\": \"mistral-small\"}"))))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set*
                   headers
                   'authorization
                   (string-join
                    (list
                     "Bearer "
                     (getenv "MISTRAL_API_KEY")))
                   'content-type "application/json")
                  params)))
         (p
          (post
           "https://api.mistral.ai/v1/chat/completions"
           #:auth auth
           #:data prompt-data))
         (r (response-json p)))
    (hash-ref
     (hash-ref (first (hash-ref r 'choices)) 'message)
     'content)))

(define (completion-mistral prompt)
  (question-mistral
   (string-append
    "Continue writing from the following text: "
    prompt)))

(define (embeddings-mistral text)
    (let* ((prompt-data
            (string-join
             (list
              (string-append
               "{\"input\": [\"" text "\"],"
               " \"model\": \"mistral-embed\"}"))))
           (auth (lambda (uri headers params)
                 (values
                  (hash-set*
                   headers
                   'authorization
                   (string-join
                    (list
                     "Bearer "
                     (getenv "MISTRAL_API_KEY")))
                   'content-type "application/json")
                  params)))
         (p
          (post
           "https://api.mistral.ai/v1/embeddings"
           #:auth auth
           #:data prompt-data))
         (r (response-json p)))
     (hash-ref
       (first (hash-ref r 'data))
       'embedding)))

;; (displayln (question-mistral "Mary is 30 and Harry is 25. Who is older?"))
;; (displayln (completion-mistral "Frank bought a new sports car. Frank drove"))
;; (displayln (embeddings-mistral "Frank bought a new sports car. Frank drove"))
;; (displayln (question-mistral "Mary is 30, Bob is 25, and Susan is 32. Describe all combinations of the three people comparing their ages, including the calculation of age differences. Be concise."))
