#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(define (question prompt)
  (let* ((prompt-data (string-join (list "{\"messages\": [ {\"role\": \"user\", \"content\": \"Answer the question: "
                                         prompt
                                         "\"}], \"model\": \"gpt-3.5-turbo\"}")))
         ;;(temp0 (displayln prompt-data))
         (auth (lambda (uri headers params)
                 (values
                  (hash-set* headers
                             'authorization (string-join (list "Bearer " (getenv "OPENAI_API_KEY")))
                             'content-type "application/json") params)))
         (p
          (post "https://api.openai.com/v1/chat/completions" ;; "https://api.openai.com/v1/engines/davinci/completions"
                #:auth auth
                #:data prompt-data))
         ;;(temp (println p))
         (r (response-json p)))
    ;;(println r)
    (hash-ref (hash-ref (first (hash-ref r 'choices)) 'message) 'content)))

(define answer2 (question "Mary is 30 and Harry is 25. Who is older?"))
answer2


(define (completion prompt)
  (question (string-append "Continue writieng from the following text: " prompt)))

(println (completion "Frank bought a new sports car. Frank drove"))
