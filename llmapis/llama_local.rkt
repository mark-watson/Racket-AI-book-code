#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(provide question-llama-local completion-llama-local)

(define (helper prompt)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \""
             prompt
             "\", \"n_predict\": 256, \"top_k\": 1}"))))
         (ignore (displayln prompt-data))
         (p
          (post
           "http://localhost:8080/completion"
           #:data prompt-data))
         (r (response-json p)))
    (hash-ref r 'content)))

(define (question-llama-local question)
  (helper (string-append "Answer: " question)))

(define (completion-llama-local prompt)
  (helper
   (string-append
    "Continue writing from the following text: "
    prompt)))


;;(displayln (question-llama-local "Mary is 30 and Harry is 25. Who is older?"))
;;(displayln (completion-llama-local "Frank bought a new sports car. Frank drove"))
