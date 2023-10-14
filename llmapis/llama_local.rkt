#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(define (question prompt)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \""
             prompt
             "\", \"n_predict\": 256, \"top_k\": 2}"))))
         (ignore (displayln prompt-data))
         (p
          (post
           "http://localhost:8080/completion"
           #:data prompt-data))
         (r (response-json p)))
    (hash-ref r 'content)))

(define (completion prompt)
  (question
   (string-append
    "Continue writing from the following text: "
    prompt)))


;; (displayln (question "Answer the question: Mary is 30 and Harry is 25. Who is older?"))
;; (displayln (completion "Frank bought a new sports car. Frank drove"))
