#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(define (helper prompt)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \""
             prompt
             "\", \"model\": \"mistral\", \"stream\": false}"))))
         (ignore (displayln prompt-data))
         (p
          (post
           "http://localhost:11434/api/generate"
           #:data prompt-data))
         (r (response-json p)))
    (hash-ref r 'response)))

(define (question question)
  (helper (string-append "Answer: " question)))

(define (completion prompt)
  (helper
   (string-append
    "Continue writing from the following text: "
    prompt)))


(displayln (question "Mary is 30 and Harry is 25. Who is older and by how much?"))
(displayln (completion "Frank bought a new sports car. Frank drove"))
