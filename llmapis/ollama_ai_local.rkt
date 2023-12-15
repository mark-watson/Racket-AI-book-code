#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(provide question-ollama-ai-local completion-ollama-ai-local)

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

(define (question-ollama-ai-local question)
  (helper (string-append "Answer: " question)))

(define (completion-ollama-ai-local prompt)
  (helper
   (string-append
    "Continue writing from the following text: "
    prompt)))


;;(displayln (question-ollama-ai-local "Mary is 30 and Harry is 25. Who is older and by how much?"))
;;(displayln (completion-ollama-ai-local "Frank bought a new sports car. Frank drove"))
;;(displayln (question-ollama-ai-local "Try on Mac with llama 2 13b: Mary is 30, Bob is 25, and Susan is 32. Describe all combinations of the three people comparing their ages, including the calculation of age differences. Be concise."))

;; EMBEDDINGS:

#|
curl http://localhost:11434/api/embeddings -d '{
  "model": "mistral:7b-instruct",
  "prompt": "Here is an article about llamas..."
}'
|#
