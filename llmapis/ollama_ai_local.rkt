#lang racket

(require net/http-easy)
(require racket/set)
(require pprint)

(provide question-ollama-ai-local completion-ollama-ai-local)

(define (helper prompt . model-name)
  (displayln (list "Model name: " model-name))
  (let* ((model
          (if (equal? model-name '()) "mistral" (first (first model-name))))
         (prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \""
             prompt
             "\", \"model\": \"" model "\", \"stream\": false}"))))
         ;;(ignore (displayln prompt-data))
         (p
          (post
           "http://localhost:11434/api/generate"
           #:data prompt-data))
         (r (response-json p)))
    ;;(displayln r)
    (hash-ref r 'response)))

(define (question-ollama-ai-local question . model-name)
  (helper (string-append "Answer: " question) model-name))

(define (completion-ollama-ai-local prompt . model-name)
  (helper
   (string-append
    "Continue writing from the following text: "
    prompt)
    model-name))


;;(displayln (question-ollama-ai-local "Mary is 30 and Harry is 25. Who is older and by how much?"))
;;(displayln (question-ollama-ai-local "Mary is 30 and Harry is 25. Who is older and by how much?" "mixtral:8x7b-instruct-v0.1-q2_K"))
;;(displayln (completion-ollama-ai-local "Frank bought a new sports car. Frank drove"))
;; The default mistral-7b model almost always gets the following wrong:
;;(displayln (question-ollama-ai-local "Mary is 30, Bob is 25, and Susan is 32. Describe all combinations of the three people comparing their ages, including the calculation of age differences. Be concise."))
;; Here we try the mixtral:8x7b-instruct-v0.1-q2_K model (2 bit quantization!) that sometimes gets the follwoing correct:
;;(displayln (question-ollama-ai-local "Mary is 30, Bob is 25, and Susan is 32. Describe all combinations of the three people comparing their ages, including the calculation of age differences. Be concise."  "mixtral:8x7b-instruct-v0.1-q2_K"))
;; Here we try the dolphin-mixtral:8x7b-v2.5-q3_K_S model (3 bit quantization!) that does better:
;;(displayln (question-ollama-ai-local "Mary is 30, Bob is 25, and Susan is 32. Describe all combinations of the three people comparing their ages, including the calculation of age differences. Be concise."  "dolphin-mixtral:8x7b-v2.5-q3_K_S"))
;; Here we try the Chineese yi:34b model:
;;(displayln (question-ollama-ai-local "Mary is 30, Bob is 25, and Susan is 32. Describe all combinations of the three people comparing their ages, including the calculation of age differences. Be concise."  "yi:34b"))

;; Meta's new llama3-instruct-8b model:

;;(displayln (question-ollama-ai-local "Mary is 30 and Harry is 25. Who is older and by how much?" "llama3:instruct"))

;; EMBEDDINGS:

(define (embeddings-ollama text)
    (let* ((prompt-data
            (string-join
             (list
              (string-append
               "{\"prompt\": \"" text "\","
               " \"model\": \"mistral\"}"))))
           (p
            (post
             "http://localhost:11434/api/embeddings"
             #:data prompt-data))
           (r (response-json p)))
      (hash-ref r 'embedding)))


;; (embeddings-ollama "Here is an article about llamas...")

#|
curl http://localhost:11434/api/embeddings -d '{
  "model": "mistral:7b-instruct",
  "prompt": "Here is an article about llamas..."
}'

curl http://localhost:11434/api/embeddings -d '{
  "model": "mistral",
  "prompt": "Here is an article about llamas..."
}'

|#
