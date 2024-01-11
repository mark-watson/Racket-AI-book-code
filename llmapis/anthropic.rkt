#lang racket

(require net/http-easy)
(require racket/set)

(provide question-anthropic completion-anthropic)

(define (question-anthropic prompt max-tokens)
  (let* ((prompt-data
          (string-join
           (list
            (string-append
             "{\"prompt\": \"\\n\\nHuman: "
             prompt
             "\\n\\nAssistant: \", \"max_tokens_to_sample\": "
             (number->string  max-tokens)
             ", \"model\": \"claude-2.1\" }"))))
;;             ", \"model\": \"claude-instant-1\" }"))))
         (zz (println prompt-data))
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
    (println r)
    (println p)
    (string-trim (hash-ref r 'completion))))

(define (completion-anthropic prompt max-tokens)
  (question-anthropic
   (string-append
    "Continue writing from the following text: "
    prompt)
   max-tokens))


(define (step-by-step-anthropic document prompt max-tokens) ;; work in progress
  (question-anthropic
   (string-append
     "Human: Answer the following question only if you know the answer or can make a well-informed guess; otherwise tell me you don't know it.\\n"
     "\\nHuman: When you reply, first find exact quotes in the FAQ relevant to the user's question and write them down word for word inside <thinking></thinking> XML tags.  This is a space for you to write down relevant content and will not be shown to the user.  Once you are done extracting relevant quotes, answer the question.  Put your answer to the user inside <answer></answer> XML tags.\\n"
     "\\nHuman: When answering my questions, please think things out step by step and break complex tasks into subtasks.\\n"
     "Please use this document text " document "\\n"
    "Please help with: "
    prompt)
   max-tokens))


;; (displayln (question-anthropic "Mary is 30 and Harry is 25. Who is older?" 40))
;; (displayln (completion-anthropic "Frank bought a new sports car. Frank drove" 200))
;; (step-by-step-anthropic "KBS is an AI conpany specializing in NLP, and LLMS. We use Lisp languages and Python. We offer low prices. Mark Watson is the president." "Write an execuative summary for a KBS business plan." 600)
;; (step-by-step-anthropic "MWA is an AI conpany specializing in NLP, and LLMS. We use Lisp languages and Python. Visit our web site https://markwatson.com" "Write an execuative summary for a KBS business plan." 600)

;;(question-anthropic "Mary is 30 and Harry is 25. Who is older by how much? Be concise." 40)