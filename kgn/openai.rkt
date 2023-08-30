#lang racket

(require net/http-easy)

(response-json
   (post "https://api.openai.com/v1/engines/davinci/completions"
        #:auth (lambda (uri headers params)
                 (values (hash
                                   'authorization "Bearer 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"
                                   'content-type "application/json"
                                   'data "{\"prompt\": \"Once upon a time\", \"max_tokens\": 5}")))))

