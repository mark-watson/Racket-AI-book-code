#lang racket

(require racket/hash
         racket/contract
         racket/sequence)

(require rackunit)

(provide (all-defined-out))

;; -----------------------------------------------------------------------------
;; Context (Blackboard)

(define (make-context)
  "Creates a new, empty context (blackboard)."
  (make-hash))

(define (context-set! context key value)
  "Sets a key-value pair in the context."
  (hash-set! context key value))

(define (context-get context key #:default default-value)
  "Gets the value associated with a key in the context.
  Returns default-value if the key is not found."
  (hash-ref context key default-value))

(define (context-contains? context key)
  "Checks if the context contains a key."
  (hash-has-key? context key))

(define (context-keys context)
  "Returns a sequence of keys in the context."
  (hash-keys context))

(define (context-values context)
  "Returns a sequence of values in the context."
  (hash-values context))

(define (context-remove! context key)
  "Removes a key-value pair from the context."
  (hash-remove! context key))

(define (context-clear! context)
  "Clears all key-value pairs from the context."
  (hash-clear! context))

(define (context-copy context)
  "Creates a shallow copy of the context."
  (hash-copy context))


;; -----------------------------------------------------------------------------
;; Tool Management

(define (register-tool context tool-name tool-function)
  "Registers a tool (Racket function) in the context for use by agents.
  `tool-name`: A symbol representing the tool's name (used in LLM prompts).
  `tool-function`: A Racket function that implements the tool."
  (context-set! context 'tools (hash-set (context-get context 'tools (make-hash))
                                          tool-name tool-function)))

(define (get-tool context tool-name)
  "Retrieves a tool function from the context by its name.
  Returns #f if the tool is not registered."
  (hash-ref (context-get context 'tools (make-hash)) tool-name #f))


;; -----------------------------------------------------------------------------
;; Agent Framework

(define (make-agent #:name [name "agent"] #:instructions [instructions "You are a helpful agent."] #:process-fn [process-fn agent-default-process-fn])
  "Creates a new agent.
  #:name: Agent's name (string).
  #:instructions: Initial instructions for the agent (string).
  #:process-fn: A function that defines the agent's processing logic.
                Defaults to `agent-default-process-fn`."
  (struct agent (name instructions process-fn))
  (agent name instructions process-fn))

(struct agent (name instructions process-fn) #:transparent)


(define (agent-process agent prompt context)
  "Processes a prompt with the agent in the given context.
  Applies the agent's `process-fn` to handle the interaction.
  Returns a list: (values response updated-context)."
  ((agent-process-fn agent) agent prompt context))


(define (agent-default-process-fn agent)
  "The default agent processing function.
  This function simulates a basic agent that:
  1. Sends a prompt to the LLM (simulated `openai-api-call`).
  2. Checks for tool calls in the LLM response (very basic parsing).
  3. Executes tools if requested and updates the context.
  4. Returns the LLM response and updated context."
  (lambda (agent prompt context)
    (define full-prompt (string-append (agent-instructions agent) "\n" prompt))
    (define llm-response (simulated-openai-api-call full-prompt context)) ; Call simulated LLM

    (define tool-call-request (parse-tool-call-request llm-response)) ; Check for tool call request

    (if tool-call-request
        (let* ([tool-name (car tool-call-request)]
               [tool-args (cdr tool-call-request)]
               [tool-function (get-tool context tool-name)])
          (if tool-function
              (let* ([tool-result (apply tool-function tool-args)] ; Execute the tool
                     [updated-context (context-copy context)]) ; Create a new context for potential updates
                (context-set! updated-context 'tool-result tool-result) ; Store tool result in context
                (values (string-append "Tool '" (symbol->string tool-name) "' called. Result in context under 'tool-result'.") updated-context))
              (values (string-append "Error: Tool '" (symbol->string tool-name) "' not found.") context)))
        (values llm-response context)))) ; Return LLM response if no tool call


;; -----------------------------------------------------------------------------
;; Tool Call Parsing (Simple Example)

(define (parse-tool-call-request response-text)
  "Parses the LLM response to check for a tool call request.
  ... (rest of the documentation as before) ..."
  (define (trim-whitespace str)
    (regexp-replace* #rx"^\\s+|\\s+$" str ""))

  (define (parse-args arg-str)
    (string-split arg-str " "))

  (define call-tool-regex #rx"\\(call-tool\\s+([^\\)]+)\\)")

  (let ([match-result (regexp-match call-tool-regex response-text)])
    (if match-result ; Check if match-result is not #f (i.e., a match was found)
        (let* ([tool-call-str (cadr match-result)] ; Use match-result here
               [parts (string-split tool-call-str " " #:trim? #t)]
               [tool-name (and (not (null? parts)) (string->symbol (car parts)))])
          (if tool-name
              (cons tool-name (map string->symbol (cdr parts)))
              #f))
        #f))) ; If match-result is #f, return #f


;; -----------------------------------------------------------------------------
;; Simulated OpenAI API and Web Search (Placeholders)

(define (simulated-openai-api-call prompt context)
  "Simulates a call to the OpenAI API.
  For demonstration purposes, it provides canned responses and can trigger tool calls.
  In a real application, you would replace this with an actual OpenAI API library call."
  (displayln (string-append "Simulated OpenAI API Call with prompt: " prompt))

  (cond
    [(string-contains? prompt "search for current weather in London")
     "(call-tool web-search \"current weather in London\")"] ; Request web_search tool
    [(string-contains? prompt "calculate 2 + 2")
     "(call-tool racket-eval \"(+ 2 2)\")"] ; Request racket-eval tool
    [(string-contains? prompt "tell me a joke")
     "Why don't scientists trust atoms? Because they make up everything!"]
    [else
     "Default response: I received your prompt and processed it."]))


(define (web-search query)
  "Simulated web search function.
  In a real application, this would perform an actual web search.
  For demonstration, it returns canned results."
  (displayln (string-append "Simulated web_search: " query))
  (cond
    [(string-contains? query "weather in London")
     "Simulated web search result: The current weather in London is sunny with a temperature of 20 degrees Celsius."]
    [else
     "Simulated web search result: No specific results found for query: " query]))


(define (racket-eval code)
  "Simulated Racket evaluation tool.
  In a real application, be extremely careful about security when evaluating arbitrary code.
  This is for demonstration purposes only and should be used with caution."
  (displayln (string-append "Simulated racket-eval: " code))
  (eval (read (open-input-string code)) (make-base-namespace))) ; Be very careful with `eval` in real systems!


;; -----------------------------------------------------------------------------
;; Example Usage

(define (test)
    (define ctx (make-context))
    (context-set! ctx 'name "test-context")
    (println (context-get ctx 'name #:default "not found"))

    (define agent (make-agent #:name "TestAgent" #:instructions "You are a test agent."))
    (define-values (response updated-ctx) (agent-process agent "Say hello." ctx))
    (println response)
)

(test)

;; -----------------------------------------------------------------------------
;; Info file for library

#;(provide (all-defined-out)) ;; Already provided at the top

#;(define-library llm-agent
  #:export (all-from-out llm-agent)
  #:import (racket/base racket/hash racket/contract racket/sequence)
  (require/provide "llm-agent.rkt"))

