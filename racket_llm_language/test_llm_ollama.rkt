#lang racket

(require llm/ollama/phi3)

(phi3-send-prompt! "What is 13 + 7? Be concise." '())
