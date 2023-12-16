#lang racket/base

(require "anthropic.rkt")
(require "llama_local.rkt")
(require "ollama_ai_local.rkt")
(require "openai.rkt")

(provide question-anthropic completion-anthropic)
(provide question-llama-local completion-llama-local embeddings-ollama)
(provide question-ollama-ai-local completion-ollama-ai-local)
(provide question-openai completion-openai embeddings-openai)
