#lang racket


(require llm/openai/gpt4o-mini)

(gpt4o-mini-send-prompt! "What is 13 + 7?" '())
