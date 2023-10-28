#lang racket

(require srfi/13) ; the string SRFI
(require racket/runtime-path)

;;(require "utils.rkt")

(provide parts-of-speech)

;; FastTag.lisp
;;
;; Conversion of KnowledgeBooks.com Java FastTag to Scheme
;;
;; Copyright 2002 by Mark Watson.  All rights reserved.
;;


(display "loading lex-hash...")

(define-runtime-path my-data-path "data")

(define lex-hash
  (let ((hash (make-hash)))
    (with-input-from-file
        (string-append (path->string my-data-path) "/tag.dat")
      (lambda ()
        (let loop ()
          (let ((p (read)))
            (if (list? p) (hash-set! hash (car p) (cadr p)) #f)
            (if (eof-object? p) #f (loop))))))
    hash))
(display "...done.")
(log-info "loading lex-hash" "ending")

(define (string-suffix? pattern str)
  (let loop ((i (- (string-length pattern) 1)) (j (- (string-length str) 1)))
    (cond
     ((negative? i) #t)
     ((negative? j) #f)
     ((char=? (string-ref pattern i) (string-ref str j))
      (loop (- i 1) (- j 1)))
     (else #f))))
;;
; parts-of-speech
;
;  input: a vector of words (each a string)
;  output: a vector of parts of speech
;;

(define (parts-of-speech words)
  (display "\n+ tagging:") (display words)
  (let ((ret '())
        (r #f)
        (lastRet #f)
        (lastWord #f))
    (for-each
     (lambda (w)
       (set! r (hash-ref lex-hash w #f))
       ;; if this word is not in the hash table, try making it ll lower case:
       (if (not r)
           (set! r '("NN"))
           #f)
       ;;(if (list? r) (set! r (car r))))
       ;; apply transformation rules:
       
       ; rule 1: DT, {VBD, VBP, VB} --> DT, NN
       (if (equal? lastRet "DT")
           (if (or
                (equal? r "VBD")
                (equal? r "VBP")
                (equal? r "VB"))
               (set! r '("NN"))
               #f)
           #f)
       ; rule 2: convert a noun to a number if a "." appears in the word
       (if (string-contains "." w) (set! r '("CD")) #f)

       ; rule 3: convert a noun to a past participle if word ends with "ed"
       (if (equal? (member "N" r) 0)
           (let* ((slen (string-length w)))
             (if (and
                  (> slen 1)
                  (equal? (substring w (- slen 2)) "ed"))
                 (set! r "VBN") #f))
           #f)

       ; rule 4: convert any type to an adverb if it ends with "ly"
       (let ((i (string-suffix? "ly" w)))
         (if (equal? i (- (string-length w) 2))
             (set! r '("RB"))
             #f))

       ; rule 5: convert a common noun (NN or NNS) to an adjective
       ;         if it ends with "al"
       (if (or
            (member "NN" r)
            (member "NNS" r))
           (let ((i (string-suffix? "al" w)))
             (if (equal? i (- (string-length w) 2))
                 (set! r '("RB"))
                 #f))
           #f)

       ; rule 6: convert a noun to a verb if the receeding word is "would"
       (if (equal? (member "NN" r) 0)
           (if (equal? lastWord "would")
               (set! r '("VB"))
               #f)
           #f)

       ; rule 7: if a word has been categorized as a common noun and it
       ;         ends with "s", then set its type to a plural noun (NNS)
       (if (member "NN" r)
           (let ((i (string-suffix? "s" w)))
             (if (equal? i (- (string-length w) 1))
                 (set! r '("NNS"))
                 #f))
           #f)

       ; rule 8: convert a common noun to a present participle verb
       ;         (i.e., a gerand)
       (if (equal? (member "NN" r) 0)
           (let ((i (string-suffix? "ing" w)))
             (if (equal? i (- (string-length w) 3))
                 (set! r '("VBG"))
                 #f))
           #f)

       (set! lastRet ret)
       (set! lastWord w)
       (set! ret (cons (first r) ret)))
     (vector->list words))  ;; not very efficient !!
    (list->vector (reverse ret))))


;;(display (parts-of-speech (list->vector '("the" "cat" "ran"))))
;;(display (parts-of-speech (list->vector '("banking" "in" "Europe" "is" "a" "good" "business" "and" "a" "liberty"))))


;; (display (parts-of-speech
;;           '#("President" "Bush" "went" "to" "San" "Diego" "to" "meet" "Ms" "." "Jones" "and" "Gen" "." "Pervez" "Musharraf" ".")))
