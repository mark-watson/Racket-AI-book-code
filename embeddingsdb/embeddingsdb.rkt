#lang racket

(require db)
(require llmapis)

(provide create-document QA)

; Function to convert list of floats to string representation
(define (floats->string floats)
  (string-join (map number->string floats) " "))

; Function to convert string representation back to list of floats
(define (string->floats str)
  (map string->number (string-split str)))


(define (read-file infile)
  (with-input-from-file infile
    (lambda ()
      (let ((contents (read)))
        contents))))

(define (join-strings separator list)
  (string-join list separator))

(define (truncate-string string length)
  (substring string 0 (min length (string-length string))))

(define (interleave list1 list2)
  (if (or (null? list1) (null? list2))
      (append list1 list2)
      (cons (car list1)
            (cons (car list2)
                  (interleave (cdr list1) (cdr list2))))))

(define (break-into-chunks text chunk-size)
  (let loop ((start 0) (chunks '()))
    (if (>= start (string-length text))
        (reverse chunks)
        (loop (+ start chunk-size)
              (cons (substring text start (min (+ start chunk-size) (string-length text))) chunks)))))

(define (string-to-list str)
  (map string->number (string-split str)))

(define (decode-row row)
  (let ((id (vector-ref row 0))
        (context (vector-ref row 1))
        (embedding (string-to-list (read-line (open-input-string (vector-ref row 2))))))
    (list id context embedding)))

(define db (sqlite3-connect #:database "test.db" #:mode 'create #:use-place #t))

(with-handlers ([exn:fail? (lambda (ex) (void))])
  (query-exec
   db
   "CREATE TABLE documents (document_path TEXT, content TEXT, embedding TEXT);"))
      
;; ... database setup, error handling, and queries ...

(define (insert-document document-path content embedding)
  (printf "~%insert-document:~%  content:~a~%~%" content)
  (query-exec
   db
   "INSERT INTO documents (document_path, content, embedding) VALUES (?, ?, ?);"
   document-path content (floats->string embedding)))

(define (get-document-by-document-path document-path)
  (map decode-row
       (query-rows db
                    "SELECT * FROM documents WHERE document_path = ?;"
                    document-path)))

(define (all-documents)
  (map
   decode-row
   (query-rows
    db
    "SELECT * FROM documents;")))
   
;; ... remaining database query functions ...

(define (create-document fpath)
  (let ((contents (break-into-chunks (file->string fpath) 200)))
    (for-each
     (lambda (content)
       (with-handlers ([exn:fail? (lambda (ex) (void))])
         (let ((embedding (embeddings-openai content)))
           (insert-document fpath content embedding))))
     contents)))


;; Assuming a function to fetch documents from database
(define (execute-to-list db query)
  (query-rows db query))

(define (dot-product a b) ;; dot product of two lists of floating point numbers
  (cond
    [(or (null? a) (null? b)) 0]
    [else
     (+ (* (car a) (car b))
        (dot-product (cdr a) (cdr b)))]))


(define (semantic-match query custom-context [cutoff 0.7])
  (let ((emb (embeddings-openai query))
        (ret '()))
    (for-each
     (lambda (doc)
       (let* ((context (second doc))
              (embedding (third doc))
              (score (dot-product emb embedding)))
         (when (> score cutoff)
           (set! ret (cons context ret)))))
     (all-documents))
    (printf "~%semantic-search: ret=~a~%" ret)
    (let* ((context (string-join (reverse ret) " . "))
           (query-with-context (string-join (list context custom-context "Question:" query) " ")))
      (question-openai query-with-context))))

(define (QA query [quiet #f])
  (let ((answer (semantic-match query "")))
    (unless quiet
      (printf "~%~%** query: ~a~%** answer: ~a~%~%" query answer))
    answer))

(define (CHAT)
  (let ((messages '(""))
        (responses '("")))
    (let loop ()
      (printf "~%Enter chat (STOP or empty line to stop) >> ")
      (let ((string (read-line)))
        (cond
         ((or (string=? string "STOP") (< (string-length string) 1))
          (list (reverse messages) (reverse responses)))
         (else
          (let* ((custom-context
                  (string-append
                   "PREVIOUS CHAT: "
                   (string-join (reverse messages) " ")))
                 (response (semantic-match string custom-context)))
            (set! messages (cons string messages))
            (set! responses (cons response responses))
            (printf "~%Response: ~a~%" response)
            (loop))))))))

;; ... test code ...

(define (test)
  "Test code for Semantic Document Search Using OpenAI GPT APIs and local vector database"
  (create-document "/Users/markw/GITHUB/Racket-AI-book-code/embeddingsdb/data/sports.txt")
  (create-document "/Users/markw/GITHUB/Racket-AI-book-code/embeddingsdb/data/chemistry.txt")
  (QA "What is the history of the science of chemistry?")
  (QA "What are the advantages of engaging in sports?"))

;; test()



