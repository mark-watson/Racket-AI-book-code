#lang racket
(require db)
(require openai)

(define (write-floats-to-string lst)
  (with-output-to-string
    (lambda (out)
      (fprintf out "( ")
      (for ([i lst])
        (fprintf out "~a " i))
      (fprintf out " )"))))

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

(define (decode-row row)
  (let ((id (list-ref row 0))
        (context (list-ref row 1))
        (embedding (read (open-input-string (list-ref row 2)))))
    (list id context embedding)))

(define db (sqlite3-connect ":memory:"))  ; For in-memory database

;; ... database setup, error handling, and queries ...

(define (insert-document document-path content embedding)
  (printf "~%insert-document:~%  content:~a~%~%" content)
  (query-exec
   db
   "INSERT INTO documents (document_path, content, embedding) VALUES (?, ?, ?);"
   document-path content (write-floats-to-string embedding)))

(define (get-document-by-document-path document-path)
  (map decode-row
       (query-rows db
                    "SELECT * FROM documents WHERE document_path = ?;"
                    document-path)))

;; ... remaining database query functions ...

(define (create-document fpath)
  (let ((contents (break-into-chunks (read-file fpath) 200)))
    (for-each
     (lambda (content)
       (with-handlers ((exn? (lambda (c)
                               (printf "Error: ~a~%" c))))
         ;; Assuming openai::embeddings is a function that gets embeddings
         (let ((embedding (openai::embeddings content)))
           (insert-document fpath content embedding))))
     contents)))


;; Assuming a function to fetch documents from database
(define (execute-to-list db query)
  (query-rows db query))

;; Assuming a function to get document embeddings
(define (openai::embeddings content)
  ;; ... implementation ...
  )

;; Assuming a function to compute dot product
(define (openai::dot-product emb1 emb2)
  ;; ... implementation ...
  )

;; Assuming a function to answer questions
(define (openai:answer-question query-with-context n)
  ;; ... implementation ...
  )

(define (semantic-match query custom-context [cutoff 0.7])
  (let ((emb (openai::embeddings query))
        (ret '()))
    (for-each
     (lambda (doc)
       (let* ((context (second doc)) ;; ignore fpath for now
              (embedding (third doc))
              (score (openai::dot-product emb embedding)))
         (when (> score cutoff)
           (set! ret (cons context ret)))))
     (all-documents))  ;; Assuming all-documents is a function that fetches all documents
    (printf "~%semantic-search: ret=~a~%" ret)
    (let* ((context (string-join (reverse ret) " . "))
           (query-with-context (string-join (list context custom-context "Question:" query) " ")))
      (openai:answer-question query-with-context 40))))

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
  (create-document "data/sports.txt")
  (create-document "data/chemistry.txt")
  (QA "What is the history of the science of chemistry?")
  (QA "What are the advantages of engaging in sports?"))

;; test()



