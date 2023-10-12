#lang racket

;; raco pkg install sqlite-table

(require db)
(require sqlite-table)

(define db-file "test.db")
(println db-file)
(define db (sqlite3-connect #:database db-file #:mode 'create))

(query-exec db
   "create temporary table the_numbers (n integer, d varchar(20))")
(query-exec db
    "insert into the_numbers values (0, 'nothing')")
(query-exec db
    "insert into the_numbers values (1, 'the loneliest number')")

(define results
  (query db "select * from the_numbers"))

(println results)