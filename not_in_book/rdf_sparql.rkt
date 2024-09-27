#lang racket

; Define the datastore as a list of triples
(define rdf-store '())

; Function to add a triple to the datastore
(define (add-triple subject predicate object)
  (set! rdf-store (cons (list subject predicate object) rdf-store)))

; Function to match a single element against a pattern
(define (match-element element pattern)
  (or (eq? pattern '_) ; '_' is used as a wildcard
      (equal? element pattern)))

; Function to match a triple against a pattern
(define (match-triple triple pattern)
  (and (match-element (first triple) (first pattern))
       (match-element (second triple) (second pattern))
       (match-element (third triple) (third pattern))))

; Function to query the datastore
(define (query-rdf subject predicate object)
  (filter (lambda (triple)
            (match-triple triple (list subject predicate object)))
          rdf-store))

; Function to print all triples in the datastore
(define (print-all-triples)
  (for-each (lambda (triple)
              (printf "~a ~a ~a\n" (first triple) (second triple) (third triple)))
            rdf-store))

; Define take-while function
(define (take-while pred lst)
  (cond [(null? lst) '()]
        [(pred (car lst)) (cons (car lst) (take-while pred (cdr lst)))]
        [else '()]))

; Define drop-while function
(define (drop-while pred lst)
  (cond [(null? lst) '()]
        [(pred (car lst)) (drop-while pred (cdr lst))]
        [else lst]))

; Function to parse a SPARQL-like query string
(define (parse-sparql-query query-string)
  (let* ([parts (string-split query-string)]
         [select-part (take-while (lambda (x) (not (equal? x "where"))) parts)]
         [where-part (drop-while (lambda (x) (not (equal? x "where"))) parts)])
    (if (or (null? where-part) (< (length where-part) 2))
        (error "Invalid SPARQL query: missing or incomplete WHERE clause")
        (let* ([triple-string (string-join (cdr where-part) " ")]
               [triple-pattern (if (and (string-prefix? triple-string "{") 
                                        (string-suffix? triple-string "}"))
                                   (string-split (substring triple-string 1 (- (string-length triple-string) 1)))
                                   (error "Invalid triple pattern: must be enclosed in curly braces {}"))])
          (if (< (length triple-pattern) 3)
              (error "Invalid triple pattern: must have subject, predicate, and object")
              (values select-part triple-pattern))))))

; Helper function to find the index of an element in a list, handling both strings and symbols
(define (index-of lst elem)
  (let loop ([i 0] [l lst])
    (cond
      [(null? l) #f] ; Base case: element not found
      [(equal? (car l) elem) i] ; Found at the beginning
      [else 
       (let ([rest-index (index-of (cdr l) elem)]) ; Recursively search the rest of the list
         (if rest-index 
             (+ rest-index 1) ; If found in the rest, adjust the index
             #f))]))) ; Otherwise, not found

; Function to execute a SPARQL-like query
(define (execute-sparql-query query-string)
  (let-values ([(select-vars triple-pattern) (parse-sparql-query query-string)])
    (let* ([s (if (string-prefix? (first triple-pattern) "?") '_ (string->symbol (first triple-pattern)))]
           [p (if (string-prefix? (second triple-pattern) "?") '_ (string->symbol (second triple-pattern)))]
           [o (if (string-prefix? (third triple-pattern) "?") '_ (string->symbol (third triple-pattern)))]
           [results (query-rdf s p o)]
           ; Corrected logic for all-vars
           [all-vars (if (or (null? select-vars)
                             (and (not (null? select-vars)) 
                                  (equal? (string-join select-vars " ") "select *"))) ; Check for "select *"
                         triple-pattern 
                         (filter (lambda (var) (string-prefix? var "?"))
                                 (cdr select-vars)))]
           )
    (map (lambda (result)
           (map (lambda (var)
                  (let ([index (index-of triple-pattern var)])
                    (if index
                        (list var (list-ref result index))
                        '())))
                all-vars))
         results))))


; Example usage
(add-triple 'John 'likes 'Pizza)
(add-triple 'Mary 'likes 'Pasta)
(add-triple 'John 'age 30)
(add-triple 'Mary 'age 28)

(printf "All triples:\n")
(print-all-triples)

(printf "\nSPARQL-like query: select ?s ?o where { ?s age ?o }\n")
(display (execute-sparql-query "select ?s ?o where { ?s age ?o }"))

(printf "\nSPARQL-like query: select ?s ?o where { ?s likes ?o }\n")
(display (execute-sparql-query "select ?s ?o where { ?s likes ?o }"))

(printf "\nSPARQL-like query: select ?o where { John age ?o }\n")
(display (execute-sparql-query "select ?o where { John age ?o }"))

(printf "\nSPARQL-like query: select ?age where { John age ?age }\n")
(display (execute-sparql-query "select ?age where { John age ?age }"))

(printf "\nSPARQL-like query: select ?person ?food where { ?person likes ?food }\n")
(display (execute-sparql-query "select ?person ?food where { ?person likes ?food }"))

(printf "\nSPARQL-like query: select ?x ?y ?z where { ?x ?y ?z }\n")
(display (execute-sparql-query "select ?x ?y ?z where { ?x ?y ?z }"))
(printf "\nSPARQL-like query: select * where { ?x ?y ?z }\n")
(display (execute-sparql-query "select * where { ?x ?y ?z }"))