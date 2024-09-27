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

; Example usage
(add-triple 'John 'likes 'Pizza)
(add-triple 'Mary 'likes 'Pasta)
(add-triple 'John 'age 30)
(add-triple 'Mary 'age 28)

(printf "All triples:\n")
(print-all-triples)

(printf "\nQuery: (John likes _)\n")
(display (query-rdf 'John 'likes '_))

(printf "\nQuery: (_ age _)\n")
(display (query-rdf '_ 'age '_))

(printf "\nQuery: (Mary _ _)\n")
(display (query-rdf 'Mary '_ '_))