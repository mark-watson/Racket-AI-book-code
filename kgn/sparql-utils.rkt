#lang at-exp racket

(provide sparql-dbpedia-for-person)
(provide sparql-dbpedia-person-uri)
(provide sparql-dbpedia-place-uri)
(provide sparql-query->hash)
(provide json->listvals)
(provide extract-name-uri-and-comment)

(require net/url)
(require net/uri-codec)
(require json)
(require racket/pretty)

(define ps-encoded-by "ps:P702")
(define wdt-instance-of "wdt:P31")
(define wdt-in-taxon "wdt:P703")
(define wd-human "wd:Q15978631")
(define wd-mouse "wd:Q83310")
(define wd-rat "wd:Q184224")
(define wd-gene "wd:Q7187")

(define (sparql-dbpedia-for-person person-uri)
  @string-append{
     SELECT
      (GROUP_CONCAT(DISTINCT ?website; SEPARATOR="  |  ") AS ?website) ?comment {
      OPTIONAL { @person-uri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . FILTER (lang(?comment) = 'en') } .
      OPTIONAL { @person-uri <http://dbpedia.org/ontology/wikiPageExternalLink> ?website . FILTER( !regex(str(?website), "dbpedia", "i"))} .
     } LIMIT 4})

(define (sparql-dbpedia-person-uri person-name)
  @string-append{
    SELECT DISTINCT ?personuri ?comment {
      ?personuri <http://xmlns.com/foaf/0.1/name> "@person-name"@"@"en .
      ?personuri <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment .
                  FILTER  (lang(?comment) = 'en') .
}})


(define (sparql-dbpedia-place-uri place-name)
  ;; work in progress - not tested
  @string-append{
    SELECT DISTINCT ?placeuri ?comment {
      ?placeuri <http://xmlns.com/foaf/0.1/name> "@place-name"@"@"en .
      ?placeuri <http://www.w3.org/2000/01/rdf-schema#comment>  ?comment .
                  FILTER  (lang(?comment) = 'en') .
}})


(define (sparql-query->hash query)
  (call/input-url (string->url (string-append "https://dbpedia.org/sparql?query=" (uri-encode query)))
                      get-pure-port
                      (lambda (port)
                        (string->jsexpr (port->string port))
                        )
                      '("Accept: application/json")))

(define (json->listvals a-hash)
  (let ((bindings (hash->list a-hash)))
    (let* ((head (first bindings))
           (vars (hash-ref (cdr head) 'vars))
           (results (second bindings)))
      (let* ((x (cdr results))
             (b (hash-ref x 'bindings)))
        (for/list ([var vars])
                  (for/list ([bc b])
                    (let ((bcequal (make-hash (hash->list bc))))
                      (let ((a-value (hash-ref (hash-ref bcequal (string->symbol var)) 'value)))
                        (list var a-value)))))))))

(define extract-name-uri-and-comment (lambda (l1 l2)
              (map ;; perform a "zip" action on teo lists
               (lambda (a b)
                 (list (second a) (second b)))
               l1 l2)))

;;(sparql-query->hash (sparql-dbpedia-person-uri "Steve Jobs"))