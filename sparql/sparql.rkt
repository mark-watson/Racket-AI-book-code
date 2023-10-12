#lang at-exp racket

(provide sparql-dbpedia-person-uri)
(provide sparql-query->hash)
(provide json->listvals)
(provide sparql-dbpedia)

(require net/url)
(require net/uri-codec)
(require json)
(require racket/pretty)

(define (sparql-dbpedia-for-person person-uri)
  @string-append{
     SELECT
      (GROUP_CONCAT(DISTINCT ?website; SEPARATOR="  |  ")
                                   AS ?website) ?comment {
      OPTIONAL {
       @person-uri
       <http://www.w3.org/2000/01/rdf-schema#comment>
       ?comment . FILTER (lang(?comment) = 'en')
      } .
      OPTIONAL {
       @person-uri
       <http://dbpedia.org/ontology/wikiPageExternalLink>
       ?website
        . FILTER( !regex(str(?website), "dbpedia", "i"))
      }
     } LIMIT 4})

(define (sparql-dbpedia-person-uri person-name)
  @string-append{
    SELECT DISTINCT ?personuri ?comment {
      ?personuri
        <http://xmlns.com/foaf/0.1/name>
        "@person-name"@"@"en .
      ?personuri
        <http://www.w3.org/2000/01/rdf-schema#comment>
        ?comment .
             FILTER  (lang(?comment) = 'en') .
}})


(define (sparql-query->hash query)
  (call/input-url
   (string->url
    (string-append
     "https://dbpedia.org/sparql?query="
     (uri-encode query)))
   get-pure-port
   (lambda (port)
     (string->jsexpr (port->string port)))
   '("Accept: application/json")))

(define (json->listvals a-hash)
  (let ((bindings (hash->list a-hash)))
    (let* ((head (first bindings))
           (vars (hash-ref (cdr head) 'vars))
           (results (second bindings)))
      (let* ((x (cdr results))
             (b (hash-ref x 'bindings)))
        (for/list
            ([var vars])
          (for/list ([bc b])
            (let ((bcequal
                   (make-hash (hash->list bc))))
              (let ((a-value
                     (hash-ref
                      (hash-ref
                       bcequal
                       (string->symbol var)) 'value)))
                (list var a-value)))))))))


(define gd (lambda (data)

    (let ((jd (json->listvals data)))

      (define gg1
        (lambda (jd) (map list (car jd))))
      (define gg2
        (lambda (jd) (map list (car jd) (cadr jd))))
      (define gg3
        (lambda (jd)
          (map list (car jd) (cadr jd) (caddr jd))))
      (define gg4
        (lambda (jd)
          (map list
               (car jd) (cadr jd)
               (caddr jd) (cadddr jd))))

      (case (length (json->listvals data))
        [(1) (gg1 (json->listvals data))]
        [(2) (gg2 (json->listvals data))]
        [(3) (gg3 (json->listvals data))]
        [(4) (gg4 (json->listvals data))]
        [else
         (error "sparql queries with 1 to 4 vars")]))))


(define sparql-dbpedia
  (lambda (sparql)
    (gd (sparql-query->hash sparql))))

;; (sparql-dbpedia (sparql-dbpedia-person-uri "Steve Jobs"))