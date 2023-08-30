#lang racket

(require html net/url xml xml/path)

(define (read-html-as-xexprs in)
  (caddr (xml->xexpr (element #f #f 'root '() (read-html-as-xml in)))))

(define reddit (string->url "http://markwatson.com"))

(define xe (call/input-url reddit get-pure-port read-html-as-xexprs))

(se-path* "//text()" xe)
