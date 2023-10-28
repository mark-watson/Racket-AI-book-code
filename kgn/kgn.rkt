#lang racket/gui

(require htdp/gui)            ;; note: when building executable, choose GRacket, not Racket to get *.app bundle
(require racket/gui/base)
(require racket/match)
(require racket/pretty)
(require scribble/text/wrap)


;; Sample queries:
;;   who is Bill Gates
;;   where is San Francisco?
;; (only who/where queries are currently handled)

(require nlp)
(require "main.rkt")
(require "dialog-utils.rkt")

(define count-substring 
  (compose length regexp-match*))

(define (short-string s)
  (if (< (string-length s) 75)
      s
      (substring s 0 73)))

(define dummy (lambda (button event) (display "\ndummy\n"))) ;; this will be redefined after UI objects are created

(let ((query-callback (lambda (button event) (dummy button event))))
  (match-let* ([frame (new frame% [label "Knowledge Graph Navigator"]
                           [height 400] [width 608] [alignment '(left top)])]
               [(list set-new-items-and-show-dialog get-selection-index) ; returns list of 2 callback functions
                (make-selection-functions frame "Test selection list")]
               [query-field (new text-field%
                                 [label "  Query:"] [parent frame]
                                 [callback
                                  (lambda( k e)
                                    (if (equal? (send e get-event-type) 'text-field-enter) (query-callback k e) #f))])]
               [a-container (new pane%
                                 [parent frame] [alignment '(left top)])]
               [a-message (new message%
                               [parent frame] [label "  Generated SPARQL:"])]
               [sparql-canvas (new text-field%
                                   (parent frame) (label "")
                                   [min-width 380] [min-height 200]
                                   [enabled #f])]
               [a-message-2 (new message% [parent frame] [label "  Results:"])]
               [results-canvas (new text-field%
                                    (parent frame) (label "")
                                    [min-height 200] [enabled #f])]
               [a-button (new button% [parent a-container]
                              [label "Process: query -> generated SPARQL -> results from DBPedia"]
                              [callback query-callback])])
    (display "\nbefore setting new query-callback\n")
    (set!
     dummy ;; override dummy labmda defined earlier
     (lambda (button event)
       (display "\n+ in query-callback\n")
       (let ((query-helper-results-all (ui-query-helper (send query-field get-value))))
         (if (equal? query-helper-results-all #f)
             (let ()
               (send  sparql-canvas set-value "no generated SPARQL")
               (send  results-canvas set-value "no results"))
             (let* ((sparql-results (first query-helper-results-all))
                    (query-helper-results-uri-and-description (cadr query-helper-results-all))
                    (uris (map first query-helper-results-uri-and-description))
                    (query-helper-results (map second query-helper-results-uri-and-description)))
               (display "\n++ query-helper-results:\n") (display query-helper-results) (display "\n")
               (if (= (length query-helper-results) 1)
                   (let ()
                     (send  sparql-canvas set-value sparql-results)
                     (send  results-canvas set-value
                            (string-append (string-join  (wrap-line (first query-helper-results) 95) "\n") "\n\n" (first uris))))
                   (if (> (length query-helper-results) 1)
                       (let ()
                         (set-new-items-and-show-dialog (map short-string query-helper-results))
                         (set! query-helper-results
                               (let ((sel-index (get-selection-index)))
                                 (if (> sel-index -1)
                                     (list-ref query-helper-results sel-index)
                                     '(""))))
                         (set! uris (list-ref uris (get-selection-index)))
                         (display query-helper-results)
                         (send  sparql-canvas set-value sparql-results)
                         (send  results-canvas set-value
                                (string-append (string-join  (wrap-line query-helper-results 95) "\n") "\n\n" uris)))
                       (send  results-canvas set-value (string-append "No results for: " (send query-field get-value))))))))))
     (send frame show #t)))
    
