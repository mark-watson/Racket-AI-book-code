#lang racket/gui

(require htdp/gui)
(require racket/gui/base)
(require racket/pretty)
(provide make-selection-functions)

(define (make-selection-functions parent-frame title)
  (let* ((dialog
          (new dialog%	 
               [label title]	 
               [parent parent-frame]	 
               [width 440]	 
               [height 480]))
         (close-callback
          (lambda (button event)
            (send dialog show #f)))
         (entity-chooser-dialog-list-box
          (new list-box%	 
               [label ""]	 
               [choices (list "aaaa" "bbbb")]	 
               [parent dialog]	 
               [callback (lambda (click event)
                           (if (equal? (send event get-event-type) 'list-box-dclick)
                               (close-callback click event)
                               #f))]))
         (quit-button
          (new button% [parent dialog]
             [label "Select an entity"]
             [callback  close-callback]))
         (set-new-items-and-show-dialog
          (lambda (a-list)
            (send entity-chooser-dialog-list-box set-selection 0)
            (send entity-chooser-dialog-list-box set a-list)
            (send dialog show #t)))
         (get-selection-index (lambda () (first (send entity-chooser-dialog-list-box get-selections)))))
    (list set-new-items-and-show-dialog get-selection-index)))

