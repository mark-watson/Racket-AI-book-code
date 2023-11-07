#lang racket

(require pdf-read)

(define (pdf->string pdf-file-path)
  (let* ((page-count (pdf-count-pages pdf-file-path)))
    (displayln page-count)
    (string-join
     (for/list ([i page-count])
       (page-text (pdf-page pdf-file-path i))))))

(displayln (pdf->string "/Users/markw/GITHUB/Racket-AI-book-code/pdf_chat/test_pdfs/alice_in_wonderland.pdf"))