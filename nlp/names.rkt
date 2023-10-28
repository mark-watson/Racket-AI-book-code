#lang racket

(require "fasttag.rkt")
(require racket/runtime-path)

(provide find-human-names)
(provide find-place-names)

(define-runtime-path my-data-path "data")

(define (process-one-word-per-line file-path func)
  (with-input-from-file file-path
	(lambda ()
	  (let loop ()
		(let ([l (read-line)])
		  (if (equal? l #f) #f (func l))
		  (if (eof-object? l) #f (loop)))))))

(define *last-name-hash* (make-hash))
(process-one-word-per-line 
  (string-append
    (path->string my-data-path)
    "/human_names/names.last")
  (lambda (x) (hash-set! *last-name-hash* x #t)))
(define *first-name-hash* (make-hash))
(process-one-word-per-line
  (string-append
    (path->string my-data-path)
    "/human_names/names.male")
  (lambda (x) (hash-set! *first-name-hash* x #t)))
(process-one-word-per-line
  (string-append
    (path->string my-data-path)
    "/human_names/names.female")
  (lambda (x) (hash-set! *first-name-hash* x #t)))

(define *place-name-hash* (make-hash))
(process-one-word-per-line
  (string-append
    (path->string my-data-path)
    "/placenames.txt")
  (lambda (x) (hash-set!  *place-name-hash* x #t)))

(display (hash-ref *last-name-hash* "Bartlow" #f))

(define *name-prefix-list*
  '("Mr" "Mrs" "Ms" "Gen" "General" "Maj" "Major" "Doctor" "Vice" "President" 
	"Lt" "Premier" "Senator" "Congressman" "Prince" "King" "Representative"
	"Sen" "St" "Dr"))

(define (not-in-list-find-names-helper a-list start end)
  (let ((rval #t))
    (do ((x a-list (cdr x)))
	((or
	  (null? x)
	  (let ()
	    (if (or
		 (and
		  (>= start (caar x))
		  (<= start (cadar x)))
		 (and
		  (>= end (caar x))
		  (<= end (cadar x))))
		(set! rval #f)
                #f)
	    (not rval)))))
    rval))

;; return a list of sublists, each sublist looks like:
;;    (("John" "Smith") (11 12) 0.75) ; last number is an importance rating
(define (find-human-names word-vector exclusion-list)
  (define (score result-list)
    (- 1.0 (* 0.2 (- 4 (length result-list)))))
  (let ((tags (parts-of-speech word-vector))
        (ret '()) (ret2 '()) (x '())
        (len (vector-length word-vector))
        (word #f))
    (display "\ntags: ") (display tags)
    ;;(dotimes (i len)
    (for/list ([i (in-range len)])
      (set! word (vector-ref word-vector i))
      (display "\nword: ") (display word)
      ;; process 4 word names:      HUMAN NAMES
      (if (< i (- len 3))
          ;; case #1: single element from '*name-prefix-list*'
          (if (and
               (not-in-list-find-names-helper ret i (+ i 4))
               (not-in-list-find-names-helper exclusion-list i (+ i 4))
               (member word *name-prefix-list*)
               (equal? "." (vector-ref word-vector (+ i 1)))
               (hash-ref *first-name-hash* (vector-ref word-vector (+ i 2)) #f)
               (hash-ref *last-name-hash* (vector-ref word-vector (+ i 3)) #f))
              (if (and
                   (string-prefix? (vector-ref tags (+ i 2)) "NN")
                   (string-prefix? (vector-ref tags (+ i 3)) "NN"))
                  (set! ret (cons (list i (+ i 4)) ret))
                  #f)
              #f)
          ;; case #1: two elements from '*name-prefix-list*'
          (if (and
               (not-in-list-find-names-helper ret i (+ i 4))
               (not-in-list-find-names-helper exclusion-list i (+ i 4))
               (member word *name-prefix-list*)
               (member (vector-ref word-vector (+ i 1)) *name-prefix-list*)
               (hash-ref *first-name-hash* (vector-ref word-vector (+ i 2)) #f)
               (hash-ref *last-name-hash* (vector-ref word-vector (+ i 3)) #f))
              (if (and
                   (string-prefix? (vector-ref tags (+ i 2)) "NN")
                   (string-prefix? (vector-ref tags (+ i 3)) "NN"))
                  (set! ret (cons (list i (+ i 4)) ret))
                  #f)
              #f))
      ;; process 3 word names:      HUMAN NAMES
      (if (< i (- len 2))
          (if (and
               (not-in-list-find-names-helper ret i (+ i 3))
               (not-in-list-find-names-helper exclusion-list i (+ i 3)))
              (if (or
                   (and
                    (member word *name-prefix-list*)
                    (hash-ref *first-name-hash* (vector-ref word-vector (+ i 1)) #f)
                    (hash-ref *last-name-hash* (vector-ref word-vector (+ i 2)) #f)
                    (string-prefix? (vector-ref tags (+ i 1)) "NN")
                    (string-prefix? (vector-ref tags (+ i 2)) "NN"))
                   (and
                    (member word *name-prefix-list*)
                    (member (vector-ref word-vector (+ i 1)) *name-prefix-list*)
                    (hash-ref *last-name-hash* (vector-ref word-vector (+ i 2)) #f)
                    (string-prefix? (vector-ref tags (+ i 1)) "NN")
                    (string-prefix? (vector-ref tags (+ i 2)) "NN"))
                   (and
                    (member word *name-prefix-list*)
                    (equal? "." (vector-ref word-vector (+ i 1)))
                    (hash-ref *last-name-hash* (vector-ref word-vector (+ i 2)) #f)
                    (string-prefix? (vector-ref tags (+ i 2)) "NN"))
                   (and
                    (hash-ref *first-name-hash* word #f)
                    (hash-ref *first-name-hash* (vector-ref word-vector (+ i 1)) #f)
                    (hash-ref *last-name-hash* (vector-ref word-vector (+ i 2)) #f)
                    (string-prefix? (vector-ref tags i) "NN")
                    (string-prefix? (vector-ref tags (+ i 1)) "NN")
                    (string-prefix? (vector-ref tags (+ i 2)) "NN")))
                  (set! ret (cons (list i (+ i 3)) ret))
                  #f)
              #f)
          #f)
      ;; process 2 word names:      HUMAN NAMES
      (if (< i (- len 1))
          (if (and
               (not-in-list-find-names-helper ret i (+ i 2))
               (not-in-list-find-names-helper exclusion-list i (+ i 2)))
              (if (or
                   (and
                    (member word '("Mr" "Mrs" "Ms" "Doctor" "President" "Premier"))
                    (string-prefix? (vector-ref tags (+ i 1)) "NN")
                    (hash-ref *last-name-hash* (vector-ref word-vector (+ i 1)) #f))
                   (and
                    (hash-ref *first-name-hash* word #f)
                    (hash-ref *last-name-hash* (vector-ref word-vector (+ i 1)) #f)
                    (string-prefix? (vector-ref tags i) "NN")
                    (string-prefix? (vector-ref tags (+ i 1)) "NN")))
                  (set! ret (cons (list i (+ i 2)) ret))
                  #f)
              #f)
          #f)
      ;; 1 word names:      HUMAN NAMES
      (if (hash-ref *last-name-hash* word #f)
          (if (and
               (string-prefix? (vector-ref tags i) "NN")
               (not-in-list-find-names-helper ret i (+ i 1))
               (not-in-list-find-names-helper exclusion-list i (+ i 1)))
              (set! ret (cons (list i (+ i 1)) ret))
              #f)
          #f))
    ;; TBD: calculate importance rating based on number of occurences of name in text:
    (set! ret2
          (map (lambda (index-pair)
                 (string-replace
                  (string-join (vector->list (vector-copy  word-vector (car index-pair) (cadr index-pair))))
                  " ." "."))
               ret))
    ret2))

(define (find-place-names word-vector exclusion-list)  ;; PLACE
  (define (score result-list)
    (- 1.0 (* 0.2 (- 4 (length result-list)))))
  (let ((tags (parts-of-speech word-vector))
        (ret '()) (ret2 '()) (x '())
        (len (vector-length word-vector))
        (word #f))
    (display "\ntags: ") (display tags)
    ;;(dotimes (i len)
    (for/list ([i (in-range len)])
      (set! word (vector-ref word-vector i))
      (display "\nword: ") (display word) (display "\n")
      ;; process 3 word names: PLACE
      (if (< i (- len 2))
          (if (and
               (not-in-list-find-names-helper ret i (+ i 3))
               (not-in-list-find-names-helper exclusion-list i (+ i 3)))
              (let ((p-name (string-append word " " (vector-ref word-vector (+ i 1)) " " (vector-ref word-vector (+ i 2)))))
                (if (hash-ref *place-name-hash* p-name #f)
                    (set! ret (cons (list i (+ i 3)) ret))
                  #f))
              #f)
          #f)
      ;; process 2 word names:  PLACE
      (if (< i (- len 1))
          (if (and
               (not-in-list-find-names-helper ret i (+ i 2))
               (not-in-list-find-names-helper exclusion-list i (+ i 2)))
              (let ((p-name (string-append word " " (vector-ref word-vector (+ i 1)))))
                (if (hash-ref *place-name-hash* p-name #f)
                    (set! ret (cons (list i (+ i 2)) ret))
                    #f)
                #f)
              #f)
          #f)
      ;; 1 word names:   PLACE
      (if (hash-ref *place-name-hash* word #f)
          (if (and
               (string-prefix? (vector-ref tags i) "NN")
               (not-in-list-find-names-helper ret i (+ i 1))
               (not-in-list-find-names-helper exclusion-list i (+ i 1)))
              (set! ret (cons (list i (+ i 1)) ret))
              #f)
          #f))
    ;; TBD: calculate importance rating based on number of occurences of name in text: can use (count-substring..) defined in utils.rkt
    (set! ret2
          (map (lambda (index-pair)
                 (string-join (vector->list (vector-copy  word-vector (car index-pair) (cadr index-pair))) " "))
               ret))
    ret2))

#|
(define nn (find-human-names '#("President" "George" "Bush" "went" "to" "San" "Diego" "to" "meet" "Ms" "." "Jones" "and" "Gen" "." "Pervez" "Musharraf" ".") '()))
(define nn2 (find-human-names '#("xxx" "eee" "tt") '()))

(display (find-place-names '#("George" "Bush" "went" "to" "San" "Diego" "and" "London") '()))

(define xx (find-place-names '#("George" "Bush" "went" "to" "San" "Diego" "and" "London") '()))
(define xx2 (find-place-names '#("qqqq" "eeee" "gggg") '()))
(define xx3 (find-place-names '#("where" "is" "San" "Francisco") '()))
|#

