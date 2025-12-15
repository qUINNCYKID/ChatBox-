;;; memory_manager.scm
;;; Pure MIT Scheme in-session memory + simple rules engine

(define MODULE_NAME "memory_manager")

;; Compatibility: ensure `string-index` is defined (MIT Scheme lacks a built-in)
;; Returns index of first occurrence of character `ch` in string `str`, or #f.
(define (string-index str ch)
  (let ((n (string-length str)))
    (let loop ((i 0))
      (cond
        ((>= i n) #f)
        ((char=? (string-ref str i) ch) i)
        (else (loop (+ i 1)))))))

;; -----------------------------------------------------------------------------
;; Internal store as alists
;; -----------------------------------------------------------------------------

(define _STORE '())

(define (ensure-context ctx)
  (let ((pair (assoc ctx _STORE)))
    (if pair
        pair
        (begin
          (set! _STORE (cons (cons ctx '()) _STORE))
          (assoc ctx _STORE)))))

(define (remember context key value)
  (let ((ctx-pair (ensure-context context)))
    (let* ((ctx-map (cdr ctx-pair))
           (existing (assoc key ctx-map)))
      (if existing
          (set-cdr! existing value)
          (set-cdr! ctx-pair (cons (cons key value) ctx-map)))))
  #t)

(define (recall context . maybe)
  (let* ((ctx-pair (ensure-context context))
         (ctx-map (cdr ctx-pair)))
    (cond
      ((null? maybe)
       ctx-map)
      ((string? (car maybe))
       (let ((k (car maybe)))
         (let ((p (assoc k ctx-map)))
           (if p (cdr p) #f))))
      (else
       (error "Unsupported recall usage")))))

;; -----------------------------------------------------------------------------
;; String helpers
;; -----------------------------------------------------------------------------

(define (string-down s)
  (string-downcase s))

(define (string-trim s)
  (let loop-left ((i 0))
    (if (>= i (string-length s))
        ""
        (if (char-whitespace? (string-ref s i))
            (loop-left (+ i 1))
            (let loop-right ((j (- (string-length s) 1)))
              (if (char-whitespace? (string-ref s j))
                  (loop-right (- j 1))
                  (substring s i (+ j 1))))))))

(define (split-on-space s)
  (let ((n (string-length s)))
    (let loop ((i 0) (current "") (parts '()))
      (if (>= i n)
          (reverse (if (string=? current "") parts (cons current parts)))
          (let ((c (string-ref s i)))
            (if (char-whitespace? c)
                (if (string=? current "")
                    (loop (+ i 1) "" parts)
                    (loop (+ i 1) "" (cons current parts)))
                (loop (+ i 1)
                      (string-append current (string c))
                      parts)))))))

(define (join-with-space lst)
  (if (null? lst)
      ""
      (let loop ((xs (cdr lst)) (acc (car lst)))
        (if (null? xs)
            acc
            (loop (cdr xs) (string-append acc " " (car xs)))))))

(define (normalize text)
  (join-with-space (split-on-space (string-down (string-trim text)))))

;; -----------------------------------------------------------------------------
;; Pattern match: “my name is *”
;; -----------------------------------------------------------------------------

;; MIT Scheme does NOT have string-index, so we implement it.
(define (string-index str ch)
  (let ((n (string-length str)))
    (let loop ((i 0))
      (cond
        ((>= i n) #f)
        ((char=? (string-ref str i) ch) i)
        (else (loop (+ i 1)))))))

(define (match-pattern pattern text)
  (let* ((p (normalize pattern))
         (t (normalize text)))
    (let ((star-index (string-index p #\*)))
      (if star-index
          ;; pre + * + post
          (let* ((pre (substring p 0 star-index))
                 (post (substring p (+ star-index 1) (string-length p)))
                 (pre-len (string-length pre))
                 (post-len (string-length post)))
            (if (and (>= (string-length t) (+ pre-len post-len))
                     (string=? pre (substring t 0 pre-len))
                     (string=? post (substring t
                             (- (string-length t) post-len)
                             (string-length t))))
                (let ((middle (substring t
                               pre-len
                               (- (string-length t) post-len))))
                  (list middle))
                #f))
          ;; No wildcard
          (if (string=? p t) (list "") #f)))))

;; -----------------------------------------------------------------------------
;; Template replacement {0}
;; -----------------------------------------------------------------------------

(define (replace-all text old new)
  (let loop ((s text))
    (let ((pos (string-search old s)))
      (if pos
          (string-append
           (substring s 0 pos)
           new
           (loop (substring s (+ pos (string-length old)) (string-length s))))
          s))))

(define (apply-template template groups)
  (let loop ((i 0) (gs groups) (res template))
    (if (null? gs)
        res
        (loop (+ i 1)
              (cdr gs)
              (replace-all res
                           (string-append "{" (number->string i) "}")
                           (car gs))))))

;; -----------------------------------------------------------------------------
;; Rules
;; -----------------------------------------------------------------------------

(define DEFAULT_RULES
  (list
   (cons "my name is *"
         (list (cons 'remember (list "user_info" "name" "{0}"))
               (cons 'reply "Nice to meet you, {0}!")))
   (cons "i am *"
         (list (cons 'remember (list "user_info" "name" "{0}"))
               (cons 'reply "Got it — I'll call you {0}.")))
   (cons "i live in *"
         (list (cons 'remember (list "user_info" "location" "{0}"))
               (cons 'reply "I'll remember you live in {0}.")))))

;; -----------------------------------------------------------------------------
;; Apply rules
;; -----------------------------------------------------------------------------

(define (process-rules text)
  (let ((txt (normalize text))
        (replies '()))
    (for-each
     (lambda (rule)
       (let* ((pattern (car rule))
              (actions (cdr rule))
              (matched (match-pattern pattern txt)))
         (when matched
           (for-each
            (lambda (act)
              (let ((atype (car act))
                    (args  (cdr act)))
                (cond
                  ((eq? atype 'remember)
                   (let* ((ctx (car args))
                          (key (cadr args))
                          (val (apply-template (caddr args) matched)))
                     (remember ctx key val)))
                  ((eq? atype 'reply)
                   (let ((msg (apply-template args matched)))
                     (set! replies (cons msg replies))))
                  (else
                   (error "Unsupported action" atype)))))
            actions))))
     DEFAULT_RULES)
    (reverse replies)))

;; -----------------------------------------------------------------------------
;; Public API
;; -----------------------------------------------------------------------------

(define (process-input text)
  (let ((replies (process-rules text)))
    (cons (not (null? replies)) replies)))