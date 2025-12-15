;;; main_first_lastname.scm
;;; Scheme main entry point for the modular stock chatbot

(load "input_processing_first_lastname.scm")
(load "response_generator_stockmarket.scm")
(load "memory_manager.scm")

(define MODULE_NAME "main_first_lastname")
(define MAIN_SCOPE_DEMO "SCOPE_DEMO set in main module")

(define TURN_COUNT 0)

;; Portable defined? helper: returns #t if symbol is bound in the interaction environment
;; Uses eval and an exception handler to avoid crashing on unbound symbols.
(define (defined? sym)
  (let ((ok #t))
    (with-exception-handler
      (lambda (ex) (set! ok #f) #f)
      (lambda () (begin (eval sym (interaction-environment)) #t)))
    ok))

;; Helper to safely call get-scope-demo if the module exports it
(define (safe-scope-demo)
  (if (defined? 'get-scope-demo-rg)
      (get-scope-demo-rg)
      "NO SCOPE_DEMO EXPORTED"))

(define (stock-chatbot)
  (let ((local-scope "SCOPE_DEMO defined locally inside stock_chatbot"))
    (display "Hi! I'm a modular Stock Market chatbot. Type 'quit' to exit.") (newline)
    (display "Type 'help' to see what I cover.") (newline) (newline)

    ;; Debug / scope demo
    (display "[scope] main.SCOPE_DEMO: ") (display MAIN_SCOPE_DEMO) (newline)
    (display "[scope] local SCOPE_DEMO: ") (display local-scope) (newline)
    (display "[scope] input_processing SCOPE_DEMO: ")
    (if (defined? 'get-scope-demo-ip)
      (display (get-scope-demo-ip))
      (display "NO SCOPE_DEMO EXPORTED")) (newline)
    (display "[scope] response_generator SCOPE_DEMO: ") (display (safe-scope-demo)) (newline)
    (newline)

    (let loop ((turn-count 0))
      (display "You: ") (flush-output)
      (let ((line (read-line)))
        (let ((user-text (if (eof-object? line) "quit" line)))

          ;; Memory manager (process-input)
          (let ((handled+replies (process-input user-text)))
            (let ((handled  (car handled+replies))
                  (replies  (cdr handled+replies)))

              (if handled
                  ;; Memory manager handled the message
                  (begin
                    (for-each
                     (lambda (r)
                       (display "Chatbot: ") (display r) (newline))
                     replies)
                    (loop turn-count))

                  ;; Otherwise detect intent and generate a normal response
                  (let* ((intent+matches (detect-intent user-text))
                         (intent        (car intent+matches))
                         (matches       (cdr intent+matches))
                         (new-turn      (+ turn-count 1))
                         (state         (cons (cons 'turn_count new-turn) '())))
                    (let ((reply (generate-response intent user-text matches state)))
                      (display "Chatbot: ") (display reply) (newline)

                      (if (eq? intent 'quit)
                          'done
                          (loop new-turn))))))))))))

(define (main)
  (stock-chatbot))