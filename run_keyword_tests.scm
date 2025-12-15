;;; run_keyword_tests.scm
;;; Simple test harness to check that each declared keyword maps to the expected intent

(load "input_processing_first_lastname.scm")

(define (run-tests)
  (let ((total 0) (passed 0) (failures '()))
    (for-each
     (lambda (pair)
       (let ((intent (car pair)) (kws (cdr pair)))
         (for-each (lambda (kw)
                     (set! total (+ total 1))
                     (let ((detected (detect-intent kw)))
                       (if (eq? (car detected) intent)
                           (set! passed (+ passed 1))
                           (set! failures (cons (list intent kw detected) failures))))) kws)))
     INTENT_KEYWORDS)
    (display "Tested ") (display total) (display " keywords: ") (display passed) (display " passed, ") (display (- total passed)) (display " failed") (newline)
    (if (null? failures)
        (display "All keywords matched their expected intents.")
        (begin (display "Failures:\n") (for-each (lambda (f) (display f) (newline)) failures)))
    (newline)))

;; To run tests, load this file and call (run-tests)
