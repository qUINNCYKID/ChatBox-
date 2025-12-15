;;; input_processing_first_lastname.scm
;;; Input normalization and intent detection for the stock chatbot (pure Scheme version)

(define MODULE_NAME "input_processing_first_lastname")
(define IP_SCOPE_DEMO "SCOPE_DEMO set in input_processing module")

;; Getter so other modules can query this module's scope demo without name collisions
(define (get-scope-demo-ip) IP_SCOPE_DEMO)

;; -----------------------------------------------------------------------------
;; Small string helpers
;; -----------------------------------------------------------------------------

(define (string-trim-left s)
  (let loop ((i 0))
    (if (>= i (string-length s))
        ""
        (if (char-whitespace? (string-ref s i))
            (loop (+ i 1))
            (substring s i (string-length s))))))

(define (string-trim-right s)
  (let loop ((i (- (string-length s) 1)))
    (if (< i 0)
        ""
        (if (char-whitespace? (string-ref s i))
            (loop (- i 1))
            (substring s 0 (+ i 1))))))

(define (string-trim s)
  (string-trim-right (string-trim-left s)))

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
            (loop (cdr xs)
                  (string-append acc " " (car xs)))))))

(define (normalize s)
  (let* ((lower (string-downcase s))
         (trimmed (string-trim lower))
         (parts (split-on-space trimmed)))
    (join-with-space parts)))

(define (contains-substring? s sub)
  (let* ((n (string-length s))
         (m (string-length sub)))
    (let loop ((i 0))
      (cond
        ((> (+ i m) n) #f)
        ((string=? (substring s i (+ i m)) sub) #t)
        (else (loop (+ i 1)))))))

;; -----------------------------------------------------------------------------
;; Intent declarations
;; -----------------------------------------------------------------------------

;; Association list:
;;   (intent-symbol . ("keyword phrase 1" "keyword phrase 2" ...))
(define INTENT_KEYWORDS
  '((quit . ("quit" "exit" "bye"))
    (greet . ("hello" "hi" "hey" "good morning" "good afternoon" "good evening"))
    (help  . ("help" "options" "what can you do" "commands" "topics"))
    (stock . ("what is a stock" "define stock" "stock" "stocks" "what is a share" "shares" "equity" "equities"))
    (etf   . ("etf" "exchange-traded fund" "what is an etf" "explain etf" "etfs"))
    (mutual_fund . ("mutual fund" "what is a mutual fund" "mutual funds" "explain mutual fund"))
    (index . ("index" "s&p 500" "sp500" "s and p 500" "market index"))
    (dividend . ("dividend" "dividends" "what is a dividend" "do companies pay dividends"))
    (order_types . ("market order" "limit order" "market vs limit" "order type" "stop loss" "stop order"))
    (risk_return . ("risk" "return" "risk and return" "volatility" "downside"))
    (diversification . ("diversification" "diversify" "spread my money" "single stock risk"))
    (dca . ("dollar cost averaging" "dca" "invest every month"))
    (long_short . ("long vs short" "short selling" "short a stock" "go long" "go short"))
    (ticker . ("ticker" "symbol" "stock symbol" "what is a ticker"))
    (bid_ask . ("bid" "ask" "bid ask spread" "spread"))
    (market_cap . ("market cap" "market capitalization" "large cap" "small cap" "mid cap"))
    (volume . ("volume" "trading volume"))
    (trading_hours . ("trading hours" "market hours" "when does the market open" "when does the market close"))
    (pe_ratio . ("p/e" "pe ratio" "price to earnings"))
    (dividend_yield . ("dividend yield" "yield on stock"))
    (what_moves_price . ("what moves stock prices" "why did my stock go up" "why did my stock go down"))))

;; -----------------------------------------------------------------------------
;; Intent detection
;; -----------------------------------------------------------------------------

;; detect-intent : string -> (intent-symbol . matched-keywords-list)
(define (detect-intent raw-text)
  (let* ((norm (normalize raw-text)))
    (let loop-intents ((pairs INTENT_KEYWORDS)
                       (best-intent 'unknown)
                       (best-matches '())
                       (best-score 0))
      (if (null? pairs)
          (cons best-intent (reverse best-matches))
          (let* ((pair (car pairs))
                 (intent (car pair))
                 (kws (cdr pair))
                 (found '())
                 (score 0))
            (for-each
             (lambda (kw)
               (let ((norm-kw (normalize kw)))
                 (if (contains-substring? norm norm-kw)
                     (begin
                       (set! found (cons kw found))
                       (let ((len (string-length norm-kw)))
                         (if (> len score)
                             (set! score len)))))))
             kws)
            (if (> score best-score)
                (loop-intents (cdr pairs) intent found score)
                (loop-intents (cdr pairs) best-intent best-matches best-score)))))))

;; Convenience: expose normalize so other modules can re-use it if needed
(define normalize-input normalize)