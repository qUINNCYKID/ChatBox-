;;; response_generator_stockmarket.scm
;;; Pure Scheme response generator for the modular stock chatbot

(define MODULE_NAME "response_generator_stockmarket")
(define RG_SCOPE_DEMO "SCOPE_DEMO set in response_generator module")

;; This is just for help-text example generation
(define SUPPORTED_KEYWORDS_EXAMPLES
  '("stock" "ETF" "index/S&P 500" "dividends" "market vs limit"
    "stop loss" "ticker" "bid/ask" "market cap" "volume"
    "trading hours" "P/E" "dividend yield" "DCA" "long vs short"
    "IPO" "risk & return" "diversification"))

;; Build a comma-separated help text list
(define (help-text)
  (let loop ((ls (cdr SUPPORTED_KEYWORDS_EXAMPLES))
             (acc (car SUPPORTED_KEYWORDS_EXAMPLES)))
    (if (null? ls)
        (string-append
         "I can explain core investing concepts: "
         acc
         ". This is educational only—no real-time data or advice.")
        (loop (cdr ls)
              (string-append acc ", " (car ls))))))

;; Main response generator
(define (generate-response intent user-text matches state)
  (let ((turns
         (if (and (pair? state) (assoc 'turn_count state))
             (cdr (assoc 'turn_count state))
             0)))
    (cond
      ;; Basic intents
      ((eq? intent 'greet)
       "Hello! Ask me stock-market basics, or type 'help'.")

      ((eq? intent 'help)
       (help-text))

      ;; Investment concepts
      ((eq? intent 'stock)
       "A stock is ownership in a company. Shareholders may benefit from price gains and sometimes dividends.")

      ((eq? intent 'etf)
       "An ETF is a basket of assets you trade like one stock. Built-in diversification, low fees.")

      ((eq? intent 'mutual_fund)
       "Mutual funds pool investor money and are priced once per day. ETFs trade intraday; mutual funds do not.")

      ((eq? intent 'index)
       "An index tracks a group of securities. The S&P 500 tracks ~500 large U.S. companies.")

      ((eq? intent 'dividend)
       "A dividend is a portion of profits paid to shareholders. Not all companies pay them.")

      ((eq? intent 'risk_return)
       "Higher potential return usually comes with higher risk. Diversification helps manage (not eliminate) risk.")

      ((eq? intent 'diversification)
       "Diversification spreads money across many assets so one bad performer doesn’t dominate results.")

      ((eq? intent 'dca)
       "Dollar-cost averaging invests a fixed amount regularly, buying more when prices are low.")

      ((eq? intent 'long_short)
       "'Long' = own shares hoping they rise. 'Short' = borrow/sell first, buy back lower later—higher risk.")

      ((eq? intent 'order_types)
       "Market orders fill immediately. Limit orders fill only at your chosen price or better.")

      ((eq? intent 'stop_loss)
       "A stop order triggers at a set price. Stop becomes market; stop-limit becomes limit.")

      ;; Market mechanics
      ((eq? intent 'ticker)
       "A ticker symbol identifies a stock or fund on an exchange (e.g., AAPL for Apple).")

      ((eq? intent 'bid_ask)
       "Bid = highest buyer price. Ask = lowest seller price. Spread = difference. Tighter spreads = more liquidity.")

      ((eq? intent 'market_cap)
       "Market cap = share price × shares outstanding. Roughly measures company size (small/mid/large-cap).")

      ((eq? intent 'volume)
       "Volume is how many shares trade within a time period. High volume often means news or strong interest.")

      ((eq? intent 'trading_hours)
       "Regular U.S. hours: 9:30 AM–4:00 PM ET. Pre-market and after-hours exist but have lower liquidity.")

      ((eq? intent 'pe_ratio)
       "P/E = price per share ÷ earnings per share. High P/E can imply growth expectations (or overvaluation).")

      ((eq? intent 'dividend_yield)
       "Dividend yield = annual dividends per share ÷ share price. High yield can also signal risk—context matters.")

      ((eq? intent 'what_moves_price)
       "Prices move because of supply/demand, earnings, guidance, rates, economic data, sector trends, and news.")

      ;; Guardrail
      ((eq? intent 'guardrail)
       "I only provide general educational information—no real-time data or financial recommendations.")

      ;; Exit
      ((eq? intent 'quit)
       "Goodbye! Educational use only—this is not financial advice.")

      ;; Default fallback
      (else
       "I'm not sure about that. Try 'help' to see topics I cover."))))

      ;; Optional small accessor so other modules can query this module's SCOPE_DEMO
      ;; (main_first_lastname.scm expects a `get-scope-demo` if exported)
      (define (get-scope-demo-rg) RG_SCOPE_DEMO)