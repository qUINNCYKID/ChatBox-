
# input_processing_first_lastname.py
"""
Module: Input Processing
- Normalizes raw user input
- Extracts keywords and maps them to high-level intents
- Demonstrates module-level scope
"""

from typing import Dict, List, Tuple

MODULE_NAME = "input_processing_first_lastname"
SCOPE_DEMO = "SCOPE_DEMO set in input_processing module"

# Central place to declare supported keywords per intent.
# Keys: intent string; Values: list of keywords to search for (lowercase)
INTENT_KEYWORDS: Dict[str, List[str]] = {
    "quit": ["quit", "exit", "bye", "goodbye", "leave", "stop", "end", "close", "see you", "later", "peace", "i'm done", "that's all"],
    "greet": ["hello", "hi", "hey", "good morning", "good afternoon", "good evening", "greetings", "howdy", "sup", "yo", "what's up", "hiya", "hey there", "morning", "evening", "good day"],
    "help": ["help", "options", "what can you do", "commands", "what do you know", "capabilities", "features", "guide", "menu", "show me", "assistance", "support", "info", "information"],
    "stock": [
        "what is a stock",
        "define stock",
        "stock?",
        "stocks",
        "share",
        "shares",
        "equity",
        "equities",
        "what is a share",
        "explain stock",
        "stock definition",
        "tell me about stocks",
        "how do stocks work",
        "what are stocks",
        "what are shares",
        "common stock",
        "preferred stock",
        "company ownership",
        "stock certificate",
        "shareholders",
        "stockholders",
        "voting rights",
        "stock market basics",
        "buying stocks",
        "owning stocks",
        "stock ownership",
    ],
    "etf": [
        "etf",
        "exchange-traded fund",
        "what is an etf",
        "what is etf",
        "explain etf",
        "etfs",
        "exchange traded fund",
        "tell me about etf",
        "how do etfs work",
        "etf definition",
        "index fund",
        "passive investing",
        "basket of stocks",
        "low cost investing",
        "vanguard etf",
        "spy etf",
        "etf vs stock",
        "etf benefits",
        "tracking index",
    ],
    "mutual_fund": [
        "mutual fund",
        "what is a mutual fund",
        "mutual funds",
        "explain mutual fund",
        "how do mutual funds work",
        "mutual fund definition",
        "actively managed fund",
        "fund manager",
        "pooled investment",
        "managed fund",
        "mutual fund fees",
        "nav",
        "net asset value",
        "open-end fund",
        "closed-end fund",
    ],
    "index": [
        "index",
        "s&p 500",
        "sp500",
        "s and p 500",
        "what is the s&p",
        "what is the sp500",
        "market index",
        "stock index",
        "dow jones",
        "nasdaq",
        "dow",
        "what is an index",
        "market benchmark",
        "benchmark index",
        "russell 2000",
        "wilshire 5000",
        "composite index",
        "index tracking",
        "index performance",
        "market indices",
        "djia",
        "nasdaq composite",
    ],
    "dividend": [
        "dividend",
        "dividends",
        "what is a dividend",
        "do companies pay dividends",
        "dividend payout",
        "dividend payment",
        "cash dividend",
        "quarterly dividend",
        "dividend income",
        "dividend stocks",
        "how do dividends work",
        "dividend reinvestment",
        "drip",
        "ex-dividend date",
        "dividend aristocrats",
        "dividend growth",
        "high dividend",
        "dividend paying stocks",
        "stock dividend",
        "special dividend",
    ],
    "bull_bear": [
        "bull vs bear",
        "bull and bear",
        "bull market",
        "bear market",
        "bullish",
        "bearish",
        "bull run",
        "market crash",
        "market correction",
        "rally",
        "downturn",
        "recession",
        "market sentiment",
        "upturn",
        "market decline",
        "market recovery",
        "volatility",
        "market volatility",
        "uptrend",
        "downtrend",
    ],
    "ipo": [
        "ipo",
        "initial public offering",
        "what is an ipo",
        "going public",
        "public offering",
        "how does ipo work",
        "company goes public",
        "direct listing",
        "ipo process",
        "underwriter",
        "ipo pricing",
        "new issue",
        "private to public",
        "stock debut",
        "ipo launch",
    ],
    "risk_return": [
        "risk and return",
        "risk vs return",
        "risk return",
        "risk tolerance",
        "expected return",
        "investment risk",
        "return on investment",
        "roi",
        "high risk",
        "low risk",
        "risk reward",
        "risk management",
        "safe investment",
        "risky investment",
        "conservative investing",
        "aggressive investing",
        "risk assessment",
        "volatility risk",
        "market risk",
    ],
    "diversify": [
        "diversify",
        "diversification",
        "how to diversify",
        "diversifying",
        "portfolio diversification",
        "spread risk",
        "asset allocation",
        "balanced portfolio",
        "mix of investments",
        "diversified portfolio",
        "portfolio balance",
        "spreading investments",
        "asset mix",
        "portfolio strategy",
        "don't put all eggs in one basket",
        "multiple assets",
        "sector diversification",
    ],
    "market_vs_limit": [
        "market vs limit",
        "market order",
        "limit order",
        "difference market and limit",
        "order types",
        "types of orders",
        "what is market order",
        "what is limit order",
        "buy at market",
        "sell at limit",
        "order execution",
        "fill price",
        "guaranteed execution",
        "price control",
    ],
    "stop_order": [
        "stop loss",
        "stop-loss",
        "stop order",
        "stop limit",
        "what is a stop order",
        "trailing stop",
        "stop loss order",
        "protective stop",
        "stop market",
        "trigger price",
        "automatic sell",
        "cut losses",
        "exit strategy",
        "protective order",
    ],
    "ticker": ["ticker", "symbol", "stock symbol", "symbol for", "ticker symbol", "stock ticker", "trading symbol", "stock code", "ticker code", "stock abbreviation", "company symbol"],
    "bid_ask": ["bid", "ask", "spread", "bid ask", "bid-ask spread", "bid price", "ask price", "quoted price", "buy price", "sell price", "market spread", "price spread", "liquidity spread"],
    "market_cap": [
        "market cap",
        "capitalization",
        "market capitalization",
        "market value",
        "company size",
        "large cap",
        "small cap",
        "mid cap",
        "mega cap",
        "micro cap",
        "nano cap",
        "company valuation",
        "total value",
        "market worth",
        "cap size",
    ],
    "volume": [
        "volume",
        "trading volume",
        "share volume",
        "daily volume",
        "how many shares traded",
        "liquidity",
        "trading activity",
        "shares traded",
        "transaction volume",
        "average volume",
        "high volume",
        "low volume",
        "volume indicator",
    ],
    "trading_hours": [
        "trading hours",
        "pre-market",
        "premarket",
        "after-hours",
        "after hours",
        "what time does market open",
        "when does market close",
        "market hours",
        "extended hours",
        "regular trading hours",
        "opening time",
        "closing time",
        "market schedule",
        "trading times",
        "when can i trade",
        "market open",
        "market close",
    ],
    "pe_ratio": ["p/e", "pe ratio", "price to earnings", "price/earnings", "price earnings ratio", "earnings multiple", "valuation ratio", "pe multiple", "price-to-earnings"],
    "dividend_yield": ["dividend yield", "yield", "dividend percentage", "dividend rate", "income yield", "payout ratio", "annual yield", "dividend return"],
    "dca": ["dca", "dollar-cost", "dollar-cost averaging", "dollar cost averaging", "dollar cost", "regular investing", "systematic investing", "averaging down", "periodic investing", "auto-invest"],
    "long_short": ["long vs short", "long and short", "long short", "shorting", "short sell", "short selling", "going long", "going short", "long position", "short position", "bear position", "buy and hold", "betting against"],
    "what_moves_price": [
        "what moves",
        "why did it go up",
        "why did it go down",
        "moves price",
        "what moves the price",
        "what affects price",
        "price movement",
        "stock price changes",
        "what drives price",
        "factors affecting price",
        "supply and demand",
        "price drivers",
        "market forces",
        "price fluctuations",
        "why prices change",
        "stock movements",
        "market movers",
        "earnings impact",
        "news impact",
    ],
    "guardrail": [
        "real-time",
        "today",
        "tomorrow",
        "buy",
        "sell",
        "what stock",
        "real time data",
        "should i buy",
        "should i sell",
        "stock picks",
        "recommendations",
        "advice",
        "which stock to buy",
        "best stock",
        "hot stocks",
        "top picks",
        "invest in",
        "buy now",
        "sell now",
        "current price",
        "latest price",
        "stock recommendation",
        "financial advice",
        "investment advice",
        "what to invest in",
    ]
}

def normalize(text: str) -> str:
    """Lowercase and strip whitespace; squeeze internal spaces."""
    cleaned = " ".join(text.lower().strip().split())
    return cleaned

def detect_intent(user_text: str) -> Tuple[str, List[str]]:
    """Return (intent, matched_keywords). Default to 'unknown'."""
    txt = normalize(user_text)
    matches: List[str] = []

    # Exact quit match takes precedence
    if txt in INTENT_KEYWORDS["quit"]:
        return "quit", [txt]

    # Build a flat list of (keyword, intent) and match the longest keywords
    # across all intents first. This prevents a short keyword from one intent
    # shadowing a longer, more specific keyword in another intent.
    flat: List[Tuple[str, str]] = []
    for intent, kws in INTENT_KEYWORDS.items():
        if intent == "quit":
            continue
        for kw in kws:
            flat.append((kw, intent))

    # Sort by keyword length (descending) so longer phrases are checked first
    flat.sort(key=lambda ki: len(ki[0]), reverse=True)

    for kw, intent in flat:
        if kw in txt:
            # Gather all matching keywords for the selected intent (longest-first)
            intent_matches: List[str] = []
            for kw2 in sorted(INTENT_KEYWORDS.get(intent, []), key=len, reverse=True):
                if kw2 in txt:
                    intent_matches.append(kw2)
            return intent, intent_matches

    return "unknown", []

def get_scope_demo() -> str:
    """Expose module-scope variable to demonstrate namespaces."""
    return SCOPE_DEMO

if __name__ == "__main__":
    # Quick manual test
    tests = ["Hi", "What is a stock", "Tell me about P/E", "quit"]
    for t in tests:
        print(t, "->", detect_intent(t))
