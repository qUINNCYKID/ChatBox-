
# response_generator_first_lastname.py
"""
Module: Response Generator
- Maps high-level intents to natural-language responses
- Keeps its own module-level constants/state to demonstrate scope
"""
from typing import Dict, List

MODULE_NAME = "response_generator_stockmarket"
SCOPE_DEMO = "SCOPE_DEMO set in response_generator module"

# Public constant used by main() to display capabilities
SUPPORTED_KEYWORDS_EXAMPLES = [
    "stock", "ETF", "index/S&P 500", "dividends", "market vs limit",
    "stop loss", "ticker", "bid/ask", "market cap", "volume",
    "trading hours", "P/E", "dividend yield", "DCA", "long vs short",
    "IPO", "risk & return", "diversification"
]

def help_text() -> str:
    topics = ", ".join(SUPPORTED_KEYWORDS_EXAMPLES)
    return (
        "I can explain core investing concepts: "
        + topics +
        ". This is educational only—no real-time data or advice."
    )

def generate_response(intent: str, user_text: str, matches: List[str], state: Dict) -> str:
    """Return a response string based on detected intent."""
    # Example of reading a value written by main() (demonstrates cross-module state pass)
    turns = state.get("turn_count", 0)

    if intent == "greet":
        return "Hello! Ask me stock-market basics, or type 'help'."
    if intent == "help":
        return help_text()
    if intent == "stock":
        return "A stock is ownership in a company. Shareholders may benefit from price gains and sometimes dividends."
    if intent == "etf":
        return "An ETF is a basket of assets you trade like one stock. It offers built-in diversification and usually low fees."
    if intent == "mutual_fund":
        return "Mutual funds pool investor money and are priced once per day. ETFs trade intraday; mutual funds do not."
    if intent == "index":
        return "An index tracks a group of securities. The S&P 500 tracks roughly 500 large U.S. companies and is a benchmark for 'the market.'"
    if intent == "dividend":
        return "A dividend is a portion of company profits paid to shareholders (often cash). Not all companies pay them."
    if intent == "bull_bear":
        return "Bull market: prices rising. Bear market: prices falling."
    if intent == "ipo":
        return "IPO = Initial Public Offering: when a private company sells shares to the public for the first time."
    if intent == "risk_return":
        return "Higher potential return generally comes with higher risk. Diversification and a long-term plan help manage (not eliminate) risk."
    if intent == "diversify":
        return "Diversification spreads money across many assets so one poor performer hurts less."
    if intent == "market_vs_limit":
        return "Market orders fill now at the best price; limit orders set a max buy or min sell and only fill at that price or better."
    if intent == "stop_order":
        return "Stop order triggers at a set price. Stop becomes market; stop-limit becomes limit. Used to cap losses or lock gains."
    if intent == "ticker":
        return "A ticker (symbol) identifies a stock/fund on an exchange (e.g., AAPL for Apple)."
    if intent == "bid_ask":
        return "Bid = highest buyer price; Ask = lowest seller price; Spread = difference. Tighter spreads often mean more liquidity."
    if intent == "market_cap":
        return "Market cap = share price × shares outstanding. It’s a rough size indicator (small-, mid-, large-cap)."
    if intent == "volume":
        return "Volume is how many shares trade in a period. High volume can signal strong interest or important news."
    if intent == "trading_hours":
        return "Regular U.S. hours: 9:30am–4:00pm ET. Some brokers offer pre-market and after-hours with lower liquidity and wider spreads."
    if intent == "pe_ratio":
        return "P/E = price per share ÷ earnings per share. Higher P/E can imply higher growth expectations."
    if intent == "dividend_yield":
        return "Dividend yield = annual dividends per share ÷ price per share. High yield can also signal risk—context matters."
    if intent == "dca":
        return "Dollar-cost averaging invests a fixed amount on a schedule, buying more when prices are low and fewer when high."
    if intent == "long_short":
        return "'Long' = you own shares hoping they rise. 'Short' = borrow and sell now, buy back lower later—higher risk."
    if intent == "what_moves_price":
        return "Prices move with supply/demand, influenced by earnings, guidance, rates, economic data, sector trends, and news."
    if intent == "guardrail":
        return "I don’t provide real-time data or recommendations—only general education."
    if intent == "quit":
        return "Goodbye! Educational use only—this is not financial advice."
    return "I’m not sure about that. Try 'help' to see topics I cover."

if __name__ == "__main__":
    print(help_text())
