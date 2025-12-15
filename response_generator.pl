% response_generator.pl
% Translated from response_generator_stockmarket.py
:- module(response_generator, [generate_response/5, help_text/1, get_scope_demo_rg/1]).
% generate_response(+Intent, +UserText, +Matches, +State, -Reply)

get_scope_demo_rg('SCOPE_DEMO set in response_generator module').

supported_keywords_examples(['stock','ETF','index/S&P 500','dividends','market vs limit','stop loss','ticker','bid/ask','market cap','volume','trading hours','P/E','dividend yield','DCA','long vs short','IPO','risk & return','diversification']).

help_text(Text) :-
    supported_keywords_examples(Ex),
    atomic_list_concat(Ex, ', ', Topics),
    atomic_list_concat(['I can explain core investing concepts: ', Topics, '. This is educational only—no real-time data or advice.'], Text).

% generate_response(+Intent, +UserText, +Matches, +State, -Reply)
% We expose generate_response/4 where Reply is returned as an atom.
% generate_response(+Intent, +UserText, +Matches, +State, -Reply)
generate_response(Intent, _UserText, _Matches, State, Reply) :-
    (   get_turns(State, Turns) ; Turns = 0 ),
    response_for_intent(Intent, Turns, Reply).
get_turns(State, Turns) :-
    (   is_dict(State), get_dict(turn_count, State, Turns) -> true ; false).

response_for_intent(greet, _T, 'Hello! Ask me stock-market basics, or type \'help\'.').
response_for_intent(help, _T, R) :- help_text(R).
response_for_intent(stock, _T, 'A stock is ownership in a company. Shareholders may benefit from price gains and sometimes dividends.').
response_for_intent(etf, _T, 'An ETF is a basket of assets you trade like one stock. It offers built-in diversification and usually low fees.').
response_for_intent(mutual_fund, _T, 'Mutual funds pool investor money and are priced once per day. ETFs trade intraday; mutual funds do not.').
response_for_intent(index, _T, 'An index tracks a group of securities. The S&P 500 tracks roughly 500 large U.S. companies and is a benchmark for \'the market.\'').
response_for_intent(dividend, _T, 'A dividend is a portion of company profits paid to shareholders (often cash). Not all companies pay them.').
response_for_intent(bull_bear, _T, 'Bull market: prices rising. Bear market: prices falling.').
response_for_intent(ipo, _T, 'IPO = Initial Public Offering: when a private company sells shares to the public for the first time.').
response_for_intent(risk_return, _T, 'Higher potential return generally comes with higher risk. Diversification and a long-term plan help manage (not eliminate) risk.').
response_for_intent(diversify, _T, 'Diversification spreads money across many assets so one poor performer hurts less.').
response_for_intent(market_vs_limit, _T, 'Market orders fill now at the best price; limit orders set a max buy or min sell and only fill at that price or better.').
response_for_intent(stop_order, _T, 'Stop order triggers at a set price. Stop becomes market; stop-limit becomes limit. Used to cap losses or lock gains.').
response_for_intent(ticker, _T, 'A ticker (symbol) identifies a stock/fund on an exchange (e.g., AAPL for Apple).').
response_for_intent(bid_ask, _T, 'Bid = highest buyer price; Ask = lowest seller price; Spread = difference. Tighter spreads often mean more liquidity.').
response_for_intent(market_cap, _T, 'Market cap = share price × shares outstanding. It’s a rough size indicator (small-, mid-, large-cap).').
response_for_intent(volume, _T, 'Volume is how many shares trade in a period. High volume can signal strong interest or important news.').
response_for_intent(trading_hours, _T, 'Regular U.S. hours: 9:30am–4:00pm ET. Some brokers offer pre-market and after-hours with lower liquidity and wider spreads.').
response_for_intent(pe_ratio, _T, 'P/E = price per share ÷ earnings per share. Higher P/E can imply higher growth expectations.').
response_for_intent(dividend_yield, _T, 'Dividend yield = annual dividends per share ÷ price per share. High yield can also signal risk—context matters.').
response_for_intent(dca, _T, 'Dollar-cost averaging invests a fixed amount on a schedule, buying more when prices are low and fewer when high.').
response_for_intent(long_short, _T, '\'Long\' = you own shares hoping they rise. \'Short\' = borrow and sell now, buy back lower later—higher risk.').
response_for_intent(what_moves_price, _T, 'Prices move with supply/demand, influenced by earnings, guidance, rates, economic data, sector trends, and news.').
response_for_intent(guardrail, _T, 'I don’t provide real-time data or recommendations—only general education.').
response_for_intent(quit, _T, 'Goodbye! Educational use only—this is not financial advice.').
response_for_intent(_, _T, 'I\'m not sure about that. Try \'help\' to see topics I cover.').
