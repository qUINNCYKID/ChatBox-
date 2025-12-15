% input_processing.pl
% Translated from input_processing_first_lastname.py
:- module(input_processing, [detect_intent/3, normalize/2, get_scope_demo/1]).

get_scope_demo('SCOPE_DEMO set in input_processing module').

% intent_keyword(Intent, Keyword).
% Keywords are all lowercase atoms (quoted when they contain spaces/punctuation).

% quit
intent_keyword(quit, 'quit').
intent_keyword(quit, 'exit').
intent_keyword(quit, 'bye').

% greet
intent_keyword(greet, 'hello').
intent_keyword(greet, 'hi').
intent_keyword(greet, 'hey').
intent_keyword(greet, 'good morning').
intent_keyword(greet, 'good afternoon').
intent_keyword(greet, 'good evening').

% help
intent_keyword(help, 'help').
intent_keyword(help, 'options').
intent_keyword(help, 'what can you do').
intent_keyword(help, 'commands').

% stock
intent_keyword(stock, 'what is a stock').
intent_keyword(stock, 'define stock').
intent_keyword(stock, 'stock?').
intent_keyword(stock, 'stocks').
intent_keyword(stock, 'share').
intent_keyword(stock, 'shares').
intent_keyword(stock, 'equity').
intent_keyword(stock, 'equities').
intent_keyword(stock, 'what is a share').
intent_keyword(stock, 'explain stock').

% etf
intent_keyword(etf, 'etf').
intent_keyword(etf, 'exchange-traded fund').
intent_keyword(etf, 'what is an etf').
intent_keyword(etf, 'what is etf').
intent_keyword(etf, 'explain etf').
intent_keyword(etf, 'etfs').

% mutual_fund
intent_keyword(mutual_fund, 'mutual fund').
intent_keyword(mutual_fund, 'what is a mutual fund').
intent_keyword(mutual_fund, 'mutual funds').
intent_keyword(mutual_fund, 'explain mutual fund').

% index
intent_keyword(index, 'index').
intent_keyword(index, 's&p 500').
intent_keyword(index, 'sp500').
intent_keyword(index, 's and p 500').
intent_keyword(index, 'what is the s&p').
intent_keyword(index, 'what is the sp500').
intent_keyword(index, 'market index').

% dividend
intent_keyword(dividend, 'dividend').
intent_keyword(dividend, 'dividends').
intent_keyword(dividend, 'what is a dividend').
intent_keyword(dividend, 'do companies pay dividends').
intent_keyword(dividend, 'dividend payout').

% bull_bear
intent_keyword(bull_bear, 'bull vs bear').
intent_keyword(bull_bear, 'bull and bear').
intent_keyword(bull_bear, 'bull market').
intent_keyword(bull_bear, 'bear market').
intent_keyword(bull_bear, 'bullish').
intent_keyword(bull_bear, 'bearish').

% ipo
intent_keyword(ipo, 'ipo').
intent_keyword(ipo, 'initial public offering').
intent_keyword(ipo, 'what is an ipo').
intent_keyword(ipo, 'going public').

% risk_return
intent_keyword(risk_return, 'risk and return').
intent_keyword(risk_return, 'risk vs return').
intent_keyword(risk_return, 'risk return').
intent_keyword(risk_return, 'risk tolerance').
intent_keyword(risk_return, 'expected return').

% diversify
intent_keyword(diversify, 'diversify').
intent_keyword(diversify, 'diversification').
intent_keyword(diversify, 'how to diversify').
intent_keyword(diversify, 'diversifying').

% market_vs_limit
intent_keyword(market_vs_limit, 'market vs limit').
intent_keyword(market_vs_limit, 'market order').
intent_keyword(market_vs_limit, 'limit order').
intent_keyword(market_vs_limit, 'difference market and limit').

% stop_order
intent_keyword(stop_order, 'stop loss').
intent_keyword(stop_order, 'stop-loss').
intent_keyword(stop_order, 'stop order').
intent_keyword(stop_order, 'stop limit').
intent_keyword(stop_order, 'what is a stop order').

% ticker
intent_keyword(ticker, 'ticker').
intent_keyword(ticker, 'symbol').
intent_keyword(ticker, 'stock symbol').
intent_keyword(ticker, 'symbol for').

% bid_ask
intent_keyword(bid_ask, 'bid').
intent_keyword(bid_ask, 'ask').
intent_keyword(bid_ask, 'spread').
intent_keyword(bid_ask, 'bid ask').

% market_cap
intent_keyword(market_cap, 'market cap').
intent_keyword(market_cap, 'capitalization').
intent_keyword(market_cap, 'market capitalization').

% volume
intent_keyword(volume, 'volume').
intent_keyword(volume, 'trading volume').
intent_keyword(volume, 'share volume').

% trading_hours
intent_keyword(trading_hours, 'trading hours').
intent_keyword(trading_hours, 'pre-market').
intent_keyword(trading_hours, 'premarket').
intent_keyword(trading_hours, 'after-hours').
intent_keyword(trading_hours, 'after hours').
intent_keyword(trading_hours, 'what time does market open').

% pe_ratio
intent_keyword(pe_ratio, 'p/e').
intent_keyword(pe_ratio, 'pe ratio').
intent_keyword(pe_ratio, 'price to earnings').
intent_keyword(pe_ratio, 'price/earnings').

% dividend_yield
intent_keyword(dividend_yield, 'dividend yield').
intent_keyword(dividend_yield, 'yield').
intent_keyword(dividend_yield, 'dividend percentage').

% dca
intent_keyword(dca, 'dca').
intent_keyword(dca, 'dollar-cost').
intent_keyword(dca, 'dollar-cost averaging').
intent_keyword(dca, 'dollar cost averaging').
intent_keyword(dca, 'dollar cost').

% long_short
intent_keyword(long_short, 'long vs short').
intent_keyword(long_short, 'long and short').
intent_keyword(long_short, 'long short').
intent_keyword(long_short, 'shorting').
intent_keyword(long_short, 'short sell').

% what_moves_price
intent_keyword(what_moves_price, 'what moves').
intent_keyword(what_moves_price, 'why did it go up').
intent_keyword(what_moves_price, 'why did it go down').
intent_keyword(what_moves_price, 'moves price').
intent_keyword(what_moves_price, 'what moves the price').
intent_keyword(what_moves_price, 'what affects price').

% guardrail (used to detect real-time / advice requests)
intent_keyword(guardrail, 'real-time').
intent_keyword(guardrail, 'today').
intent_keyword(guardrail, 'tomorrow').
intent_keyword(guardrail, 'buy').
intent_keyword(guardrail, 'sell').
intent_keyword(guardrail, 'what stock').
intent_keyword(guardrail, 'real time data').

% normalize(+RawString, -NormalizedAtom)
% Lowercases, trims and squeezes internal spaces.
normalize(Raw, Normalized) :-
    (   atom(Raw) -> Atom = Raw ; atom_string(Atom, Raw) ),
    downcase_atom(Atom, Lower),
    % split on whitespace and recombine with single spaces
    split_string(Lower, " \t\n\r", " \t\n\r", Parts),
    atomic_list_concat(Parts, ' ', Normalized).

% detect_intent(+UserText, -Intent, -Matches)
% Matches is a list of matching keyword atoms (in order found for the first matching intent).
detect_intent(UserText, Intent, Matches) :-
    normalize(UserText, Norm),
    % Check exact quit match first
    (   intent_keyword(quit, Norm) ->
        Intent = quit,
        Matches = [Norm]
    ;   % Otherwise, try each intent (except quit) and return on first with matches
        setof(I, K^(intent_keyword(I,K), I \= quit), Intents),
        find_first_intent_match(Intents, Norm, Intent, Matches)
    ), !.

% find_first_intent_match(+IntentList, +Norm, -Intent, -Matches)
find_first_intent_match([], _, unknown, []).
find_first_intent_match([I|Is], Norm, Intent, Matches) :-
    findall(K, (intent_keyword(I,K), sub_atom(Norm, _, _, _, K)), RawMatches),
    (   RawMatches \= [] ->
        % sort matches by descending length to mimic Python's longest-first
        map_list_to_pairs(atom_length, RawMatches, Pairs),
        keysort(Pairs, SortedAsc),
        reverse(SortedAsc, SortedDesc),
        pairs_values(SortedDesc, Matches),
        Intent = I
    ;   find_first_intent_match(Is, Norm, Intent, Matches)
    ).
