# 📘 Modular Stock Market Chatbot (Prolog Version)

A fully modular, educational Stock Market chatbot implemented in SWI-Prolog, designed to demonstrate:

- **Modular programming**
- **Logic programming paradigm**
- **Intent detection via pattern matching**
- **Rule-based knowledge representation**
- **Separation of concerns**

This project is ideal for coursework, logic-programming practice, and demonstrating mastery of Prolog.

## 📁 Project Structure

```
/prolog/prolog
 ├── main_stock.pl
 ├── input_processing.pl
 ├── response_generator.pl
 ├── memory_manager.pl
 ├── run_tests.pl
 └── README.md
```

### File Descriptions

| File | Purpose |
|------|---------|
| `main_stock.pl` | Entry point. Runs the chatbot loop and handles user interactions. |
| `input_processing.pl` | Detects the user's intent based on keyword matching. |
| `response_generator.pl` | Generates appropriate educational stock-market responses. |
| `memory_manager.pl` | Optional: remembers user's name and location if enabled. |
| `run_tests.pl` | Optional automated tests (not required). |

## 🚀 Running the Chatbot

### Prerequisites
- SWI-Prolog installed on your system

### Steps

1. **Navigate to the project directory:**
   ```bash
   cd ~/prolog/prolog
   ```

2. **Start SWI-Prolog:**
   ```bash
   swipl
   ```

3. **Load the main file:**
   ```prolog
   ?- [main_stock].
   ```

4. **Start the chatbot:**
   ```prolog
   ?- main_stock:start.
   ```

## 💬 Example Interaction

```
Hi! I'm a modular Stock Market chatbot. Type 'quit' to exit.
Type 'help' to see what I cover.

You: stocks
Bot: A stock represents ownership in a company...

You: etf
Bot: An ETF (Exchange-Traded Fund) is...

You: what is a dividend
Bot: A dividend is a payment made by a corporation...

You: market vs limit
Bot: Market orders execute immediately...

You: quit
Goodbye!
```

## 📚 Supported Topics

The chatbot can explain:

- **Basic Concepts:** stocks, ETFs, mutual funds, indexes
- **Orders:** market vs limit, stop loss
- **Metrics:** market cap, P/E ratio, dividend yield, bid/ask
- **Strategies:** diversification, dollar cost averaging, long vs short
- **Market Behavior:** what moves prices, bullish vs bearish, IPOs
- **Trading:** trading hours, ticker symbols

### Example Keywords

- `stocks`
- `etf`
- `what is a dividend`
- `market vs limit`
- `what moves the price`
- `explain mutual fund`
- `what is an ipo`
- `bullish vs bearish`
- `what is a ticker symbol`
- `what does market cap mean`
- `how do trading hours work`
- `what is p/e ratio`
- `what is dividend yield`
- `what is dollar cost averaging`
- `long vs short`

## 🧩 Module Architecture

```
main_stock.pl
 ├── input_processing.pl        (intent detection)
 ├── response_generator.pl      (answers for each intent)
 └── memory_manager.pl          (optional memory features)
```

### How It Works

1. **User Input** → `main_stock.pl` captures the input
2. **Intent Detection** → `input_processing.pl` identifies user intent
3. **Response Generation** → `response_generator.pl` provides educational answer
4. **Memory** → `memory_manager.pl` can store context (optional)
5. **Output** → Response displayed to user

## 🛑 How to Exit

### Inside the chatbot:
```
quit
```

### Inside Prolog:
```prolog
?- halt.
```

## 🧪 Running Tests (Optional)

If you want to test the intent detection:

```prolog
?- [run_tests].
?- run_tests.
```

## 🛠 Extending the Chatbot

### Add New Intents

1. **In `input_processing.pl`:** Add new keyword patterns
2. **In `response_generator.pl`:** Add corresponding responses

### Add Memory Features

Modify `memory_manager.pl` to store additional user context or preferences.

## 🎓 Educational Focus

This chatbot demonstrates:

✔ **Logic programming** with Prolog predicates and unification  
✔ **Modular design** with separate concern files  
✔ **Pattern matching** for intent detection  
✔ **Rule-based systems** for knowledge representation  
✔ **Declarative programming** paradigm

## ✔ License

This chatbot is intended for personal, educational, portfolio, or academic project use.
