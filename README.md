# Stock Market Chatbot - Modular Python Project

A modular, educational chatbot that explains stock market concepts using Python namespaces, scope management, and clean module separation. Features intent detection, natural language processing, and persistent memory capabilities.

---

## 🎯 Project Overview

This chatbot demonstrates professional Python software engineering practices including:
- **Modular architecture** with clear separation of concerns
- **Intent-based conversation flow** with 20+ supported topics
- **Memory management** for user context and preferences
- **Namespace and scope** demonstration for educational purposes
- **Comprehensive keyword matching** with longest-first algorithm

### Key Features

✅ Explains 20+ stock market concepts (stocks, ETFs, trading, metrics, strategies)  
✅ Memory system to remember user's name, location, and preferences  
✅ Intelligent intent detection with extensive keyword coverage  
✅ Educational focus with guardrails against financial advice  
✅ Clean modular design following software engineering best practices  

---

## 📁 Project Structure

```
python/
├── main_stock.py                          # Entry point & orchestration
├── input_processing_first_lastname.py     # Intent detection & keyword matching
├── response_generator_stockmarket.py      # Response generation
├── memory_manager.py                      # User context storage
├── run_keyword_tests.py                   # Automated testing suite
├── README.md                              # This file
├── README_MEMORY.md                       # Memory module documentation
└── UML_and_Reflection_Modular_Chatbot.md  # Architecture & design reflection
```

---

## 🚀 Quick Start


### Running the Chatbot

```powershell
python main_stock.py
```

### Sample Interaction

```
Hi! I'm a modular Stock Market chatbot. Type 'quit' to exit.
Type 'help' to see what I cover.

You: My name is Alice
Chatbot: Okay, I'll remember that your name is Alice.

You: what is a stock?
Chatbot: A stock is ownership in a company. Shareholders may benefit from price gains and sometimes dividends.

You: who am i?
Chatbot: You told me your name is Alice.

You: tell me about ETFs
Chatbot: An ETF is a basket of assets you trade like one stock. It offers built-in diversification and usually low fees.

You: help
Chatbot: I can explain core investing concepts: stock, ETF, index/S&P 500, dividends, market vs limit, stop loss, ticker, bid/ask, market cap, volume, trading hours, P/E, dividend yield, DCA, long vs short, IPO, risk & return, diversification. This is educational only—no real-time data or advice.

You: quit
Chatbot: Goodbye! Educational use only—this is not financial advice.
```

---

## 📚 Supported Topics

### Basic Concepts
- **Stocks** - Company ownership and shares
- **ETFs** - Exchange-traded funds
- **Mutual Funds** - Pooled investment vehicles
- **Index** - Market indices like S&P 500
- **Dividends** - Company profit distributions

### Trading Mechanics
- **Market vs Limit Orders** - Order types explained
- **Stop Loss Orders** - Risk management tools
- **Ticker Symbols** - Stock identifiers
- **Bid/Ask Spread** - Price quotes
- **Trading Hours** - Market timing

### Financial Metrics
- **Market Cap** - Company size indicator
- **Volume** - Trading activity
- **P/E Ratio** - Price-to-earnings valuation
- **Dividend Yield** - Income metric

### Investment Strategies
- **Diversification** - Risk spreading
- **DCA** - Dollar-cost averaging
- **Long vs Short** - Position types
- **Risk/Return** - Investment fundamentals

### Market Concepts
- **Bull/Bear Markets** - Market conditions
- **IPO** - Initial public offerings
- **Price Movement** - What affects stock prices

---

## 🧠 Memory System

The chatbot remembers information about you during the conversation:

### Supported Memory Contexts
- **Name**: "My name is [name]"
- **Location**: "I live in [city]"

### Recall Queries
- "Who am I?" or "What is my name?"
- "Where do I live?" or "Where am I from?"

### Memory Features
- Pattern matching with wildcards (e.g., "My name is *")
- Multiple actions per rule (remember + reply)
- Context-based storage (name, location, extensible)
- Session-persistent (memory lasts until program exits)

For detailed memory documentation, see `README_MEMORY.md`.

---

## 🏗️ Architecture

### Module Responsibilities

#### `main_stock.py`
- Entry point and conversation orchestration
- Integrates all modules
- Manages session state (turn counter)
- Demonstrates scope and namespaces
- Handles memory queries and intent routing

#### `input_processing_first_lastname.py`
- Normalizes user input
- Detects intents via keyword matching
- Longest-first matching algorithm
- Returns intent + matched keywords
- Supports 20+ intents

#### `response_generator_stockmarket.py`
- Maps intents to educational responses
- Maintains response templates
- Includes guardrails for safety
- State-aware response generation

#### `memory_manager.py`
- In-memory facts storage
- Rules engine with pattern matching
- Supports wildcards in patterns
- Multiple actions per rule
- Syntax validation and error handling

### Data Flow

```
User Input
    ↓
[Memory Manager] ← Check for memory patterns first
    ↓ (if not handled)
[Input Processing] ← Normalize & detect intent
    ↓
[Main] ← Update state, coordinate modules
    ↓
[Response Generator] ← Generate appropriate response
    ↓
Chatbot Output
```

---

## 🧪 Testing

Run the automated keyword test suite:

```powershell
python run_keyword_tests.py
```

This tests intent detection across all supported keywords and scenarios.

### Manual Testing

Each module can be run independently:

```powershell
# Test input processing
python input_processing_first_lastname.py

# Test response generator
python response_generator_stockmarket.py

# Test memory manager
python memory_manager.py
```

---

## 🔧 Extending the Chatbot

### Adding a New Intent

1. **Add keywords** in `input_processing_first_lastname.py`:
```python
INTENT_KEYWORDS = {
    # ... existing intents ...
    "bonds": ["bond", "bonds", "fixed income", "treasury"],
}
```

2. **Add response** in `response_generator_stockmarket.py`:
```python
if intent == "bonds":
    return "Bonds are debt securities where you loan money to an entity for interest payments."
```

3. **Update help text** in `response_generator_stockmarket.py`:
```python
SUPPORTED_KEYWORDS_EXAMPLES = [
    # ... existing topics ...
    "bonds",
]
```

### Adding a New Memory Context

Edit `memory_manager.py` to add new contexts:

```python
_STORE: Dict[str, Dict[str, Any]] = {
    "name": {},
    "location": {},
    "occupation": {},  # New context
}

DEFAULT_RULES = [
    # ... existing rules ...
    {
        "pattern": "i work as *",
        "actions": [
            {"action": "remember", "context": "occupation", "key": "job"},
            {"action": "reply", "template": "Got it, you work as {occupation.job}."}
        ]
    },
]
```

---

## 📖 Educational Concepts

This project demonstrates:

### Python Module System
- Module imports and namespacing
- Module-level vs. local scope
- Variable shadowing
- Cross-module state passing
- `globals()` dictionary usage

### Software Engineering
- Separation of concerns
- Single responsibility principle
- Clean interfaces between modules
- Explicit dependencies over implicit globals
- Modular, testable architecture

### Data Structures
- Dictionaries for key-value storage
- Lists for keyword collections
- Tuples for multiple return values
- Type hints for clarity

### String Processing
- Text normalization
- Pattern matching with regex
- Wildcard replacement
- Case-insensitive comparison

---

## ⚠️ Important Disclaimers

### Educational Purpose Only
This chatbot is designed for **educational purposes only**. It:
- ❌ Does NOT provide real-time market data
- ❌ Does NOT offer financial advice
- ❌ Does NOT make buy/sell recommendations
- ❌ Is NOT a substitute for professional financial guidance

### Limitations
- No persistent storage (memory clears on exit)
- No real-time market data integration
- No user authentication or multi-user support
- Text-based CLI interface only

---

## 🛠️ Technical Details

### Requirements
- **Python Version**: 3.7+
- **Dependencies**: Standard library only
  - `typing` - Type hints
  - `re` - Regular expressions
  - `sys` - System interface

### Module-Level Constants

Each module defines:
- `MODULE_NAME` - Module identifier
- `SCOPE_DEMO` - Scope demonstration string

### Session State
- Turn counter tracks conversation length
- Memory persists during session
- State passed explicitly via dictionaries

---

## 📝 Documentation

- **`README_MEMORY.md`** - Detailed memory system documentation
- **`UML_and_Reflection_Modular_Chatbot.md`** - Architecture diagrams and design reflection

---

## 🤝 Contributing

To contribute or modify:

1. Follow the existing modular structure
2. Maintain clean separation between modules
3. Add tests for new features
4. Update documentation
5. Keep responses educational and neutral

---

## 📄 License

This is an educational project. Use for learning purposes.

---

## 👤 Author

Created as a demonstration of modular Python programming, namespace management, and clean software architecture.

---

## 🔄 Version History

- **v1.0** - Initial release with 20+ intents, memory system, and modular architecture

---

## 📞 Support

For questions about the code structure or Python concepts demonstrated:
- Review the inline code comments
- Check the UML and reflection document
- Examine individual module implementations

---

**Remember**: This chatbot explains concepts. For actual investment decisions, consult a qualified financial advisor.
