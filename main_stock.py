
# main_first_lastname.py
"""
Entry point for the modular chatbot.
- Demonstrates imports, namespaces, and variable scope
- Wires input processing to response generation
"""
import sys
from typing import Dict

import input_processing_first_lastname as ip
import response_generator_stockmarket as rg
import memory_manager as mm
import re

MODULE_NAME = "main_first_lastname"
SCOPE_DEMO = "SCOPE_DEMO set in main module"

# Module-level state (visible across functions in this module only unless passed along)
TURN_COUNT = 0

def stock_chatbot() -> None:
    # Local variable shadows module-level name (scope demonstration)
    SCOPE_DEMO = "SCOPE_DEMO defined locally inside stock_chatbot()"

    print("Hi! I'm a modular Stock Market chatbot. Type 'quit' to exit.")
    print("Type 'help' to see what I cover.\n")

    # Show scope/namespace differences explicitly
    print(f"[scope] main.SCOPE_DEMO: {globals()['SCOPE_DEMO']}")
    print(f"[scope] main.stock_chatbot local SCOPE_DEMO: {SCOPE_DEMO}")
    print(f"[scope] ip.SCOPE_DEMO: {ip.get_scope_demo()}")
    print(f"[scope] rg.SCOPE_DEMO: {rg.SCOPE_DEMO}\n")

    while True:
        try:
            user = input("You: ")
        except EOFError:
            print()  # clean newline
            user = "quit"

        # First, allow the memory manager to capture statements like
        # "My name is Alice" or "I live in Paris" via pattern rules.
        try:
            handled, replies = mm.process_input(user)
        except Exception as e:
            print("Chatbot: Sorry, I couldn't process that memory request:", str(e))
            handled = False
            replies = []

        if handled:
            for r in replies:
                print("Chatbot:", r)
            # memory handled the input; skip normal intent processing
            continue

        # Simple recall queries (explicit checks). Keep these lightweight.
        q = " ".join(user.lower().strip().split())
        if re.search(r"\b(who am i|what is my name)\b", q):
            try:
                name = mm.recall("name", "user")
                print("Chatbot:", f"You told me your name is {name}.")
            except KeyError:
                print("Chatbot: I don't know your name yet. Tell me by saying 'My name is ...'")
            continue
        if re.search(r"\b(where do i live|where am i from|where do i live\?)\b", q):
            try:
                loc = mm.recall("location", "home")
                print("Chatbot:", f"You told me you live in {loc}.")
            except KeyError:
                print("Chatbot: I don't know where you live yet. Tell me by saying 'I live in ...'")
            continue

        intent, matches = ip.detect_intent(user)

        # Update module-level state
        global TURN_COUNT
        TURN_COUNT += 1

        # State we choose to pass across modules (explicit!)
        state: Dict = {"turn_count": TURN_COUNT}

        reply = rg.generate_response(intent, user, matches, state)
        print("Chatbot:", reply)

        if intent == "quit":
            break

if __name__ == "__main__":
    stock_chatbot()
