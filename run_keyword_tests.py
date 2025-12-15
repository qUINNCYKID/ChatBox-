"""Run keyword tests for input_processing_first_lastname.INTENT_KEYWORDS.

This script iterates every keyword for each declared intent and checks that
`detect_intent(keyword)` returns the expected intent. It prints a short
report with failures for inspection.
"""
from typing import Dict, List
import input_processing_first_lastname as ip


def run_tests() -> Dict[str, List[str]]:
    failures = {}
    total = 0
    passed = 0

    for expected_intent, keywords in ip.INTENT_KEYWORDS.items():
        for kw in keywords:
            total += 1
            detected_intent, matches = ip.detect_intent(kw)
            if detected_intent == expected_intent:
                passed += 1
            else:
                failures.setdefault(expected_intent, []).append(
                    f"keyword={kw!r} -> detected={detected_intent!r}, matches={matches!r}"
                )

    print(f"Tested {total} keywords: {passed} passed, {total-passed} failed")
    if failures:
        print("\nFailures by expected intent:")
        for intent, items in failures.items():
            print(f"- {intent} ({len(items)} failures):")
            for it in items:
                print("    ", it)
    else:
        print("All keywords matched their expected intents.")

    return failures


if __name__ == "__main__":
    run_tests()
