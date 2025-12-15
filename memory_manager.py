"""memory_manager.py

Simple in-memory facts store with a rules engine supporting wildcard
pattern matching and multiple actions per rule.

Requirements addressed:
- Use a dictionary to store key-value pairs.
- Support at least two context types (name, location).
- Facts persist throughout the session (module-level store).
- Modularized memory functions: remember(), recall(), process_input().
- Pattern matching supports wildcard `*`.
- Rules allow multiple actions per rule.
- Syntax validation and error handling included.
"""
from typing import Dict, Any, List, Optional, Tuple
import re


class MemoryError(Exception):
    pass


class MemorySyntaxError(MemoryError):
    pass


# module-level memory store: context -> key -> value
_STORE: Dict[str, Dict[str, Any]] = {
    "name": {},
    "location": {},
}


def _validate_context(context: str) -> None:
    if not isinstance(context, str) or not context:
        raise MemorySyntaxError("context must be a non-empty string")


def remember(context: str, key: str, value: Any) -> None:
    """Store a (key, value) pair under a context.

    Raises MemorySyntaxError for invalid inputs.
    """
    _validate_context(context)
    if not isinstance(key, str) or not key:
        raise MemorySyntaxError("key must be a non-empty string")

    # Create context dict if unknown (allow extensibility)
    ctx = _STORE.setdefault(context, {})
    ctx[key] = value


def recall(context: str, key: Optional[str] = None, pattern: Optional[str] = None) -> Any:
    """Return stored facts.

    - If `key` is provided, return value or raise KeyError.
    - If `pattern` is provided (wildcard `*` supported), return dict of matches.
    - If neither provided, return the whole context dict.
    """
    _validate_context(context)
    ctx = _STORE.get(context, {})
    if key is not None:
        try:
            return ctx[key]
        except KeyError:
            raise KeyError(f"No key '{key}' in context '{context}'")

    if pattern is not None:
        regex = _wildcard_to_regex(pattern)
        result = {k: v for k, v in ctx.items() if re.fullmatch(regex, k) or (isinstance(v, str) and re.fullmatch(regex, v))}
        return result

    # return a shallow copy to avoid accidental external mutation
    return dict(ctx)


def list_contexts() -> List[str]:
    return list(_STORE.keys())


def clear_context(context: str) -> None:
    _validate_context(context)
    _STORE[context] = {}


def _wildcard_to_regex(pattern: str) -> str:
    """Convert a simple wildcard pattern using `*` to a regex.

    - `*` -> `(.+?)` (non-greedy match of one or more chars)
    - If pattern has no `*`, the regex matches the whole string exactly.
    - Validates pattern is a non-empty string.
    """
    if not isinstance(pattern, str) or not pattern:
        raise MemorySyntaxError("pattern must be a non-empty string")

    # Escape regex metacharacters then replace escaped \* with .+?
    esc = re.escape(pattern)
    regex = esc.replace(r"\*", r"(.+?)")
    # Ensure full-match semantics
    return r"^" + regex + r"$"


# --- Rules engine ---------------------------------------------------------
# A rule is a dict with keys:
#  - pattern: str (may contain `*` wildcards)
#  - actions: list of action dicts
# Actions supported:
#  - {"type": "remember", "context": str, "key_template": str, "value_template": str}
#  - {"type": "reply", "template": str}


def _apply_template(template: str, groups: Tuple[str, ...]) -> str:
    """Replace placeholders {0}, {1}, ... in template with regex groups."""
    try:
        return template.format(*groups)
    except Exception as e:
        raise MemorySyntaxError(f"Template formatting error: {e}")


def _match_pattern(pattern: str, text: str) -> Optional[Tuple[str, Tuple[str, ...]]]:
    """Return (pattern, groups) if text matches pattern else None."""
    regex = _wildcard_to_regex(pattern)
    m = re.fullmatch(regex, text)
    if not m:
        return None
    # groups from wildcard captures (may be empty tuple)
    return pattern, m.groups()


# Default rules: at least two context types (name, location). Multiple actions per rule.
DEFAULT_RULES = [
    {
        "pattern": "my name is *",
        "actions": [
            {"type": "remember", "context": "name", "key_template": "user", "value_template": "{0}"},
            {"type": "reply", "template": "Nice to meet you, {0}! I'll remember your name."},
        ],
    },
    {
        "pattern": "i am *",
        "actions": [
            {"type": "remember", "context": "name", "key_template": "user", "value_template": "{0}"},
            {"type": "reply", "template": "Got it — I'll call you {0}."},
        ],
    },
    {
        "pattern": "i live in *",
        "actions": [
            {"type": "remember", "context": "location", "key_template": "home", "value_template": "{0}"},
            {"type": "reply", "template": "I'll remember you live in {0}."},
        ],
    },
    {
        "pattern": "remember * as *",
        "actions": [
            {"type": "remember_explicit", "template": "{0}:{1}"},
            {"type": "reply", "template": "Okay, remembered {0} as {1}."},
        ],
    },
]


def process_rules(text: str, rules: Optional[List[Dict]] = None) -> List[str]:
    """Process rules against `text`. Returns a list of reply strings (may be empty).

    This function supports multiple actions per matching rule and uses
    templates with captured wildcard groups.
    """
    if rules is None:
        rules = DEFAULT_RULES

    replies: List[str] = []
    txt = " ".join(text.lower().strip().split())

    for rule in rules:
        patt = rule.get("pattern")
        if not patt or not isinstance(patt, str):
            raise MemorySyntaxError("Each rule must have a string 'pattern'")

        matched = _match_pattern(patt, txt)
        if not matched:
            continue

        _, groups = matched
        actions = rule.get("actions", [])
        for action in actions:
            atype = action.get("type")
            if atype == "remember":
                ctx = action.get("context")
                key_t = action.get("key_template")
                val_t = action.get("value_template")
                if not ctx or not key_t or not val_t:
                    raise MemorySyntaxError("remember action requires 'context','key_template','value_template'")
                key = _apply_template(key_t, groups)
                val = _apply_template(val_t, groups)
                remember(ctx, key, val)
            elif atype == "remember_explicit":
                # template should be like "context:key" after formatting
                templ = action.get("template")
                if not templ:
                    raise MemorySyntaxError("remember_explicit requires 'template'")
                text_pair = _apply_template(templ, groups)
                if ":" not in text_pair:
                    raise MemorySyntaxError("remember_explicit template must produce 'context:key'")
                ctx, kv = text_pair.split(":", 1)
                # treat kv as key=value or just key; store value as empty
                if "=" in kv:
                    k, v = kv.split("=", 1)
                else:
                    # default key 'value'
                    k, v = kv, ""
                remember(ctx.strip(), k.strip(), v.strip())
            elif atype == "reply":
                templ = action.get("template")
                if not templ:
                    raise MemorySyntaxError("reply action requires 'template'")
                replies.append(_apply_template(templ, groups))
            else:
                raise MemorySyntaxError(f"Unsupported action type: {atype}")

    return replies


def process_input(text: str) -> Tuple[bool, List[str]]:
    """Examine `text`, run rules, and return (handled_flag, replies).

    If handled_flag==True, callers should not further process text as a normal intent.
    """
    try:
        replies = process_rules(text)
        handled = len(replies) > 0
        return handled, replies
    except MemorySyntaxError:
        # Re-raise for the caller to handle or return a safe error reply
        raise



if __name__ == "__main__":
    # Quick manual demo
    print("Memory manager demo")
    handled, replies = process_input("My name is Alice")
    print(handled, replies)
    print("Recall name:", recall("name"))
