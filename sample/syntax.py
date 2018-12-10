import sys
import os
import collections
try:
    import readline
except ImportError:
    pass

class ParseError(Exception):
    pass

### Constants

special_toks = ["(", ")", "λ", "\\", ".", "=", ":", "->"]

reserved_words = ["0", "succ", "pred", "iszero",
                  "true", "false", "if", "then", "else",
                  "let", "in",
                  "fix"]

### Lexer

def lexer(s):
    i = j = 0
    tokens = []
    def flush():
        nonlocal i
        if i < j:
            tokens.append("".join(s[i:j]))
            i = j
    while j < len(s):
        if s[j].isspace():
            flush()
            i = j = j+1
        else:
            for tok in special_toks:
                if s[j:j+len(tok)] == tok:
                    flush()
                    tokens.append(tok)
                    i = j = j+len(tok)
                    break
            else:
                j += 1
    flush()
    return tokens

### Parser

def expect(what, w):
    if len(w) == 0 or w[0] != what:
        raise ParseError("expected '{}'".format(what))
    w.popleft()
    
def parse_term(s):
    w = collections.deque(lexer(s))
    t = parse_abs(w)
    if len(w) != 0:
        raise ParseError("unexpected '{}' after term".format(w[0]))
    return t
    
def parse_abs(w):
    if len(w) == 0:
        raise ParseError("unexpected end of strigg")
    elif w[0] in ["\\", "λ"]:
        w.popleft()
        x = parse_var(w)
        if len(w) == 0:
            raise ParseError("unexpected end of string")
        elif w[0] == ":":
            expect(":", w)
            tau = parse_arrow(w)
            expect(".", w)
            t = parse_abs(w)
            return ["typedlambda", x, tau, t]
        elif w[0] == ".":
            expect(".", w)
            t = parse_abs(w)
            return ["lambda", x, t]
        else:
            raise ParseError("expected . or :")
    elif w[0] == "let":
        expect("let", w)
        x = parse_var(w)
        expect("=", w)
        t1 = parse_abs(w)
        expect("in", w)
        t2 = parse_abs(w)
        return ["let", x, t1, t2]
    elif w[0] == "if":
        expect("if", w)
        t1 = parse_abs(w)
        expect("then", w)
        t2 = parse_abs(w)
        expect("else", w)
        t3 = parse_abs(w)
        return ["if", t1, t2, t3]
    else:
        return parse_app(w)

def parse_app(w):
    if w[0] in ["succ", "pred", "iszero", "fix"]:
        op = w.popleft()
        t = [op, parse_atom(w)]
    else:
        t = parse_atom(w)
    while len(w) > 0 and w[0] not in [")", "then", "else", "in"]:
        t = ["app", t, parse_atom(w)]
    return t

def parse_atom(w):
    if len(w) == 0:
        raise ParseError("unexpected end of string")
    elif w[0] == "(":
        expect("(", w)
        t = parse_abs(w)
        expect(")", w)
        return t
    elif w[0] == "0":
        expect("0", w)
        return "zero"
    elif w[0] in ["true", "false"]:
        return w.popleft()
    else:
        return ["var", parse_var(w)]

def parse_var(w):
    if len(w) == 0:
        raise ParseError("unexpected end of string")
    elif w[0] in special_toks or w[0] in reserved_words:
        raise ParseError("unexpected '{}'".format(w[0]))
    else:
        return w.popleft()

def parse_arrow(w):
    tau = parse_base(w)
    if len(w) > 0 and w[0] == "->":
        expect("->", w)
        tau2 = parse_arrow(w)
        tau = ["arrow", tau, tau2]
    return tau

def parse_base(w):
    if len(w) == 0:
        raise ParseError("unexpected end of string")
    elif w[0] in ["Nat", "Bool"]:
        tau = w[0]
        w.popleft()
        return tau
    elif w[0] == "(":
        expect("(", w)
        tau = parse_arrow(w)
        expect(")", w)
        return tau
    else:
        raise ParseError("unexpected '{}'".format(w[0]))

def format_abs(t):
    if isinstance(t, list):
        if t[0] == "if":
            return "if {} then {} else {}".format(format_abs(t[1]), format_abs(t[2]), format_abs(t[3]))
        elif t[0] == "let":
            return "let {} = {} in {}".format(t[1], format_abs(t[2]), format_abs(t[3]))
        elif t[0] == "lambda":
            return "λ{}. {}".format(t[1], format_abs(t[2]))
        elif t[0] == "typedlambda":
            return "λ{}:{}. {}".format(t[1], format_type(t[2]), format_abs(t[3]))
    return format_app(t)

def format_app(t):
    if isinstance(t, list):
        if t[0] in ["succ", "pred", "iszero", "fix"]:
            return "{} {}".format(t[0], format_atom(t[1]))
        elif t[0] == "app":
            return "{} {}".format(format_app(t[1]), format_atom(t[2]))
    return format_atom(t)

def format_atom(t):
    if isinstance(t, list):
        if t[0] == "var":
            return t[1]
        else:
            return "({})".format(format_abs(t))
    elif t == "zero":
        return "0"
    elif t in ["true", "false"]:
        return t

format_term = format_abs

def format_arrow(tau):
    if isinstance(tau, list) and tau[0] == "arrow":
        return "{}->{}".format(format_base(tau[1]), format_arrow(tau[2]))
    return format_base(tau)

def format_base(tau):
    if isinstance(tau, list):
        if tau[0] == "typevar":
            return tau[1]
        else:
            return "({})".format(format_arrow(tau))
    else:
        return tau

format_type = format_arrow

def read_lines(prompt=""):
    """Read lines from stdin. If the file is a tty, that is, keyboard input
    from the user, then display a prompt and allow editing and history."""
    if os.isatty(sys.stdin.fileno()):
        while True:
            try:
                line = input(prompt)
            except EOFError:
                print()
                break
            yield line
    else:
        for line in sys.stdin:
            yield line
