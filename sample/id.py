import sys
import syntax

# REPL that does nothing
for line in syntax.read_lines("> "):
    try:
        t = syntax.parse_term(line)
        print(syntax.format_term(t))
    except syntax.ParseError as e:
        print("error: {}".format(e))
