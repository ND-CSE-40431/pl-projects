#!/usr/bin/env python3

from __future__ import print_function
import sys
if sys.version_info >= (3, 0, 0):
    from itertools import zip_longest
else:
    from itertools import izip_longest as zip_longest

if len(sys.argv) != 4:
    sys.exit("usage: diff.py <input> <output> <submitted>")

any_failed = False
for i, (iline, oline, sline) in enumerate(zip_longest(*map(open, sys.argv[1:]))):
    iline = iline.strip()

    if oline is None:
        print("warning: your file is too long (ignoring)")
        break
    else:
        oline = oline.strip()

    if sline is None:
        sline = ""
    else:
        sline = sline.strip()

    if oline.startswith("error:") and sline.startswith("error:"):
        print("{} => error: PASSED".format(iline))
    elif oline == sline:
        print("{} => {}: PASSED".format(iline, oline))
    else:
        print("{}: FAILED".format(iline))
        print("    correct: {}".format(repr(oline)))
        print("    yours:   {}".format(repr(sline)))
        any_failed = True

sys.exit(1 if any_failed else 0)
