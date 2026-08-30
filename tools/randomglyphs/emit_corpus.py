"""Emit the stroke corpus as an F# source file.

This is the hand-off point between the offline Python tooling and the runtime
F# generator: Python ingests source fonts and writes StrokeCorpus.fs; the F#
generator only ever reads that.  Nothing here runs at runtime, and the F# side
needs no JHF parser, simplification or coordinate mapping.

Re-run only when adding or changing a source font:
    python3 tools/randomglyphs/emit_corpus.py
"""
import os, sys
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, '..', '..'))
sys.path.insert(0, HERE)
from corpus import build, SIMPLEX

OUT = os.path.join(REPO, 'src', 'generator', 'StrokeCorpus.fs')

def fsharp_str(s):
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'

def main():
    inv, combos = build()
    rows = []
    for role in sorted(inv):
        for source, defn, _pts, _seps, _closed in inv[role]:
            rows.append((role, source, defn))
    from collections import Counter
    pattern_counts = sorted(Counter(combos).items())

    L = []
    L.append("/// GENERATED FILE - do not edit by hand.")
    L.append("/// Produced by tools/randomglyphs/emit_corpus.py from the Hershey simplex")
    L.append("/// faces (see tools/randomglyphs/README.md) plus Dactyl's own glyphMap.")
    L.append("/// Hershey font data: Copyright 1967 Dr. A. V. Hershey, James Hurt.")
    L.append("/// See tools/randomglyphs/data/hershey/LICENSE.txt for required acknowledgements.")
    L.append("module StrokeCorpus")
    L.append("")
    L.append("/// One harvested stroke: role, source face, and its glyph-string definition.")
    L.append("/// Roles are geometric and script-independent: stem, bar, arc, diag, bowl, dot.")
    L.append("let strokes: (string * string * string) list =")
    L.append("    [ " + "\n      ".join(
        f"{fsharp_str(r)}, {fsharp_str(s)}, {fsharp_str(d)}" for r, s, d in rows) + " ]")
    L.append("")
    L.append("/// Role patterns observed across source glyphs, e.g. [\"stem\"; \"bar\"], paired")
    L.append("/// with how many source glyphs used that exact pattern. Sampling weighted by")
    L.append("/// this count keeps generated glyphs to combinations real letters actually use,")
    L.append("/// in roughly the proportions real letters actually use them.")
    L.append("let rolePatterns: (string list * int) list =")
    L.append("    [ " + "\n      ".join(
        "[ " + "; ".join(fsharp_str(x) for x in p) + " ], " + str(n) for p, n in pattern_counts) + " ]")
    L.append("")
    open(OUT, 'w').write("\n".join(L))
    print(f"wrote {os.path.relpath(OUT, REPO)}")
    print(f"  {len(rows)} strokes from {len(SIMPLEX)} Hershey simplex faces + dactyl")
    print(f"  {len(pattern_counts)} role patterns over {len(combos)} source glyphs")

if __name__ == '__main__':
    main()
