"""Guard: reject Hershey faces that are not single-stroke centrelines.

Only the *simplex* Hershey faces are true spines.  The duplex, complex and
triplex faces draw each stem as two or more parallel lines, and japanese.jhf
traces brush outlines; ingesting those fills the corpus with half-strokes that
only make sense in pairs.

The reliable discriminator is the stroke count of 'A': a true spine draws it
with three (two diagonals and a crossbar).  A path-length/bounding-box ratio
test was tried first and misclassified in both directions -- a single curved
stroke wanders as much as an outline doubles back -- so do not reinstate it.
"""
import sys, os
from hershey_jhf import parse_jhf

HERE = os.path.dirname(os.path.abspath(__file__))
MAX_A_STROKES = 3

def a_strokes(path):
    g = parse_jhf(path)
    idx = ord('A') - 32
    return len(g[idx][3]) if idx < len(g) else None

def is_centreline(path):
    n = a_strokes(path)
    return n is not None and n <= MAX_A_STROKES, n

def main(paths):
    if not paths:
        d = os.path.join(HERE, 'data', 'hershey')
        paths = [os.path.join(d, f) for f in sorted(os.listdir(d)) if f.endswith('.jhf')]
    bad = 0
    for p in paths:
        ok, n = is_centreline(p)
        print(f"{os.path.basename(p):16s} A={n} strokes  {'centreline' if ok else 'MULTI-LINE / OUTLINE -- do not ingest'}")
        if not ok: bad += 1
    print(f"\n{len(paths)-bad}/{len(paths)} usable as centreline sources")
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
