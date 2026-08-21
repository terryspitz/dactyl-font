# Random glyphs — corpus ingestion

Offline tooling that harvests stroke skeletons from public-domain
[Hershey vector fonts](https://en.wikipedia.org/wiki/Hershey_fonts) and writes
them out as [`src/generator/StrokeCorpus.fs`](../../src/generator/StrokeCorpus.fs).

See [`docs/RandomGlyphs.md`](../../docs/RandomGlyphs.md) for the design this
serves.

## What this is not

**Nothing here generates glyphs.** Glyph generation happens on demand in F#, at
runtime, when the user asks for it — sampling fresh combinations out of the
corpus on every click.

This tooling only produces the *raw material* it samples from: a table of stroke
definitions harvested from real letterforms, no different in kind from the
hand-written `glyphMap` in `GlyphStringDefs.fs`. It just happens to be
transcribed from Hershey rather than typed by hand.

The split is by **when code runs**, not by language:

| | Runs | Where | Job |
|---|---|---|---|
| This tooling | Offline, rarely — only when a source font is added | Python | Turn `.jhf` polylines into glyph strings |
| `StrokeCorpus.fs` | Build time | Generated F# | The hand-off: 922 strokes + 70 role patterns |
| Generator | On demand, per click | F# → Fable → browser | Propose → Filter → Assemble a new glyph |

So the F# side never needs a JHF parser, curve simplification or coordinate
mapping, and no logic exists in two places to drift apart.

## Regenerating the corpus

Only needed when adding or changing a source font:

```
python3 tools/randomglyphs/emit_corpus.py
```

No dependencies beyond the Python standard library. Commit the regenerated
`StrokeCorpus.fs`; `StrokeCorpusTests` will check every stroke still parses.

## Adding a source font

**Check it is a centreline font first.**

```
python3 tools/randomglyphs/verify_centreline.py path/to/font.jhf
```

Only the Hershey *simplex* faces are true single-stroke spines. The duplex,
complex and triplex faces draw every stem as two or more parallel lines, and
`japanese.jhf` traces brush **outlines**. Ingesting one of those fills the
corpus with half-strokes that only make sense in pairs — and it fails silently,
because the strings still parse.

The discriminator is the stroke count of `A`: **3** for a true spine (two
diagonals and a crossbar) against 6 for duplex, 9 for `gothgrt`, 12 for
`rowmant` and 7 for `japanese`.

> A path-length / bounding-box-diagonal ratio test was tried first and is **not**
> reliable — it called the verified-clean `futural` "mixed" and the outline-based
> `japanese` "centreline", because a single curved stroke wanders as much as an
> outline doubles back. Don't reinstate it.

Once it passes, add the name to `SIMPLEX` in `corpus.py` and re-run
`emit_corpus.py`.

## Files

| File | Purpose |
|---|---|
| `hershey_jhf.py` | Parser for the `.jhf` format |
| `glyph_compile.py` | Inverse compiler: polylines → glyph strings (mirror of `parse_curve`) |
| `corpus.py` | Combines Hershey simplex faces with Dactyl's own `glyphMap`; classifies stroke roles |
| `emit_corpus.py` | Writes `StrokeCorpus.fs` |
| `verify_centreline.py` | The centreline guard described above |
| `data/hershey/` | Source font data + licence |

## Two traps in the inverse compiler

Both produce strings that parse cleanly and only look wrong once rendered, so
they are invisible to unit tests:

- **Corners must be detected on the original polyline, not the simplified one.**
  A coarsely sampled arc has large turn angles between its chords, so detecting
  corners after Ramer–Douglas–Peucker sprays spurious `K` kinks around every
  bowl (`C`, `D`, `G`, `O`).
- **A closed contour repeats its start point at the end.** Keeping both leaves a
  zero-length segment that the solver renders as a lump — `O` and `Q` came out
  as dented eggs until the duplicate was dropped and the trailing separator left
  to close the path.

Render the output and look at it. Neither of these shows up any other way.

## Licence

The Hershey font data in `data/hershey/` is used under its own licence (see
`data/hershey/LICENSE.txt`), which permits any use, commercial or otherwise,
provided these acknowledgements travel with the data:

- The Hershey Fonts were originally created by Dr. A. V. Hershey while working
  at the U. S. National Bureau of Standards.
- The format of the font data was originally created by James Hurt, Cognition,
  Inc., 900 Technology Park Drive, Billerica, MA 01821.

Copyright 1967 Dr. A. V. Hershey, James Hurt.

The data may be converted into any format *except* the one distributed by the
U.S. NTIS. The `.jhf` files here are the data only; the GPL in the upstream
package covers its C tooling, none of which is used.
