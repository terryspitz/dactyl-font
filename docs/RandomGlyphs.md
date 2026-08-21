# Random Glyphs: generating a fake alphabet

A design for generating plausible, novel letterforms — a "fake alphabet" — by
learning from the existing glyph definition strings in
[`GlyphStringDefs.fs`](../src/generator/GlyphStringDefs.fs), with a path to
training on other scripts and typefaces.

---

## 1. Is there enough data?

Short answer: **no — not for a neural model, and not even close.** But the
question turns out to be the wrong one, and the right one has a good answer.

### The corpus, measured

| Measure | Value |
|---|---|
| Glyphs in `glyphMap` | 103 |
| Total definition text | 2,082 characters (~2 KB) |
| Strokes (space-separated runs) | 159 |
| Point instances | 504 |
| Distinct point tokens | 184 |
| Tokens appearing exactly **once** | 118 (64%) |

Two kilobytes. A character-level RNN wants ~1 MB before it stops merely
memorising; this is roughly 500× short. And 64% of the vocabulary is
*hapax* — seen once, ever — so most tokens have no usable statistics at all.

### Confirmed by experiment

A bigram model over point tokens (with unigram backoff) trained on the corpus
produces:

- **51%** of samples syntactically valid against `glyph_re`
- output dominated by one- and two-point fragments: `tel-`, `hlo~`, `hcW~`

These are not letters. Pure sequence statistics over this corpus fail, as the
size predicts.

### Why the question is the wrong one

A neural font model (the [SVG-VAE](https://magenta.tensorflow.org/svg-vae) idea
already noted in [TODO.md](TODO.md)) spends most of its capacity learning
**style**: weight, contrast, stroke terminals, serifs, slant, roundedness,
optical corrections. Dactyl already has all of that, as ~40 explicit axes in
[`Axes.fs`](../src/generator/Axes.fs).

So the model does not need to learn style. It only needs to learn **skeleton
topology** — which strokes a letter is made of and where they meet. Everything
else is a slider.

Topology is low-dimensional and combinatorial, and that changes the data
requirement completely. Decoding the corpus geometrically gives:

| Stroke role | Count |
|---|---|
| arc | 62 |
| diag | 31 |
| stem | 26 |
| bar | 20 |
| dot | 12 |
| bowl (closed) | 8 |

159 strokes in 6 script-independent roles, combined in **25 distinct role
patterns** across 102 glyphs. That is enough to **recombine**. It is not enough
to **extrapolate** — which is exactly why the design below is a recombination
sampler now, with a learned model deferred until §4 supplies real data.

---

## 2. Design: Propose → Filter → Assemble

### Stage 1 — Propose

1. **Decode** each corpus stroke into geometry: resolve every coordinate
   expression to an `(x, y)` through the same guide values `FontMetrics` uses
   (`L/C/R/W`, `B/X/H/T/D`), recording open/closed and the separator run.
2. **Classify** each stroke into a role by pure geometry — closed → `bowl`,
   single point → `dot`, tall and narrow → `stem`, wide and flat → `bar`,
   two points → `diag`, else `arc`. Deliberately script-independent, so the
   same classifier works on the imported corpora in §4.
3. **Learn** three small distributions: `P(role pattern)` (25 observed),
   `P(stroke | role)`, and `P(transform)`.
4. **Sample** a role pattern, draw a stroke per role, apply a
   lattice-preserving transform (identity, mirror in x, flip in y, shift up or
   down), and re-encode.

**The coordinate codebook** is what makes re-encoding safe. Collect every
coordinate expression appearing in the corpus together with its numeric value
(`bbt` → 200, `r4c` → 270, `h9b` → 30, …). To place a point at value *v*, pick
the nearest codebook entry. This guarantees syntactic validity by construction,
and keeps generated glyphs written in the corpus's own idiom rather than in
arbitrary new spellings.

**Measured results** for this stage:

- **86%** of samples valid against `glyph_re` (vs 51% for the bigram)
- **2,222 unique novel** glyphs from 4,000 samples
- **500 / 500** parse through the real F# `rawDefToElem` — zero exceptions,
  zero silent collapses to `Dot`

That last check matters: `rawDefToElem` catches every exception and returns a
`Dot`, so a bad string fails *silently*. Any implementation must treat "came
back as a `Dot`" as a rejection, not a success.

### Stage 2 — Filter

Rendering 40 samples through the real outline pipeline shows the proposer is
sound but that roughly a third of output needs rejecting, in specific and
recognisable ways. Each filter below targets a failure actually observed:

| Filter | Rejects |
|---|---|
| **Connectivity** | Strokes floating apart with no junction — a bar sitting beside an unrelated stem. Require every stroke to touch another within a tolerance, or be a legal dot/diacritic. |
| **Spike / sliver** | Long thin degenerate wedges from near-collinear points. Enforce a minimum turn angle and minimum segment length. This is the artifact [CLAUDE.md](../CLAUDE.md) warns about — it is invisible in unit tests and obvious on sight. |
| **Bounding box** | Glyphs drifting outside the advance width and clipping. |
| **Complexity floor** | Trivial output — a lone bar (`hr-hl`) is not a letter. Require a minimum ink length and point count. |
| **Self-intersection** | Strokes crossing their own body into visual mush. |

Filtering is where quality comes from. The proposer should be run generously
and rejected from hard, rather than tuned to be conservative.

### Stage 3 — Assemble an alphabet

An alphabet is not a bag of independent glyphs. Sampling 26 glyphs
independently produces 26 unrelated doodles. Three constraints make the set
read as a writing system:

1. **One style for the whole set.** Sample the Dactyl axes *once* per alphabet
   and apply to every glyph. Consistency of weight, width and roundedness comes
   free — this is the payoff of §1's observation.
2. **A shared part inventory.** Sample a restricted stroke pool once per
   alphabet, then build every glyph from that pool. Real scripts reuse a small
   set of parts; this is what produces family resemblance.
3. **Mutual distinctiveness.** Select greedily, rejecting any candidate too
   similar to one already accepted (compare on the decoded skeleton, not the
   string). Letters must be telling apart.

### Where it lives

In F#, beside `GlyphStringDefs.fs`, so it is shared by the web app via Fable
and by `generateFonts`. Stage 2 needs the parser and outline geometry anyway.
No new dependencies. The natural UI is a "Random" button on the **Glyphs** tab,
which already renders multi-line `char: def` text through
`generateSplineDebugSvgFromDefs` — so the output target exists today and needs
no new rendering work.

---

## 3. Model options, ranked for this data regime

| Model | Verdict |
|---|---|
| n-gram / Markov over tokens | Tested: 51% valid, fragment output. Not viable alone. |
| **Role-based recombination (§2)** | **Tested: 86% valid, letter-like. Build this.** |
| PCFG over the DSL grammar | Good complement — learn production probabilities, valid by construction. Viable at current size. |
| Sequence model / VAE over strokes | Needs ~10k+ glyphs. Unlocked by §4, not before. |
| Char-level LM on glyph strings | Needs far more data still; only sensible fine-tuned on an imported corpus. |

The ladder is real: each rung needs roughly 10× the data of the one below, and
§4 is how you climb it.

---

## 4. Training on other scripts and typefaces

This is the most valuable part of the idea, and it hinges on one distinction.

> **Dactyl glyph strings are stroke *spines*. TTF/OTF fonts store filled
> *outlines*.**

A font file does not contain the centreline of the `o`; it contains the two
sides of it. Recovering a spine from an outline is medial-axis extraction:
genuinely hard, and lossy precisely where letterforms are most interesting
(high-contrast strokes, serifs, junctions). Building the whole plan on "read
some TTFs" would sink it.

So rank sources by how close they already are to spines.

### Tier 1 — already centrelines, no skeletonisation

- **[Hershey fonts](https://en.wikipedia.org/wiki/Hershey_fonts)** — public
  domain, ~2,000+ glyphs defined as polyline strokes on a coordinate grid,
  covering Latin, **Greek, Cyrillic, Japanese kana**, and symbols. These are
  almost exactly Dactyl strings in a different notation; the format is a near
  perfect match and the licence is unencumbered. Best possible starting point.
- **[KanjiVG](https://kanjivg.tagaini.net/)** (CC BY-SA 3.0) — ~11,000 kanji as
  SVG **stroke centrelines**, each stroke tagged with type and stroke order.
  The largest centreline corpus available anywhere, and stroke-typed, which
  feeds the role classifier directly.

Tier 1 alone moves the corpus from 159 strokes to on the order of 100,000,
across four-plus scripts — the point at which the learned models in §3 become
trainable, and the natural home for the SVG-VAE idea in TODO.md.

### Tier 2 — monoline outline fonts

A constant-width sans has a clean, well-behaved medial axis, so skeletonisation
is tractable. Google Fonts supplies plenty. This is the route to *typeface*
variety (as opposed to *script* variety) and should follow Tier 1.

### Tier 3 — general outline fonts

Serif and high-contrast faces need real skeletonisation with branch pruning.
Treat as a stretch goal; do not gate anything on it.

### The inverse compiler

Ingestion is the mirror of `parse_curve` — geometry in, glyph string out:

```
stroke centreline
  → normalise into Dactyl's guide box
  → simplify (Ramer–Douglas–Peucker)
  → per segment: straight or curved?        → '-' or '~'
  → turn angle above threshold?             → 'K'
  → axis extreme?                           → fitted '(c)'
  → snap coordinates via the codebook (§2)
  → emit string
  → ROUND-TRIP VERIFY
```

**Round-trip verification is the quality gate, and it is cheap:** parse the
emitted string with the existing parser and compare the resulting points
against the source geometry. Reject if the error exceeds a threshold. Ingestion
validates itself, so a bad importer cannot quietly poison the corpus. `opentype.js`
is already a dependency and can read outlines when Tier 2 arrives.

### One caveat

The guide letters `t/x/h/b/d` are Latin-specific (cap height, x-height,
baseline, descender). For CJK the em square maps cleanly onto `t`…`b` and
`l`…`w`, with `x` and `h` as interior guides — fine as a normalised lattice,
but the semantics are no longer typographic. Keep the source script as a
conditioning label so a later model can generate "fake Greek" or "fake kana"
deliberately, rather than an undifferentiated average of all scripts.

---

## 5. Suggested staging

| Phase | Work | Unlocks |
|---|---|---|
| 1 | Recombination sampler + filters, F#, Glyphs-tab button | Usable fake alphabets now, no new deps |
| 2 | Hershey ingestion + round-trip verification | Validates the inverse compiler; adds Greek/Cyrillic/kana |
| 3 | KanjiVG ingestion | ~11k glyphs; corpus large enough to train on |
| 4 | Learned model (PCFG → sequence model / VAE), script-conditioned | Genuine novelty rather than recombination |

Phase 1 stands alone and delivers the feature. Phases 2–4 are the path from
"plausible recombinations of Latin parts" to a model that has actually seen how
the world's writing systems are built.
