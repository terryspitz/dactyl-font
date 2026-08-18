# Generalised stroke rendering — proposal

*Status: proposal, nothing implemented yet.*

`stroked` and `scratches` are two hardcoded points in a space the renderer could
expose continuously.  This document works out what that space is, proposes the
axes and UI to expose it, and shows how five of the unimplemented "artistic
axes" in [TODO.md](TODO.md) fall out of the same change.

---

## 1. Where we are today

There are two unrelated stroke renderers.

### A. The ribbon renderer — `Font.getDactylConstantOffsetOutlines` (`Font.fs:1284`)

The modern path.  It solves the spine, samples it by arc length, and emits an
outline point per sample per side:

```
E(s, σ) = P(s) + D(s)·n̂(s)  +  σ · W(s, θ(s)) · n̂(s)
                └ displacement ┘  └──── half-width ────┘
```

where `s` is arc length, `θ(s)` the spine tangent, `n̂` the unit normal and
`σ = ±1` the side.  **Every artistic axis is already a term in that one
formula:**

| axis | role | where |
|---|---|---|
| `weight` | base of `W` | `Font.fs:1285` |
| `contrast` | scales `W` by direction — but folded into `addPolarContrast` | `Font.fs:86` |
| `nib`, `nib_angle` | `W ×= nibFactor θ` | `Font.fs:1476` |
| `taper`, `taper_end` | `W ×=` ramp in `s` | `Font.fs:1488` |
| `roughness` | `W +=` per-side noise(`s`) | `Font.fs:1503` |
| `wobble` | `D(s)` = sine of `s` | `Font.fs:1387` |
| `mobius` | separate panel builder, `W ×= |cos φ(s)|` | `Font.fs:1699` |
| `joint_gap` | trims the `s` domain at joint ends | `Font.fs:1452` |

They compose cleanly with each other because they are all multiplicative
factors on `W` or additive terms on `D`.

### B. The multi-line renderer — `stroked` and `scratches` (`Font.fs:1025`, `1029`)

Both go through `spiroToLines n` (`Font.fs:994`), which builds `n` copies of the
spine offset perpendicular by evenly spaced amounts across `±thickness`:

```fsharp
let offset = thickness * (float i / float (lines - 1) - 0.5) * 2.0
```

- **`stroked`** — 4 lines at −T, −T/3, +T/3, +T, each outlined at `weight = 2`
  (hairline rails).
- **`scratches`** — 3 lines at −T, 0, +T, each stroked at T/3 with deliberately
  ragged caps (start cap kicks backwards at 0.9π, end cap is a single point).

## 2. Why B is a dead end as written

1. **They are booleans, not axes.**  The Tweens tab renders a row for every
   control, so `tween-stroked` and `tween-scratches` are just on/off pairs;
   randomisation can only flip them; there is no continuum from a solid stroke
   to four rails.
2. **They are locked to the legacy engine.**  `getOutline` (`Font.fs:1909`)
   tests `stroked`/`scratches` *before* it looks at `dactyl_spline`, so they
   always render through Spiro.  They compose with **none** of nib, taper,
   wobble, roughness, mobius or joint_gap.
3. **Everything is a literal.**  Line counts, spacings and sub-weights are
   hardcoded, and `FontMetrics.thickness` (`GeneratorTypes.fs:73`) has to
   special-case them with `max weight 30.0` to stop the effect collapsing at low
   weights.
4. **They are the same feature.**  "4 lines, hairline" and "3 lines, fat and
   ragged" differ only in count, spacing, sub-weight and cap style.

---

## 3. The generalisation

Two layers, independently useful.  Layer 1 is the direct answer to
"stroked and scratches hint at a more general function"; layer 2 is the
refactor that makes layer 1 — and the TODO axes — cheap.

### 3.1 Layer 1: *traces* — parallel ribbons

Replace the two booleans with four axes that describe **N parallel ribbons per
spine**:

| axis | control | default | meaning |
|---|---|---|---|
| `traces` | `Range(1, 6)` | `1` | number of parallel strokes drawn per spine. 1 = today's solid stroke |
| `trace_spread` | `FracRange(0.0, 3.0)` | `2.0` | lateral span from first trace to last, in stroke thicknesses. 2.0 puts the outer traces exactly where a solid stroke's edges would be |
| `trace_weight` | `FracRange(0.02, 1.0)` | `0.1` | each trace's width as a fraction of `weight` |
| `trace_jitter` | `FracRange(0.0, 1.0)` | `0.0` | random per-trace lateral offset and end-length variation — the sketchy multi-pass look |

`traces = 1` is a bit-exact no-op, so the default costs nothing.

The trace centres use the same formula `spiroToLines` uses today, with
`trace_spread` replacing the hardcoded `2.0`:

```fsharp
let traceCentre i =
    if traces <= 1 then 0.0
    else fthickness * trace_spread * (float i / float (traces - 1) - 0.5)
```

**Existing looks become points in that space, and new ones come free:**

| look | `traces` | `trace_spread` | `trace_weight` | `trace_jitter` | also |
|---|---|---|---|---|---|
| Solid (default) | 1 | – | – | – | |
| `stroked` (today) | 4 | 2.0 | 0.07 | 0 | |
| `scratches` (today) | 3 | 2.0 | 0.33 | 0.5 | `roughness 0.3` |
| **Inline** (rail letters) | 2 | 2.0 | 0.35 | 0 | |
| **Split nib** *(TODO)* | 2 | 0.45 | 0.12 | 0 | `nib 0.8` |
| **Backscratch** | 4 | 2.4 | 0.05 | 0.15 | |

`FontMetrics.thickness`'s `max weight 30.0` clamp can be deleted: `trace_spread`
is measured in thicknesses and `trace_weight` is a fraction of `weight`, so the
whole construction scales with the pen instead of needing a floor.

#### Implementation

Smaller than it looks, because `buildSide` (`Font.fs:1521`) already emits an
offset polyline — it just hardcodes the offset to `sign * w`.  Generalise it to
a *signed* offset around a trace centre:

```fsharp
// today                                    // proposed
let buildSide (sign: float) includeEnds     let buildSide (centre: float) (sign: float) includeEnds
    let w = widthAt sLen th |> roughen ...      let o = centre + sign * (widthAt sLen th |> roughen ...)
    addPolarContrast x y (th + perpAngle) w     addPolarContrast x y (th + PI/2.) o
```

with three consequential changes:

- The convex/concave miter decision at corners (`emitAtBezPt`, `Font.fs:1540`)
  currently keys off `reverse`; it must key off `sign o` instead, since a trace
  wholly on one side of the spine has both its edges on that side.
- Caps: `squeezeCap` (`Font.fs:1829`) already scales a cap perpendicular to the
  tangent.  Generalise it to `transformCap perpScale perpShift` and each trace
  reuses the normal serif/flare/bulb cap geometry, squeezed to `trace_weight`
  and shifted to its centre.  (This is a strict improvement on `stroked`, whose
  rails get no cap styling at all.)
- The outer result becomes `traces` closed paths per stroke instead of one:
  `getDactylConstantOffsetOutlines` returns a list already, so this is a
  `List.collect` over trace indices.

**Self-intersection guard.**  Offsetting a spine by `c` self-intersects on the
concave side once `|c|` approaches the curvature radius — visible today on
`stroked` at tight bends.  Curvature is now available for free (§3.3), so clamp
per sample:

```fsharp
let cSafe = if k = 0.0 then c else clamp c (-0.9 / abs k) (0.9 / abs k)  // sign-aware
```

### 3.2 Layer 2: the *pen* record

The ribbon renderer's terms are currently open-coded as `let` bindings inside a
280-line closure.  Lift them into one record so the trace loop, the mobius panel
builder and any future term all consume the same object:

```fsharp
/// Everything known about one arc-length sample of a spine.
type PenSample =
    { s: float            // arc length from stroke start
      sFrac: float        // s / totalLen
      th: float           // spine tangent
      curvature: float    // signed 1/radius, analytic (see §3.3)
      side: float }       // +1 / -1, for per-side noise

/// A pen is what turns a spine into ribbons.
type Pen =
    { /// perpendicular spine displacement, font units
      displace: PenSample -> float
      /// ribbon half-width, font units
      halfWidth: PenSample -> float
      /// ribbon twist angle; apparent width scales by |cos φ|
      twist: PenSample -> float
      /// (lateral centre, width scale) per parallel trace
      traces: (float * float) list }

module Pen =
    /// Build the pen implied by an axis set. Each axis contributes one term.
    let ofAxes (axes: Axes) (thickness: float) (totalLen: float) : Pen = ...
```

Width terms multiply, displacement terms add, so `ofAxes` is a fold and each
axis is ~3 lines in one place instead of a hand-placed insertion into the
middle of `buildSide`.

This is worth doing on its own merits even before traces: it makes
`getDactylConstantOffsetOutlines` a sampler plus a pen rather than one function
that knows about seven artistic axes.

### 3.3 Curvature is already available — which unblocks `pressure`

TODO.md records that `pressure` was tried with "curvature-from-tangent-samples"
and reverted for looking noisy.  That is a property of the estimator, not of the
idea: `DactylSpline.getCurvature` (`DactylSpline.fs:218`) computes **analytic**
curvature of the spine cubic from its first and second derivatives, and the
sampler already evaluates that same cubic at the same `t` in `bezEval`
(`Font.fs:1327`).  Returning `κ` alongside `(x, y, θ)` costs one extra call per
sample and is smooth by construction.

Then, bounded so a tight bend cannot blow up:

```fsharp
// pressure: tighter curves get thicker, straight runs stay thin
let pressureFactor (p: PenSample) =
    1.0 + axes.pressure * tanh (abs p.curvature * thickness)
```

---

## 4. What this unlocks from TODO.md

Every remaining "artistic axes (not yet implemented)" item becomes one term:

| TODO axis | becomes |
|---|---|
| **pressure** | a `halfWidth` factor, `tanh(|κ|·T)` — §3.3 |
| **ink_spread** | a `halfWidth` term, `+ ink_spread · T · bump(s)` |
| **split_nib** | not an axis at all: `traces = 2` with a small `trace_spread` |
| **gravity** | a `displace` term, `+ sag · sin(π · sFrac)` projected onto world-down (so it sags regardless of stroke direction) |
| **bounce** | glyph-level, not sample-level: a per-glyph baseline offset seeded from the code point, alongside the existing per-glyph randomisation in `glyphRandom.js` |

Related tidy-up, worth flagging but **not** bundled in: `contrast` is applied
inside `addPolarContrast` (`Font.fs:86`) as an additive x-stretch on *every*
polar offset in the file — caps, serifs and dots included — rather than as a
width-vs-direction ratio.  Under the pen model it belongs in `halfWidth`
alongside `nib`, which it closely resembles.  Doing so changes existing
rendering, so it deserves its own change and its own snapshot review.

---

## 5. UI

The `artistic` category already holds 12 controls in a flat scrolling list;
adding 4 trace axes and 3 pen axes would take it to ~19.  Three cheap changes
keep it usable, one optional one makes it pleasant.

### 5.1 Pen presets (chip row) — *recommended, do this with §3.1*

A row of chips at the top of the **Artistic** category, each applying a named
bundle of artistic axes and leaving backbone/outline axes alone:

> `Solid` · `Broad nib` · `Brush` · `Marker` · `Sketch` · `Inline` · `Split nib` · `Ribbon` · `Backscratch`

This is what actually replaces the discoverability of the two checkboxes — one
click still gets you the scratchy font, but now it lands you *inside* a space
you can keep tuning, instead of at a dead end.  Define the table in F# next to
the axes so it exports through `Api.controlDefinitions`' existing mechanism:

```fsharp
static member presets =
    [ "Solid",      [ "traces", 1.0 ]
      "Sketch",     [ "traces", 3.0; "trace_spread", 2.0; "trace_weight", 0.33
                      "trace_jitter", 0.5; "roughness", 0.3 ]
      "Split nib",  [ "traces", 2.0; "trace_spread", 0.45; "trace_weight", 0.12
                      "nib", 0.8 ]
      ... ]
```

The Textures tab already has chip styling (`.proof-chip`, `App.jsx:2217`) to
reuse.

### 5.2 Sub-groups inside a category

Let a category collapse into labelled sub-groups so 19 sliders read as five
short lists:

- **Pen** — `weight`, `contrast`, `nib`, `nib_angle`, `pressure`
- **Ends** — `taper`, `taper_end`, `flare`, `end_bulb`, `serif`, `joint_gap`
- **Hand** — `wobble`, `roughness`, `ink_spread`, `gravity`, `bounce`
- **Traces** — `traces`, `trace_spread`, `trace_weight`, `trace_jitter`
- **Ribbon** — `mobius`

Rather than widening the 4-tuple in `Axes.controls` (45 entries to edit), add a
sibling list and let `Api.getControlDetails` attach a `group` field:

```fsharp
static member groups =
    [ "Pen",    [ "weight"; "contrast"; "nib"; "nib_angle"; "pressure" ]
      "Traces", [ "traces"; "trace_spread"; "trace_weight"; "trace_jitter" ]
      ... ]
```

`App.jsx`'s `controlsByCategory` (line 423) gains one more level of grouping;
render a sub-heading only when a group has more than one control.

### 5.3 Dim inapplicable axes

`nib_angle` does nothing at `nib = 0`; `taper_end` does nothing at `taper = 0`;
all three `trace_*` do nothing at `traces = 1`.  Add

```fsharp
static member dependsOn = [ "nib_angle", "nib"; "taper_end", "taper"
                            "trace_spread", "traces"; ... ]
```

and grey out (do not hide) the dependent slider when its parent is at default.
This shrinks the *apparent* size of the panel without hiding anything, and
matters much more once traces exist.

### 5.4 Stroke preview *(optional, nice)*

A ~120×60 inline SVG at the top of the Artistic panel showing a single S-curve
and a 45° bar drawn with the current pen and nothing else.  It updates on every
slider drag at roughly one glyph's cost, so nib angle, taper, trace spacing and
jitter are all legible without hunting for a letter that happens to show them.
Needs one new export, `Api.renderPenSample axes -> string`, reusing
`charToSvg`'s path on a synthetic 3-knot curve.

### 5.5 Free consequences, and one gap noticed

The **Tweens** and **Visual Diffs** tabs enumerate `controlDefinitions`, so new
axes appear in both with no per-axis work — a `traces` tween row is immediately
more informative than the current on/off `tween-stroked`.

Noticed while checking migration cost: **axes are not persisted anywhere** — not
in the URL, not in `localStorage` (`App.jsx:117` starts from `defaultAxes` every
load).  So removing `stroked`/`scratches` breaks no saved state.  It also means
a tuned pen cannot be shared or recovered, which is a real gap that pairs
naturally with presets — a "copy settings link" button writing non-default axes
into the query string would be a small, separate win.

---

## 6. Rollout

Ordered so each phase is independently reviewable, and the first is provably a
no-op.

| phase | change | expected visual diff |
|---|---|---|
| **1** | Extract the `Pen` record (§3.2) — no axis changes | **none** (this is the acceptance test) |
| **2** | Add the four `trace_*` axes; delete `stroked`, `scratches`, `spiroToLines`, `getStroked`, `getScratches` and the ≥30 thickness clamp | defaults unchanged; two tween rows replaced by four |
| **3** | UI: preset chips, sub-groups, dimming (§5.1–5.3) | sidebar layout only |
| **4** | New pen terms: `pressure`, `ink_spread`, `gravity`, and glyph-level `bounce` | new tween rows only, defaults off |
| **5** | *(optional)* unify `contrast` into `halfWidth` (§4) | real diff, needs its own review |

### Snapshot impact

`tweens.spec.js` screenshots **one file per axis**, discovered from the page, so
added axes create new baselines and leave existing ones untouched, and removed
axes leave a stale file rather than failing.  The single shot that does change
is `tabs.spec.js-snapshots/tweens-chromium-linux.png`, which captures the whole
grid.  Everything else (`font`, `glyphs`, `proofs`, `splines`) renders at
default axes, so **those must not move** — that is the check that phases 1 and 2
are regression-free.

(Baselines stay a manual step for the repo owner via the *Visual Tests*
workflow, per `CLAUDE.md`.)

### Costs and risks

- **Path count scales with `traces`.**  N traces means N closed paths per
  stroke, so OTF export size and render time grow linearly — hence the cap at 6.
- **`traces` × `mobius`** multiplies panel counts (traces × half-twists).
  Allowed, but expect it to be slow at the extremes; the randomiser's
  centre-biased sampling (`glyphRandom.js:32`) already makes stacked extremes
  rare.
- **Concave self-intersection** at large `trace_spread` on tight bends — guarded
  by the curvature clamp in §3.1, which is more than today's code does.
- **Ragged scratch caps are not reproduced exactly.**  Today's scratch look
  comes partly from bespoke asymmetric cap geometry (`Font.fs:1036`).
  `trace_jitter` plus `roughness` gets the same character but not the same
  pixels.  If an exact match matters, the raked cap is itself generalisable — a
  `cap_rake` axis skewing the cap edge off perpendicular, which is a real
  calligraphic feature worth having independently.
