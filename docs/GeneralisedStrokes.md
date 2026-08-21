# Generalised stroke rendering — proposal

*Status: implemented.*  Phases 1–4 of the rollout in §6 are on
`claude/generalized-stroke-design-85lros`; §7 records where the built thing
differs from the proposal, and why.  Phase 5 (`contrast`) is deliberately still
outstanding.

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
| `traces` | `SteppedFracRange(1, 6, 1)` | `1` | number of parallel strokes drawn per spine. 1 = today's solid stroke |
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
`stroked` at tight bends.  Curvature is now available for free (§3.3), so pull
the offset back per sample.

A hard clamp turned out to be the wrong shape (§7.2): it leaves a visible notch
wherever it starts and stops biting.  The built version saturates smoothly
instead, and only where the offset heads toward the inside of the bend:

```fsharp
// r = 1/|k|; ~identity while |o| << r, and never reaches r
if (o > 0.0) <> (k > 0.0) then o else r * tanh (o / r)
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
    { /// spine displacement as (offset, d(offset)/ds) — the derivative lets the
      /// caller re-angle the tangent so the displaced spine's normal stays normal
      displace: PenSample -> float * float
      /// half-width before the twist factor — what `mobius` pinches
      baseHalfWidth: PenSample -> float
      /// ribbon half-width, font units
      halfWidth: PenSample -> float
      /// ribbon twist angle; apparent width scales by |cos φ|
      twist: PenSample -> float
      /// (lateral centre, width scale, extra end trim) per parallel trace
      traces: (float * float * float) list }

module Pen =
    /// Build the pen implied by an axis set. Each axis contributes one term.
    let ofAxes (axes: Axes) (thickness: float) (totalLen: float) (isClosed: bool) : Pen = ...
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

The sidebar holds **44 axes in 5 collapsible sections** today, and `artistic` is
already the largest at 13.  These changes remove 2 axes (`stroked`,
`scratches`) and add 8 (4 `trace_*`, plus `pressure`, `ink_spread`, `gravity`,
`bounce`), so `artistic` would reach 19 in one flat scrolling list.  It needs
restructuring either way; §5.2 is the recommended shape.

### 5.1 Pen presets (chip row) — *recommended, do this with §3.1*

A row of chips at the very top of the controls list, above the first section —
a preset is a statement about the whole pen, so it cuts across the groups in
§5.2 rather than living inside one.  Each chip applies a named bundle of pen
axes and leaves the backbone and render axes alone:

> `Solid` · `Broad nib` · `Brush` · `Marker` · `Sketch` · `Inline` · `Split nib` · `Ribbon` · `Backscratch` · `Tired hand`

This is what actually replaces the discoverability of the two checkboxes — one
click still gets you the scratchy font, but now it lands you *inside* a space
you can keep tuning, instead of at a dead end.  The table lives in F# next to
the axes (`Axes.presets`) and exports through `Api.penPresets`:

| preset | axes |
|---|---|
| **Solid** | *(all pen axes at default)* |
| **Broad nib** | `nib 1.0`, `nib_angle 30` |
| **Brush** | `pressure 0.8`, `taper 0.2`, `taper_end 0.45`, `ink_spread 0.15` |
| **Marker** | `ink_spread 0.55`, `flare 0.1`, `end_bulb 0.3` |
| **Sketch** | `traces 3`, `trace_spread 2.0`, `trace_weight 0.33`, `trace_jitter 0.5`, `roughness 0.3` |
| **Inline** | `traces 2`, `trace_spread 1.2`, `trace_weight 0.15` |
| **Split nib** | `traces 2`, `trace_spread 0.8`, `trace_weight 0.2`, `nib 0.8` |
| **Ribbon** | `mobius 1.0`, `taper 0.2` |
| **Backscratch** | `traces 5`, `trace_spread 2.4`, `trace_weight 0.06`, `trace_jitter 0.9`, `roughness 0.5`, `wobble 0.2` |
| **Tired hand** | `gravity 0.7`, `wobble 0.4`, `roughness 0.25` |

A second list, `Axes.presetAxes`, names the 16 axes a preset *speaks for*.
Applying a chip resets all 16 to their defaults and then applies that chip's
values, so the chips are alternatives rather than layers — clicking `Sketch`
after `Broad nib` gives a sketch, not a sketchy nib.  `weight`, `contrast` and
`serif` are deliberately outside that set (typographic choices the user makes
independently of pen character), as are `bounce` (a property of the line, not
the stroke) and `joint_gap` (a stencil effect).

The Textures tab's chip styling (`.proof-chip`) is reused, and the chip for the
preset the axes currently match exactly is shown selected.

### 5.2 Retire `artistic`; promote the groups to top level — *recommended*

The natural instinct is to keep `artistic` as a roof and nest labelled
sub-groups underneath it.  Sketching both side by side (see
[the comparison](https://claude.ai/code/artifact/39f7da82-44d5-435e-a8cf-1e79b3a881fa))
makes the flat option the clear winner: **the accordion is already the
nesting.**  A second level inside it costs a click on every slider and a new
field threaded through `Axes.fs` → `Api.fs` → `App.jsx`, and leaves a category
name that never said what was inside it.

Dissolve `artistic` and let its groups stand as top-level sections:

| section | axes | |
|---|---|---|
| **Backbone** | `width`, `height`, `x_height`, `descender_depth`, `spacing`, `leading`, `monospace`, `slant`, `cursive`, `roundedness`, `overshoot`, `balance` | 12, unchanged |
| **Pen** | `weight`, `contrast`, `nib`, `nib_angle`, `pressure` | 5 |
| **Ends** | `taper`, `taper_end`, `flare`, `end_bulb`, `serif`, `joint_gap` | 6 |
| **Hand** | `wobble`, `roughness`, `ink_spread`, `gravity`, `bounce` | 5 |
| **Traces** | `traces`, `trace_spread`, `trace_weight`, `trace_jitter`, `mobius` | 5 |
| **Render** | `softness`, `axis_align_caps`, `outline`, `filled` | 4, was `outline` |
| **Experimental** | *(unchanged)* | 8 |
| **Debug** | *(unchanged)* | 5 |

Two things fall out of drawing it, neither of them obvious from the axis list
alone:

- **`mobius` folds into Traces.**  A "Ribbon" section holding exactly one axis
  is noise, and "how many ribbons and how they twist" is one idea.
- **`weight` and `contrast` move into Pen.**  They live in `outline` today, not
  `artistic` — so under *any* grouping of the artistic axes they would end up
  separated from `nib` and `pressure`, which is backwards: they are the base of
  the same half-width term (§1).  Once they leave, `outline` is holding only
  render switches, hence the rename to **Render**.

#### Cost

Almost nothing, because the section renderer already loops over whatever
categories it finds:

- `Axes.fs` — a one-word edit per axis line, changing the `category` string in
  the 4-tuple.  No new list, no new field, no `Api.getControlDetails` change.
- `App.jsx` — four new entries in `categoryIcons` and a `CLOSED_CATEGORIES` list
  in the `openCategories` initialiser.  As built, **Backbone**, **Pen** and
  **Render** start open and Ends/Hand/Traces start closed, which keeps the
  opened height close to today's.  `controlsByCategory` and the rendering below
  it are untouched.
- One extra edit not foreseen: the sections render in `Axes.controls` order, so
  that list was re-sorted into the section order above.  It also drives the
  per-glyph randomiser's stream of `rand()` calls, so re-sorting changes which
  glyph gets which roll — cosmetic, but it does move the randomised snapshots.
- Randomisation and Visual Diffs keep working unchanged: `glyphRandom.js` skips
  by category name (`SKIPPED_CATEGORIES = ['experimental', 'debug']`), and none
  of the new names are in that list.

Suggested icons, matching the existing Material Symbols set: `ink_pen` (Pen),
`line_end` (Ends), `gesture` (Hand), `density_medium` (Traces); `brush` stays
with Render, `straighten` with Backbone.

#### The one real cost

The collapsed sidebar is a hover-to-expand icon rail, and it goes from **5
icons to 8**.  There is also no longer a single click that hides every effect at
once.  Defaulting the four new sections closed except Pen should absorb both —
but this is the part worth eyeballing on a phone before committing, since the
rail is the whole mobile navigation.

### 5.3 Dim inapplicable axes

`nib_angle` does nothing at `nib = 0`; `taper_end` does nothing at `taper = 0`;
all three `trace_*` do nothing at `traces = 1`.  Add

```fsharp
static member dependsOn = [ "nib_angle", "nib"; "taper_end", "taper"
                            "trace_spread", "traces"; "trace_weight", "traces"
                            "trace_jitter", "traces" ]
```

and grey out (do not hide) the dependent slider when its parent is at default —
`.control-group.inactive { opacity: 0.45 }`.  The slider still works; moving the
parent brings it back to full strength.
This shrinks the *apparent* size of the panel without hiding anything, and
matters much more once traces exist.

### 5.4 Stroke preview *(optional, nice)*

A ~120×60 inline SVG at the top of the Pen section showing a single S-curve
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

| phase | change | expected visual diff | |
|---|---|---|---|
| **1** | Extract the `Pen` record (§3.2) — no axis changes | **none** (this is the acceptance test) | done |
| **2** | Add the four `trace_*` axes; delete `stroked`, `scratches`, `spiroToLines`, `getStroked`, `getScratches` and the ≥30 thickness clamp | defaults unchanged; two tween rows replaced by four | done |
| **3** | UI: preset chips, retire `artistic` for top-level groups, dimming (§5.1–5.3) | sidebar layout only | done |
| **4** | New pen terms: `pressure`, `ink_spread`, `gravity`, and glyph-level `bounce` | new tween rows only, defaults off | done |
| **5** | *(optional)* unify `contrast` into `halfWidth` (§4) | real diff, needs its own review | not done |

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

---

## 7. As built — where the implementation differs, and why

Phases 1–4 landed as described above.  Five things came out differently once the
code existed; each is a correction to this document, not a shortcut.

### 7.1 A no-op has to be proved, not asserted

Phase 1's acceptance test is "**none**", and unit tests are a poor instrument
for that — they check the properties you thought to check.  What actually
carried the phase was `src/generator/tests/PenNoOpDump.fs`: a harness that
renders 70 glyphs × ~19 axis variants to SVG and writes them to `$DUMP_OUT`, so
before/after is a `diff` over ~840 renderings.

It earned its keep twice.  It caught a cap-transform helper that was
algebraically the identity but emitted `-0` where the old code emitted `0`, and
it caught the pointed-tip case where `addPolarContrast` still adds
`contrast · thickness` at zero distance — so "offset by nothing" is not the same
as "don't offset".  Both are invisible to any test anyone would have written,
and both would have shown up later as an unexplained snapshot diff.

### 7.2 The curvature clamp needed two fixes

The guard in §3.1 shipped, but not on the first two attempts:

1. **Curvature was filed against the wrong arc length.**  The first version
   indexed the curvature sequence by uniform spline parameter while the offsets
   were indexed by arc length, so on unevenly-parameterised segments the clamp
   consulted the curvature of a different part of the curve.  Fixed by
   accumulating true chord lengths alongside the samples.
2. **A hard clamp leaves notches.**  Cutting the offset at `0.9/|k|` makes
   neighbouring samples land either side of the cut, which reads as a small
   corner in the outline.  Replaced with `r·tanh(o/r)` (§3.1), which is
   ~identity while the offset is small against the radius and asymptotes below
   it, so the edge never reaches the cusp at all.

The fold-back loops this guards against were only ever visible in a rendered
image — subpath counts said the geometry was fine.

### 7.3 `gravity` needed the sampler told about it

`widthVariesAlongStroke` decides whether a straight spine segment gets interior
samples.  A straight segment otherwise carries only its two endpoints, and
`gravity`'s sag is zero at both ends by construction — so `T`'s crossbar did not
move at all until the new axes were added to that predicate.  `ink_spread` had
the same problem and the same fix.

`pressure` is deliberately **not** in the predicate: it is driven by curvature,
which is zero along a straight segment, so forcing samples there would cost
geometry for no visible change.

### 7.4 `traces` had to become a stepped control

`Range(1, 6)` derives its slider step as `(max - min) / 20`, i.e. `0.25` — so
the slider could hand the renderer 2.25 traces, corrupting both the loop bound
and the even spacing.  It is now `SteppedFracRange(1, 6, 1)`, and both consumers
round defensively, since the randomiser ignores step.

### 7.5 One pre-existing quirk surfaced

A vertical bar's bounding box shifts by one unit when arc-length sampling is
forced on, independent of any new axis (provable by setting `roughness` to
`1e-9`).  It is integer rounding of the forced samples and predates this work;
noted here so it is not mistaken for a regression.  The relevant test compares
against a same-sampling baseline rather than the unsampled one.

### 7.6 Still outstanding

- **Phase 5** (`contrast` into `halfWidth`, §4) — unchanged from the proposal: a
  real rendering change that deserves its own review.
- **§5.4**, the inline stroke preview — not built.  Worth revisiting now that
  presets exist, since the two answer the same "what does this axis do" question
  from opposite ends.
- **Snapshot rebaselining** is the repo owner's manual step, per `CLAUDE.md`.
  Two shots change, both verified by eye first:
  - `tabs.spec.js-snapshots/tweens-chromium-linux.png` — the grid gains and loses
    axis rows.
  - `tweens.spec.js-snapshots/tween-pressure-chromium-linux.png` — **a stale
    baseline**, not a new one. The reverted `pressure` attempt left a committed
    snapshot behind in which all six samples are identical, because the axis did
    nothing; the new implementation thickens the bowl of the `a` while leaving
    its straight stem alone, so it legitimately differs. The other new axes
    (`ink_spread`, `gravity`, `bounce`, the four `trace_*`) have no baseline and
    are generated rather than failed, per `updateSnapshots: 'missing'`.

  Other stale baselines sit alongside it for axes that no longer exist —
  `tween-stroked` and `tween-scratches` are newly orphaned by §3.1, joining
  `tween-thickness`, `tween-italic`, `tween-alt-a-g`, `tween-show-comb` and
  `tween-soft-corners` from earlier changes. They do not fail anything; deleting
  them is left to the repo owner along with the rest of the snapshot handling.

- **A sidebar feature can move every tab snapshot.**  `.sidebar` is shrink-to-fit
  (`width: auto`, 280–400px) and the tab tests screenshot `.preview-content`
  beside it, so anything that widens the sidebar narrows the preview on *every*
  tab. A wrapping flex row's max-content width is all of its items on one line —
  wrapping is not considered — so the ten preset chips pushed the sidebar to its
  400px cap and moved 13 snapshots. `width: 0` keeps the row out of that
  intrinsic measurement and `min-width: 100%` fills the settled width. Worth
  knowing before adding anything else to the sidebar.
