# Growing fonts: generative growth from Dactyl backbones

Brainstorm for a feature that "grows" letterforms starting from the existing
Dactyl backbones, applying rules / cellular automata so strokes fill space,
fuse, and interact — inspired by Namco's *Techno Drive* (1998) logotype
(are.na/block/3325152): inflated blobby strokes, near-constant whitespace
channels between them, fused glyphs, and multiple nested outline layers.

## Why Dactyl is unusually well suited to this

Dactyl glyphs are *skeletons first*: every glyph is a set of spine curves
(`Element` → `Curve` of `Knot`s) that only later get inflated by `thickness`
into outlines. A growth process is just a different — smarter — inflation
stage. The `constant_offset` mode already produces dense sampled polylines
along the spine, which is exactly the seed geometry a field/grid simulation
wants. So the plug-in point is clear:

```
backbone knots → solved spline → [ GROWTH STAGE ] → contours → SVG / OTF
```

## Idea 1 — Constant-gap inflation (the Techno Drive look, and the best MVP)

Instead of offsetting the spine by a fixed `thickness`, let every stroke
swell until the *whitespace channel* between it and the nearest other stroke
(or glyph neighbour, or em-box edge) narrows to a target `gap`. Equivalently:
compute the distance field of all spines, and give each point of ink the
radius `min(max_radius, (distance_to_nearest_other_spine − gap) / 2)`.

* Strokes bulge into open space and pinch where they pass close — exactly the
  reference image.
* Counters (the holes in `a`, `e`, `o`) survive automatically because the
  opposing stroke pushes back; `gap` directly controls legibility.
* Deterministic, fast, no RNG — friendly to visual regression tests.
* Run it over a whole word's combined field and glyphs kiss or fuse into a
  logotype; `tracking` becomes "how much the letters melt together".

Axes: `grow` (0 = classic offset … 1 = full space-fill), `gap`, `fuse`
(allow inter-glyph merging), `grow_smooth` (corner rounding of the result).

## Idea 2 — Cellular automata on a raster petri dish

Rasterise the backbone into a per-glyph (or per-word) grid; the ink cells are
the living seed; run N generations of a rule; vectorise back with marching
squares + smoothing.

* **Dilation-style rules** (`B1..8/S0..8` Life-like notation as an axis!):
  plain B-any/S-all is uniform growth; sparser birth rules grow crystalline /
  dendritic edges.
* **Inhibited growth**: a cell may only turn on if it has ≥k ink neighbours
  *and* is farther than `gap` cells from ink of a *different* stroke id —
  this is the CA formulation of Idea 1, but it naturally supports asymmetric
  and noisy variants.
* **Growth budget**: stop when ink coverage hits a target fraction of the em
  box ("fill the space" knob), or after `generations` steps — animatable, so
  the Explorer could literally show letters growing.
* **Lenia / SmoothLife** (continuous CA) for soft organic amoeba edges rather
  than pixel stairs.

## Idea 3 — Reaction–diffusion (Turing patterns rooted on the skeleton)

Seed Gray–Scott U/V concentrations along the spine and iterate. Feed/kill
rate axes morph the result between spots, stripes, labyrinths and coral. Two
flavours:

* **Grown silhouette**: threshold the field → blobby organic letter shells.
* **Filled texture**: keep the classic outline but fill its interior (or its
  *counters*) with the pattern — zebra letters, coral letters.

## Idea 4 — Differential growth on the outline (vector-native)

Take the existing dense outline polyline and iterate: each vertex is
attracted to its neighbours, repelled by all nearby vertices (including other
glyphs), jittered slightly, and edges resample as they stretch. Outlines
buckle into wavy, brain-coral folds that fill available space — the
inconvergent / Anders Hoff look. Repulsion against neighbouring glyphs keeps
words legible; turning it down lets letters interlock like puzzle pieces.
Stays vector end-to-end, so OTF export is straightforward.

## Idea 5 — Branching growth off the spine

The spine as a trunk, empty space as light:

* **Space colonisation** (venation/tree algorithm): scatter attractor points
  in the counters and margins; grow twigs from the backbone toward them.
  Serifs become roots and branches; at low strength it reads as organic
  flared serifs, at high strength as ivy letters.
* **DLA (diffusion-limited aggregation)**: random walkers stick to the
  backbone → frost/lichen crusted letters.
* **L-systems** sprouting at knots or at fixed arc-length intervals along the
  spine, angle-relative to the local tangent (the tangent data is already in
  `Segment`) — thorns, leaves, feathers.

## Idea 6 — Interaction rules between glyph parts

Beyond filling space, rules about *relationships*:

* **Stems negotiate**: parallel strokes within a glyph repel (opening
  counters) or attract (condensing) — a smarter, shape-aware `monospace`.
* **Cross-glyph ligatures for free**: any two strokes from adjacent glyphs
  closer than `fuse_distance` merge with a smooth fillet — emergent
  ligatures everywhere, not just fi/fl.
* **Layered outlines** (the reference has white core / light blue / dark
  blue / black keyline): after growth, emit successive offset contours as
  stacked SVG paths, and eventually a COLR colour font. A `layers` axis
  (count + spacing) would make everything instantly Y2K.

## Where it would live in the codebase

* **Prototype in the web worker (JS/canvas)**: `Api.fs` already exposes spine
  and outline data; a `grow` message type in `web/src/worker.js` could
  rasterise to `ImageData`, run the CA / distance field, marching-squares it
  back, and hand SVG paths to `App.jsx`. Fast iteration, progress callbacks
  already wired.
* **Graduate to F# (`src/generator/`)** once a winner emerges, so OTF export
  and all tabs get it: a `Growth.fs` stage between glyph reduction and
  outline emission, driven by new entries in `Axes.fs` (`grow`, `gap`,
  `generations`, `grow_rule`, `fuse`, `layers`…). Note `Controls` may want a
  new `Choice` variant for rule dropdowns.
* **Determinism**: seed any RNG from a `seed` axis so visual tests stay
  stable.
* **Performance**: per-word grid at ~4–8 font-units per cell, a few dozen
  generations — well within worker budget; distance-field inflation is
  cheaper still (one pass with a jump-flood or chamfer transform).

## Suggested first slice

1. `grow` + `gap` axes implementing **constant-gap inflation** (Idea 1) on
   the Font tab, worker-side raster prototype, marching squares back to SVG.
2. Add `fuse` across glyph boundaries → instant logotype mode.
3. Add `layers` for the nested-outline Y2K rendering.
4. Then unlock `grow_rule` as a dropdown: `inflate | life | reaction-diffusion
   | differential | branching`, each reusing the same seed/rasterise/vectorise
   scaffolding.

## SDF directions (added after the first slice shipped)

The shipped growth field `f = rAllowed − d1` is already *almost* a signed
distance field: its zero iso-contour is the letter edge, and wherever no
opposition is active it is a true SDF (|∇f| = 1).  Making that explicit
unlocks the standard SDF toolbox:

1. **Jump-flood the field instead of per-cell nearest searches.**  Splat the
   spine samples into the grid and run a jump-flood (JFA) pass that gives
   every cell its nearest sample *and* that sample's id in O(cells),
   independent of sample count; a second candidate slot constrained to
   "opposes the first" yields dOpp.  The field then costs milliseconds, not
   seconds, and is computed once per text/axes change.
2. **Threshold the field in a fragment shader.**  Upload (d1, dOpp) as a
   two-channel texture; the shader computes f and paints every keyline band
   as just another threshold of the same texture with `smoothstep` AA.
   grow / gap / layers become uniforms — dragging sliders never recomputes
   anything, and animating the threshold makes letters visibly grow.
   Marching squares remains only as the vector export path.
3. **Shape algebra.**  smooth-min between per-glyph SDFs = controllable
   fusion/ligatures (a blend-radius `fuse` slider); erode-then-dilate =
   a field-space `soft_corners`; subtraction = counter protection.
   *(Partly shipped: a `fuse` slider melts neighbouring glyphs into a logotype
   by relaxing — and overshooting — the constant gap only where the opposition
   is cross-glyph.  A third field channel, `cross`, tags cross-glyph
   opposition, so counters inside a glyph stay open at any fuse.  This is the
   field-relaxation form of the same idea; a literal per-glyph smooth-min is
   still open for smoother fillets and erode/dilate corners.)*
4. **Exact segment SDFs.**  Distance to the resampled polyline's *segments*
   (capsule SDF) instead of to point samples decouples field accuracy from
   sample spacing.
5. **MSDF export.**  Dactyl generates the field natively, so exporting
   multi-channel SDF atlases (msdfgen-style) for game/GPU text rendering is
   unusually direct.
6. **Farther out:** domain-warp the field with noise for field-space wobble
   *(shipped: a `warp` slider displaces the field lookup by value noise —
   `domainWarp` in `growth.js`, matching GLSL noise + `uWarp*` uniforms in
   `GrowCanvas.jsx`, seeded for determinism)*; run reaction–diffusion inside
   the SDF band so patterns hug the letterform; extract the ridge (medial
   axis) of an *imported* font's SDF to reverse-engineer backbones —
   Dactyl-izing arbitrary fonts.

Status: items 1–2 are implemented (JFA in `web/src/growth.js`, shader
preview in `web/src/GrowCanvas.jsx`); item 3 is partly implemented (the
cross-glyph `fuse` slider — field channel in `growth.js`, `uFuse` uniform in
`GrowCanvas.jsx`); item 6's domain-warp `warp` slider is implemented; the rest
of 4–6 are open.
