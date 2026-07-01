# Dactyl Font Rendering Pipeline

This document describes how a character travels from a glyph string definition through to final SVG output.  The code lives primarily in `src/generator/Font.fs`, with types defined in `src/generator/GeneratorTypes.fs` and `src/generator/Axes.fs`.

---

## Pipeline overview

```
Character (char)
  │
  ▼  Font.charToElem
  ├─ GlyphStringDefs.glyphMap lookup → glyph string (e.g. "x(c)~(xb)l~")
  ├─ parse_curves → Element tree (Curve / Dot / EList / Space)
  ├─ constrainTangents   (if axes.constraints)
  ├─ monospace           (if axes.monospace > 0)
  ├─ translateByThickness
  └─ italicise           (if axes.italic ≠ 0)
       │
       ▼  Font.getOutline
       ├─ stroked mode      → getStroked (four parallel lines per stroke)
       ├─ scratches mode    → getScratches (textured paint-stroke effect)
       └─ outline mode:
            ├─ constant_offset  → getDactylConstantOffsetOutlines
            ├─ dactyl_spline    → getDactylSansOutlines
            └─ (legacy)         → getSpiroSansOutlines
                 │  optionally: constrainTangents
                 ▼
             Element tree of outline Curves
                 │
                 ▼  Font.elementToSvgPath
             SVG <path d="…"> strings
```

---

## Stage 1 — Spine construction (`charToElem`)

`Font.charToElem` builds the **spine** (the single-width centreline) of a glyph:

1. `Glyph(ch)` is looked up in `GlyphStringDefs.glyphMap` and parsed by `parse_curves` into an `Element` tree — a union of `Curve`, `Dot`, `EList`, and `Space` nodes.
2. **`constrainTangents`** (optional) clips tangent angles so smooth curves cannot exit the glyph bounding box.
3. **`monospace`** normalises advance widths to a fixed cell when `axes.monospace > 0`.
4. **`translateByThickness`** shifts the spine outward by half the stroke thickness so that the outline stays within the original bounding box.
5. **`italicise`** applies an affine horizontal shear.  Because shearing distorts Bézier handles, this step subdivides curves first for a better approximation.

---

## Stage 2 — Outline expansion (`getOutline`)

`Font.getOutline` dispatches to one of four strategies based on axes:

### `getDactylSansOutlines` (default)
The main outline path.  For each solved Bézier segment from the DactylSpline:
- Calls `offsetSegment` to build the left and right offset paths at distance ±`thickness/2`.
- Joins adjacent segments with **miter** geometry at convex corners and a **bisector** point at concave corners.
- Calls `startCap` / `endCap` to close open stroke ends.

### `getDactylConstantOffsetOutlines` (when `constant_offset = true`, or automatically when any *artistic width* axis is active)
Walks every cubic Bézier at 16 t-steps, emitting perpendicular offset samples as straight-line `Corner` knots.  Produces smoother outlines for strongly curved strokes at the cost of more points.  Cap and join logic is shared with the default path.

Because it samples the spine densely, this path also hosts the arc-length-varying **artistic axes** (`sampledArtistic` auto-routes here when `dactyl_spline` is on):

| Axis | Effect (via `widthAt` / `displace`) |
|------|-------------------------------------|
| `nib`, `nib_angle` | Broad-nib pen: half-width scales with the stroke's angle relative to the nib, so strokes along the nib nearly vanish. |
| `taper`, `taper_end` | Brush taper: half-width narrows over the first/last part of the stroke down to `taper_end` of full width (`0` = a point). |
| `wobble` | Hand-drawn waviness: the spine is displaced perpendicular to its tangent by an integer number of wavelengths (so ends stay put). |
| `mobius` | Twisting ribbon: width follows `|cos θ|` of a per-arc-length twist, split into separate closed panels at the pinch points. |

Because these make the two sides of a stroke (and its ends) different widths, the corner and cap handling is specialised:
- **Corners** offset each side at its own width. Gentle (≤ right-angle) corners use the exact intersection of the two offset edges; sharp outer corners bevel; sharp inner corners use a clamped bisector.
- **Caps** (nib / taper, `taper_end > 0`) build the normal cap geometry — so ends extend to the same length and keep the serif/flare/bulb style — then squeeze it perpendicular to the reduced end width. Pointed taper (`taper_end = 0`) simply meets at the spine endpoint.

### `getSpiroSansOutlines` (legacy, when `dactyl_spline = false`)
Same structural approach as the DactylSpline path but uses the Spiro-solved segments.

### `getStroked` / `getScratches`
Alternative rendering modes (four-line backscratch font; textured scratch effect) that bypass the normal outline logic entirely.

---

## Cap types

Both outline modes call the shared `cap` helper, which selects the cap geometry based on axes:

| Condition | Result |
|-----------|--------|
| `serif ≠ 0` and not a joint | Rectangular bracketed serif (6 corner knots) |
| `flare ≠ 0` and not a joint | Flared endcap (bell-curve profile, 4–5 knots) |
| `end_bulb ≠ 0` and not a joint | Circular bulb at stroke end |
| joint (two strokes meeting) | Simple miter / bevel join, no cap decoration |
| default | Flat perpendicular endcap (2 corner knots) |

`axis_align_caps` snaps cap angles to the nearest 90° so they stay visually horizontal or vertical.

---

## Soft corners (`roundCorners`)

When `soft_corners > 0`, every `Corner` knot in the outline (excluding joint knots) is replaced by a small arc.  The arc radius is proportional to `soft_corners × thickness`, clamped to 40% of each adjacent segment so short segments are never over-consumed.

---

## Stage 3 — SVG output (`elementToSvgPath`)

The outline `Element` tree is walked recursively.  Each `Curve` is solved (DactylSpline or Spiro depending on axes) and emitted as an SVG `<path d="…">`.  `Dot` nodes become small filled `<circle>` elements.  Multiple sub-paths within a glyph (e.g. the dot and body of `!`) are emitted as separate paths sharing the same colour fill.

---

## Key types

| Type | Where | Purpose |
|------|-------|---------|
| `Element` | `GeneratorTypes.fs` | Union: `Curve \| Dot \| EList \| Space \| Glyph` |
| `Knot` | `GeneratorTypes.fs` | One point on a `Curve`, with `SpiroPointType` and optional `th_in`/`th_out` |
| `FontMetrics` | `GeneratorTypes.fs` | Computed guide coordinates (L/R/C/T/X/H/B/D) from axes |
| `Axes` | `Axes.fs` | All font parameters; `DefaultAxes` holds sensible defaults |
| `Font` | `Font.fs` | Stateful renderer; instantiated once per axes combination |

---

## Curve solvers

Three curve solvers are available and selectable in the UI:

### DactylSpline (default) — `src/generator/DactylSpline.fs`
Fits Euler-spiral (clothoid) segments using a Nelder-Mead numerical optimiser (fmin submodule at `web/src/lib/fmin/`).

| | |
|--|--|
| **Pros** | Handles all mixed line/curve/corner topologies; supports fitted coordinates (auto-cardinal tangents); robust on degenerate loops (e.g. three-point closed paths); fully integrated with Dactyl's glyph-string language |
| **Cons** | Numerical optimiser can be slow on many-knot paths; result quality depends on iteration limit (`max_spline_iter` axis); not guaranteed to converge to global optimum |

### Spline2 — `src/spline-research/`
F# port of Raph Levien's [spline-research](https://github.com/raphlinus/spline-research) algorithm (2018), described in his [blog post](https://raphlinus.github.io/curves/2018/12/21/new-spline.html).

| | |
|--|--|
| **Pros** | Elegant closed-form Euler-spiral fit; direct per-knot tangent control (no numerical search); produces very smooth curves with minimal input |
| **Cons** | Narrower topology support than DactylSpline; less integrated with Dactyl's fitted-coordinate and corner features |

### Spiro (legacy) — `src/SpiroFs/`
F# port of Wiesław Šoltés's [C# port](https://github.com/wieslawsoltes/SpiroNet) of Raph Levien's original [Spiro curves](https://www.levien.com/spiro/).

| | |
|--|--|
| **Pros** | Historically proven in FontForge; produces characteristically smooth, calligraphic curves; supported in standard font tooling |
| **Cons** | Matrix solver can fail or produce artefacts on tightly packed closed loops with only three points; no tangent handles; superseded by Spline2 and DactylSpline |

---

## SplineEditor tab (`web/src/SplineEditor.jsx`)

The **Splines** tab provides an interactive canvas for building `DactylSpline` control-point sequences without writing glyph strings.  Features:

- Add, drag, and delete knots on a canvas with guide lines.
- Set each knot's type: Corner, Smooth, Line-Curve, or Curve-Line.
- Set explicit incoming/outgoing tangent angles via cardinal preset buttons or a degree input.
- Toggle open/closed path and see the DactylSpline solve update in real time.
- The **Spline Grid** tab (`SplineGrid.jsx`) shows the same knot sequence rendered across a grid of axis values.

The editor uses `solveSplineEditor` from `Api.fs` (via `worker.js`) to run the Nelder-Mead solver and returns SVG path data for the canvas overlay.

---

## OTF font export (`web/src/fontExport.js`)

When the user downloads a font, `buildFontDataUrl` converts the SVG glyph data to an OTF binary via `opentype.js`.  Because `Font.fs` strokes each curve as an independent closed ribbon, intersecting strokes (e.g. the bowl and stem of `b`) produce overlapping contours.  OTF/CFF requires non-self-intersecting outlines, so `fontExport.js` runs a paper.js boolean union:

1. **Single unite** (`CompoundPath.unite()`): correct and fast for most glyphs.
2. **Iterative unite**: each contour is self-resolved, then accumulated pairwise.  Needed when three or more strokes overlap (e.g. the middle bar of `B`), which can produce winding number 3 that paper.js's single-pass operator misresolves.
3. **Fallback**: raw contours are used if both strategies produce an output that does not match the expected fill area (checked by sampling just inside each contour edge).
