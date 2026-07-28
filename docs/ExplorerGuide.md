# Dactyl Live Explorer — User Guide

The [Dactyl Live Explorer](https://terryspitz.github.io/dactyl-font) lets you design and preview Dactyl fonts interactively in the browser, download a custom OTF font, and inspect the underlying curve mathematics.

---

## Layout

```
┌──────────────┬──────────────────────────────────────────┐
│              │  [Font][Glyphs][Tweens][Visual Diffs]    │
│   Sidebar    │  [Splines][Spline Grid][Proofs][Grow] [⬇]│
│  (Controls)  │                                           │
│              │              Preview area                 │
│              │                                           │
└──────────────┴──────────────────────────────────────────┘
```

The **sidebar** on the left holds all font axis controls.  The **top bar** holds the tab row and per-tab actions.  The **preview area** fills the rest of the page.

---

## Sidebar

### Toolbar icons
| Icon | Action |
|------|--------|
| ↺ Reset | Restore all axes to their default values (also clears per-glyph randomisation) |
| 🎲 Randomize | Randomise all non-experimental axes |
| 📖 Documentation | Open the README in a new tab |

### Axis categories
Controls are grouped into collapsible sections.  Click a section header to expand or collapse it.

| Category | Icon | What it contains |
|----------|------|-----------------|
| **Backbone** | straighten | Overall glyph proportions: width, height, x-height, italic, monospace, tracking, leading, roundedness |
| **Outline** | brush | Stroke appearance: thickness, contrast, soft_corners, axis_align_caps, outline toggle, filled toggle, smooth toggle |
| **Artistic** | palette | Decorative features: end_bulb, flare, serif, stroked (backscratch), scratches, and the broad-nib / brush axes nib, nib_angle, taper, taper_end, wobble, roughness, mobius (these auto-use the arc-length sampled outline path) |
| **Experimental** | science | Advanced solver settings: dactyl_spline, spline2, constraints, constant_offset, max_spline_iter, flatness, end_flatness |
| **Debug** | bug | Diagnostic overlays: show_knots, show_tangents, joints, clip_rect, debug |

*Experimental and Debug sections start collapsed.*

Each axis shows a labelled slider (for numeric ranges) or a toggle switch (for booleans), with the current value displayed to the right of sliders.

---

## Tabs

### Font
Renders the full character set (or whatever text is in the text box) using the current axis values.  Supports zoom via the +/−/↺ buttons or Ctrl+scroll.

The **shuffle button** (🔀) in the top-right randomises *every glyph independently* — each character gets its own axis values, following the same rules as the sidebar 🎲 button but nudged around the current axes rather than the defaults (so the sliders still drive the overall look; `leading` is left alone as it is a line property, not a glyph one).

The result is **stable**: clicking the button rolls a single seed, and every character's settings are derived from that seed plus its code point.  So a given letter looks the same everywhere it appears, and the variant font survives re-renders, sidebar tweaks and tab switches.  It also feeds the OTF export, the Proofs tab and the Visual Diffs font comparison.  Click again for a new set; the ✕ next to it (or the sidebar ↺ Reset) turns it off.

The **download button** (⬇) in the top-right exports a custom OTF font file built from the current axes.  With per-glyph randomisation on, the seed goes in the font's style name and filename (`Dactyl-Random216973057.otf`) — a random font isn't described by its axes, so the seed is what makes a saved file traceable back to the settings that produced it.

**Saving the typed string as a picture.** On the canvas, next to the zoom buttons, are a **copy** (⧉) and a **download** (⬇) button, the same pair the Grow tab has:

| Button | What it does |
|--------|-------------|
| ⧉ Copy | Rasterises the current string to a transparent PNG and puts it on the clipboard |
| ⬇ Download | Saves a transparent PNG; the caret beside it offers PNG or SVG |

Both re-render the string tightly cropped (the on-screen preview uses a fixed large viewBox, which would export as mostly empty space), and both carry per-glyph randomisation through, so what you save is what you see.  Filenames are built from the text and the seed, e.g. `dactyl-Hamburgefonstiv-random216973057.png`.

### Glyphs
Renders individual glyphs and shows the underlying curve geometry.  Most useful for authoring new glyph string definitions.

**Inputs:**
- **Characters textarea** — which characters to display.
- **Glyph Definitions textarea** — editable glyph string definitions in the [DactylGlyph language](DactylGlyphs.md).  Changes update the preview in real time; the last value is saved in `localStorage`.

**Legend (floating, draggable):**
| Checkbox | Colour | What it shows |
|----------|--------|---------------|
| Spiro | blue | Spiro-solved curves |
| Spline2 | green | Raph Levien's spline-research curves |
| DactylSpline | orange | Dactyl's own Euler-spiral solver |
| Comb | outline | Curvature comb along each Bézier segment |
| Tangents | red | Incoming/outgoing tangent handles at each knot |
| Guides | grey | Horizontal guide lines (T, X, H, B, D) |
| Labels | abc | Point coordinate labels |
| Knots | circles | Solved control-point positions |
| Filled | — | Fill the outline instead of stroking it |

**Key (below the definition textarea):**
```
y: (t)op  (x)-height  (h)alf  (b)ottom  (d)escender  (o)ffset in  (e)xtended out
x: (l)eft  (c)enter  (r)ight  (w)ide   Solo point → dot
Dirs: N S E W    Lines: (-) straight  (~) curve    Brackets = fit this coordinate
Repeats average coordinates (e.g. "bt"="h"); a digit repeats the letter before it, so "b2t"="bbt" and "r4c"="rrrrc"
```

### Tweens
Shows how a single glyph (default: `a`) changes as each axis is swept across its full range.  Each axis produces a row of thumbnail previews.

Use the `?tween=<axis_name>` URL parameter (see below) to filter to a single axis row.

Axes `tracking`, `leading`, and `debug` are excluded from this view.

### Visual Diffs
Side-by-side comparison of the full character set under two different axis values.

**Controls in the top bar:**
- **Diff axis** drop-down — choose any axis or the special "Spline engine (old vs new)" option.
- **A / B** inputs — for range axes, enter the two values to compare.
- **⇄ Swap** button — swap A and B values.

### Splines
Interactive DactylSpline editor.  Build a control-point sequence by clicking the canvas, and see the solved curve update live.

- **Point types:** Corner, Smooth, Line-Curve, Curve-Line (set per knot).
- **Tangent handles:** drag or set via cardinal direction presets (→ ↑ ← ↓ and diagonals) or a degree input field.
- **Open / closed** toggle for the path.
- **Zoom:** Ctrl+scroll or the zoom buttons.

### Spline Grid
Shows the current Spline editor's control points solved across a grid of axis configurations, useful for checking how a curve shape holds up under different thickness / roundedness / etc.

### Proofs
Renders the font using professionally designed proof texts (sourced from [typography.com](https://www.typography.com/blog/text-for-proofing-fonts)).

**Proof chips in the top bar:**
| Chip | Content |
|------|---------|
| Lowercase | Lowercase letter frequency proof |
| Uppercase | Uppercase letter frequency proof |
| Alphabet | Full character set (`allChars`) |
| Classic ↺ | Random excerpt from a classic book (randomly selected each click) |

The Proofs tab renders using a live CSS `@font-face` built from the current axes, so it reflects the real typeset appearance (not SVG outlines).

### Grow
Grows the letterforms out of the current backbones — an experimental generative
mode inspired by Namco's *Techno Drive* (1998) logotype.  Instead of offsetting
each spine by a fixed thickness, every stroke swells into the surrounding
whitespace until the channel between it and the nearest *opposing* stroke
narrows to a constant gap.  Counters (the holes in `a`, `e`, `o`) stay open
because the opposing stroke pushes back, while joints (`n`, `e`) don't pinch.
All the usual axes still apply, since growth starts from the same backbones.

**Controls in the top bar:**
| Control | Effect |
|---------|--------|
| `grow` | 0 = classic constant offset … 1 = full space-filling bulge |
| `gap` | Whitespace channel preserved between opposing strokes |
| `fuse` | 0 = letters stay separate … 1 = neighbouring glyphs melt into a single logotype (the gap is relaxed and overshot only between different glyphs, so counters inside a letter stay open) |
| `warp` | 0 = clean edges … 1 = strong organic wobble; domain-warps the grown field with seeded value noise for a hand-drawn / molten look |
| `layers` | Emit nested keyline bands (near-white core → light blue → dark blue → black keyline) that fuse between glyphs for a Y2K logotype look |
| `animate` | Ramp the growth up and down so the letters visibly grow (WebGL only) |

**Save controls** (top bar, right): a **copy** icon copies the result as a
transparent PNG to the clipboard, and a **download** icon saves a PNG by
default — its caret dropdown offers PNG (transparent, high-res) or SVG (vector).

The preview renders on the GPU where WebGL2 is available: the worker computes a
distance field once per text/axes change and a fragment shader thresholds it,
so dragging `grow`/`gap`/`fuse`/`warp`/`layers` never re-runs the worker.  Without WebGL2 it
falls back to a worker-rendered SVG.  Multi-line text is supported; a
determinate progress bar shows while the field is (re)built.

---

## URL parameters

All tabs and settings are bookmarkable via URL parameters.

| Parameter | Values | Effect |
|-----------|--------|--------|
| `?view=` | `font` `glyphs` `tweens` `visualDiffs` `splines` `splineGrid` `proofs` `grow` | Open the named tab on load |
| `?zoom=` | e.g. `0.85` | Set initial zoom level for all tabs |
| `?proof=` | `lowercase` `uppercase` `alphabet` `classic` | Select a proof preset |
| `?book=` | integer index | Pre-select a specific classic book |
| `?tween=` | axis name, e.g. `thickness` | Show only that axis row in Tweens |
| `?diffAxis=` | axis name or `spline_engine` | Pre-set the Visual Diffs axis |
| `?diffA=`, `?diffB=` | numbers | Pre-set the A and B diff values |
