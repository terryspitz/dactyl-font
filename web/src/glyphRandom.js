// Axis randomisation, shared by the sidebar "Randomize" button and the Font
// tab's "randomise every glyph" mode.
//
// Per-glyph mode is driven by a single integer seed. There are two flavours:
//
//  - Per-character (buildGlyphAxes): axes are derived from (seed, code point)
//    alone, so every occurrence of a letter looks the same. Required for OTF
//    export and the embedded-font proof preview, where a code point can only
//    have one glyph shape.
//  - Per-occurrence (buildPerGlyphTextAxes): axes are derived from (seed, code
//    point, nth occurrence of that character so far), so a repeated run like
//    "555555" renders with a different roll per character. Only valid for
//    direct SVG rendering (no real font involved), and only for the specific
//    text being drawn — the nth occurrence shifts if an earlier occurrence of
//    the same character is inserted/removed, though unrelated edits don't
//    disturb it.
//
// Either way, re-renders, tab switches and sidebar tweaks reproduce the same
// glyphs; a new set only appears when a new seed is rolled (or randomisation
// is reset).

// Only touch a fraction of axes, and bias sampled values toward the centre
// (nudge, don't reroll) so extreme/rare effects don't stack up. Used by the
// sidebar "Randomize" button, which is picking one overall family look.
export const RANDOMIZE_PROBABILITY = 0.35
export const RANDOMIZE_SPREAD = 0.3

// Per-glyph mode wants each character to read as plainly, individually
// different, not a gentle nudge to the family look — so it touches axes more
// often and with a wider spread than the sidebar button. It keeps the same
// centre-biased (triangular) distribution rather than sampling uniformly:
// with ~15 eligible axes, a high enough touch probability already means most
// glyphs get several axes changed at once, and letting *all* of those land
// near the extremes of their range simultaneously (rather than mostly-near-
// centre with occasional bigger swings) reliably produces broken/illegible
// glyphs — self-intersecting outlines the spline solver can't make sense of.
export const PER_GLYPH_RANDOMIZE_PROBABILITY = 0.55
export const PER_GLYPH_RANDOMIZE_SPREAD = 0.45

// Categories never randomised: experimental axes are half-finished and debug
// axes are view options, not design choices.
const SKIPPED_CATEGORIES = ['experimental', 'debug']

// Line spacing is a paragraph property, not a glyph one — varying it per glyph
// would just shunt whole lines around, so per-glyph mode leaves it alone.
export const PER_GLYPH_SKIPPED_AXES = ['leading']

/// Small, fast, seedable PRNG.  Returns a function yielding floats in [0, 1).
export function mulberry32(seed) {
  let a = seed >>> 0
  return function () {
    a = (a + 0x6d2b79f5) | 0
    let t = Math.imul(a ^ (a >>> 15), 1 | a)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

/// Mix a seed with a character code (and, for per-occurrence mode, which
/// occurrence of that character this is) into a well-distributed 32-bit PRNG
/// seed, so adjacent code points (e.g. 'a' and 'b') — and repeated instances
/// of the same code point — get unrelated settings. `occurrence` defaults to
/// 0, which reproduces the plain per-character seed exactly.
export function glyphSeedFor(seed, codePoint, occurrence = 0) {
  let h = ((seed | 0) ^ 0x9e3779b9) >>> 0
  h = Math.imul(h ^ codePoint, 0x85ebca6b)
  h ^= h >>> 13
  h = Math.imul(h ^ occurrence, 0xc2b2ae35)
  return (h ^ (h >>> 16)) >>> 0
}

/// Randomise a set of axes.
///
/// `base`     axes to copy untouched values (skipped categories/axes) from.
/// `centre`   axes that randomised values are sampled around, and that
///            not-selected-this-roll axes are set to.  The sidebar button
///            passes the *defaults* here (so repeated clicks don't compound);
///            per-glyph mode passes the current axes (so the sliders still
///            drive the overall look).
/// `rand`     () => [0, 1) source, e.g. Math.random or mulberry32(seed).
/// `options.probability` chance [0, 1) each axis is touched at all (default
///            RANDOMIZE_PROBABILITY).
/// `options.spread`      offset amplitude as a fraction of the axis's full
///            range, for touched continuous axes — sampled with a triangular
///            distribution centred on `centre` either way, so most touched
///            draws are a moderate nudge and only occasionally a big swing
///            (default RANDOMIZE_SPREAD). Checkboxes have no "spread" to
///            widen; a touched checkbox is always a 50/50 coin-flip, so
///            `probability` alone controls how often they leave `centre`.
export function randomizeAxes(base, centre, controls, rand, skippedAxes = [], options = {}) {
  const { probability = RANDOMIZE_PROBABILITY, spread = RANDOMIZE_SPREAD } = options
  const next = { ...base }
  controls.forEach(ctrl => {
    if (SKIPPED_CATEGORIES.includes(ctrl.category)) return
    if (skippedAxes.includes(ctrl.name)) return

    next[ctrl.name] = centre[ctrl.name]
    if (rand() > probability) return

    if (ctrl.type_ === 'checkbox') {
      next[ctrl.name] = rand() > 0.5
    } else {
      const c = centre[ctrl.name] ?? (ctrl.min + ctrl.max) / 2
      const range = ctrl.max - ctrl.min
      // triangular distribution centred on 0: most draws land near `c`
      const offset = (rand() - rand()) * range * spread
      next[ctrl.name] = Math.min(ctrl.max, Math.max(ctrl.min, c + offset))
    }
  })
  return next
}

/// Axes for a single character under the per-glyph random seed. `occurrence`
/// distinguishes repeats of the same character within one render (see
/// buildPerGlyphTextAxes); omit it for the stable per-character roll used by
/// font export.
export function glyphAxes(char, seed, axes, controls, occurrence = 0) {
  const rand = mulberry32(glyphSeedFor(seed, char.codePointAt(0), occurrence))
  return randomizeAxes(axes, axes, controls, rand, PER_GLYPH_SKIPPED_AXES, {
    probability: PER_GLYPH_RANDOMIZE_PROBABILITY,
    spread: PER_GLYPH_RANDOMIZE_SPREAD,
  })
}

/// Build the parallel (chars, axesList) arrays the F# API expects for
/// per-character rendering (OTF export, embedded-font proof preview).
/// Duplicate characters collapse to one entry, so a letter always renders
/// identically — required since a real font has one glyph per code point.
export function buildGlyphAxes(chars, seed, axes, controls) {
  const distinct = [...new Set(Array.from(chars))].filter(c => c !== '\n' && c !== '\r')
  return {
    chars: distinct.join(''),
    axesList: distinct.map(c => glyphAxes(c, seed, axes, controls)),
  }
}

/// Build a positional axesList for direct SVG rendering of `text`: one entry
/// per character, in order, aligned with how the F# side splits the text into
/// lines (text.Replace("\r\n","\n").Split('\n'), lines concatenated back to
/// back). Unlike buildGlyphAxes this is *not* deduplicated by character — the
/// nth occurrence of a character gets its own roll — so repeated characters
/// like "555555" can each render differently.
export function buildPerGlyphTextAxes(text, seed, axes, controls) {
  const flat = text.replace(/\r\n/g, '\n').split('\n').join('')
  const occurrence = new Map()
  return Array.from(flat).map(c => {
    const n = occurrence.get(c) ?? 0
    occurrence.set(c, n + 1)
    return glyphAxes(c, seed, axes, controls, n)
  })
}

/// A fresh seed for the "randomise every glyph" button.
export function newGlyphSeed() {
  return Math.floor(Math.random() * 0x7fffffff)
}
