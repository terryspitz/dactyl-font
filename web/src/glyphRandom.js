// Axis randomisation, shared by the sidebar "Randomize" button and the Font
// tab's "randomise every glyph" mode.
//
// Per-glyph mode is driven by a single integer seed: each character's axes are
// derived deterministically from (seed, code point).  That keeps the variant
// font *stable* — re-renders, tab switches, sidebar tweaks and OTF export all
// reproduce the same glyphs, and every occurrence of a letter looks the same.
// A new set only appears when a new seed is rolled (or randomisation is reset).

// Only touch a fraction of axes, and bias sampled values toward the centre
// (nudge, don't reroll) so extreme/rare effects don't stack up.
export const RANDOMIZE_PROBABILITY = 0.35
export const RANDOMIZE_SPREAD = 0.3

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

/// Mix a seed with a character code into a well-distributed 32-bit PRNG seed,
/// so adjacent code points (e.g. 'a' and 'b') get unrelated settings.
export function glyphSeedFor(seed, codePoint) {
  let h = ((seed | 0) ^ 0x9e3779b9) >>> 0
  h = Math.imul(h ^ codePoint, 0x85ebca6b)
  h ^= h >>> 13
  h = Math.imul(h, 0xc2b2ae35)
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
export function randomizeAxes(base, centre, controls, rand, skippedAxes = []) {
  const next = { ...base }
  controls.forEach(ctrl => {
    if (SKIPPED_CATEGORIES.includes(ctrl.category)) return
    if (skippedAxes.includes(ctrl.name)) return

    next[ctrl.name] = centre[ctrl.name]
    if (rand() > RANDOMIZE_PROBABILITY) return

    if (ctrl.type_ === 'checkbox') {
      next[ctrl.name] = rand() > 0.5
    } else {
      const c = centre[ctrl.name] ?? (ctrl.min + ctrl.max) / 2
      const range = ctrl.max - ctrl.min
      // triangular distribution centred on 0: most draws land near `c`
      const offset = (rand() - rand()) * range * RANDOMIZE_SPREAD
      next[ctrl.name] = Math.min(ctrl.max, Math.max(ctrl.min, c + offset))
    }
  })
  return next
}

/// Axes for a single character under the per-glyph random seed.
export function glyphAxes(char, seed, axes, controls) {
  const rand = mulberry32(glyphSeedFor(seed, char.codePointAt(0)))
  return randomizeAxes(axes, axes, controls, rand, PER_GLYPH_SKIPPED_AXES)
}

/// Build the parallel (chars, axesList) arrays the F# API expects.  Duplicate
/// characters collapse to one entry, so a letter always renders identically.
export function buildGlyphAxes(chars, seed, axes, controls) {
  const distinct = [...new Set(Array.from(chars))].filter(c => c !== '\n' && c !== '\r')
  return {
    chars: distinct.join(''),
    axesList: distinct.map(c => glyphAxes(c, seed, axes, controls)),
  }
}

/// A fresh seed for the "randomise every glyph" button.
export function newGlyphSeed() {
  return Math.floor(Math.random() * 0x7fffffff)
}
