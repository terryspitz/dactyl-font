import { describe, it, expect } from 'vitest'
import {
  mulberry32,
  glyphSeedFor,
  randomizeAxes,
  glyphAxes,
  buildGlyphAxes,
  buildPerGlyphTextAxes,
  PER_GLYPH_SKIPPED_AXES,
  RANDOMIZE_PROBABILITY,
} from './glyphRandom'

// Stand-in for the Fable-generated controlDefinitions.
const controls = [
  { name: 'weight', type_: 'range', min: 1, max: 100, category: 'basic' },
  { name: 'roundedness', type_: 'range', min: 0, max: 300, category: 'basic' },
  { name: 'leading', type_: 'range', min: 0, max: 200, category: 'basic' },
  { name: 'slant', type_: 'range', min: 0, max: 1, category: 'artistic' },
  { name: 'serif', type_: 'range', min: 0, max: 70, category: 'artistic' },
  { name: 'stroked', type_: 'checkbox', min: 0, max: 1, category: 'artistic' },
  { name: 'wild', type_: 'range', min: 0, max: 1, category: 'experimental' },
  { name: 'show_knots', type_: 'checkbox', min: 0, max: 1, category: 'debug' },
]

const axes = {
  weight: 30, roundedness: 100, leading: 50, slant: 0, serif: 0,
  stroked: false, wild: 0.5, show_knots: true,
}
const defaults = { ...axes, wild: 0, show_knots: false }

describe('mulberry32', () => {
  it('is deterministic for a given seed', () => {
    const a = mulberry32(12345)
    const b = mulberry32(12345)
    expect([a(), a(), a()]).toEqual([b(), b(), b()])
  })

  it('produces different streams for different seeds', () => {
    expect(mulberry32(1)()).not.toBe(mulberry32(2)())
  })

  it('stays within [0, 1)', () => {
    const r = mulberry32(7)
    for (let i = 0; i < 500; i++) {
      const v = r()
      expect(v).toBeGreaterThanOrEqual(0)
      expect(v).toBeLessThan(1)
    }
  })
})

describe('glyphSeedFor', () => {
  it('separates adjacent code points', () => {
    const seeds = 'abcdefghijklmnopqrstuvwxyz'
      .split('')
      .map(c => glyphSeedFor(42, c.codePointAt(0)))
    expect(new Set(seeds).size).toBe(26)
  })

  it('separates adjacent seeds for the same character', () => {
    const a = glyphSeedFor(1, 97)
    const b = glyphSeedFor(2, 97)
    expect(a).not.toBe(b)
  })

  it('defaults occurrence to 0, matching the plain (seed, codePoint) form', () => {
    expect(glyphSeedFor(1, 97, 0)).toBe(glyphSeedFor(1, 97))
  })

  it('separates occurrences of the same character', () => {
    const seeds = new Set(Array.from({ length: 10 }, (_, n) => glyphSeedFor(1, 97, n)))
    expect(seeds.size).toBe(10)
  })
})

describe('randomizeAxes', () => {
  it('never touches experimental or debug axes', () => {
    const out = randomizeAxes(axes, defaults, controls, mulberry32(3))
    expect(out.wild).toBe(axes.wild)
    expect(out.show_knots).toBe(axes.show_knots)
  })

  it('keeps range values inside their control bounds', () => {
    for (let seed = 0; seed < 200; seed++) {
      const out = randomizeAxes(axes, defaults, controls, mulberry32(seed))
      for (const c of controls) {
        if (c.type_ !== 'range' || c.category === 'experimental' || c.category === 'debug') continue
        expect(out[c.name]).toBeGreaterThanOrEqual(c.min)
        expect(out[c.name]).toBeLessThanOrEqual(c.max)
      }
    }
  })

  it('resets untouched axes to the centre, so repeat rolls do not compound', () => {
    // A roll from a drifted starting point lands in the same place as a roll
    // from the centre, because every eligible axis is re-seeded from `centre`.
    const drifted = { ...axes, weight: 95, serif: 68, slant: 0.9 }
    const a = randomizeAxes(drifted, defaults, controls, mulberry32(11))
    const b = randomizeAxes(defaults, defaults, controls, mulberry32(11))
    for (const c of controls) {
      if (c.category === 'experimental' || c.category === 'debug') continue
      expect(a[c.name]).toBe(b[c.name])
    }
  })

  it('honours skippedAxes', () => {
    for (let seed = 0; seed < 50; seed++) {
      const out = randomizeAxes(axes, axes, controls, mulberry32(seed), ['leading'])
      expect(out.leading).toBe(axes.leading)
    }
  })

  it('does at least sometimes change something', () => {
    const changed = Array.from({ length: 20 }, (_, s) =>
      randomizeAxes(axes, defaults, controls, mulberry32(s))
    ).some(out => out.weight !== defaults.weight || out.slant !== defaults.slant)
    expect(changed).toBe(true)
  })

})

describe('glyphAxes', () => {
  it('is stable for the same (char, seed)', () => {
    expect(glyphAxes('a', 99, axes, controls)).toEqual(glyphAxes('a', 99, axes, controls))
  })

  it('differs between characters under one seed', () => {
    const letters = 'abcdefghijklmnopqrstuvwxyz'.split('')
    const rendered = letters.map(c => JSON.stringify(glyphAxes(c, 99, axes, controls)))
    // not asking for all-distinct (rolls can coincide), just real variety
    expect(new Set(rendered).size).toBeGreaterThan(letters.length / 2)
  })

  it('gives repeated occurrences of one character real variety, not a faint nudge', () => {
    // Regression guard: this used to reuse a much gentler tuning that mostly
    // just tracked the current sidebar value, giving repeated/adjacent
    // characters barely visible differences.
    const n = 300
    const occurrences = Array.from({ length: n }, (_, i) => glyphAxes('5', 1, axes, controls, i))
    const weightRange = Math.max(...occurrences.map(a => a.weight)) - Math.min(...occurrences.map(a => a.weight))
    const fullWeightRange = controls.find(c => c.name === 'weight').max - controls.find(c => c.name === 'weight').min
    // A handful of samples should already cover a large chunk of the full range.
    expect(weightRange).toBeGreaterThan(fullWeightRange * 0.5)

    // axes.stroked is false, so the expected fraction landing true is
    // roughly probability * 0.5 (touched half the time it's a 50/50 flip).
    const strokedFraction = occurrences.filter(a => a.stroked).length / n
    const expectedFraction = RANDOMIZE_PROBABILITY * 0.5
    expect(strokedFraction).toBeGreaterThan(expectedFraction - 0.15)
    expect(strokedFraction).toBeLessThan(expectedFraction + 0.15)
  })

  it('changes when the seed changes', () => {
    const before = 'abcdefghij'.split('').map(c => JSON.stringify(glyphAxes(c, 1, axes, controls)))
    const after = 'abcdefghij'.split('').map(c => JSON.stringify(glyphAxes(c, 2, axes, controls)))
    expect(after).not.toEqual(before)
  })

  it('centres on the current axes, so sidebar changes still show through', () => {
    // `leading` is skipped, so it must track whatever the sidebar says.
    expect(PER_GLYPH_SKIPPED_AXES).toContain('leading')
    const out = glyphAxes('a', 5, { ...axes, leading: 123 }, controls)
    expect(out.leading).toBe(123)
  })
})

describe('buildGlyphAxes', () => {
  it('collapses duplicates and drops newlines', () => {
    const { chars, axesList } = buildGlyphAxes('aab\nba\r', 7, axes, controls)
    expect(chars).toBe('ab')
    expect(axesList).toHaveLength(2)
  })

  it('matches glyphAxes entry by entry', () => {
    const { chars, axesList } = buildGlyphAxes('xyz', 7, axes, controls)
    chars.split('').forEach((c, i) => {
      expect(axesList[i]).toEqual(glyphAxes(c, 7, axes, controls))
    })
  })

  it('gives the same result for the same seed regardless of input order', () => {
    const a = buildGlyphAxes('abc', 7, axes, controls)
    const b = buildGlyphAxes('cba', 7, axes, controls)
    const byChar = o => Object.fromEntries(o.chars.split('').map((c, i) => [c, o.axesList[i]]))
    expect(byChar(a)).toEqual(byChar(b))
  })
})

describe('buildPerGlyphTextAxes', () => {
  it('keeps repeated characters as separate, potentially different entries', () => {
    const axesList = buildPerGlyphTextAxes('555555', 7, axes, controls)
    expect(axesList).toHaveLength(6)
    // not asking for all-distinct (rolls can coincide), just real variety
    expect(new Set(axesList.map(a => JSON.stringify(a))).size).toBeGreaterThan(1)
  })

  it('gives each occurrence the same axes as glyphAxes with that occurrence index', () => {
    const axesList = buildPerGlyphTextAxes('555555', 7, axes, controls)
    axesList.forEach((a, i) => {
      expect(a).toEqual(glyphAxes('5', 7, axes, controls, i))
    })
  })

  it('drops newlines but keeps position alignment otherwise', () => {
    const axesList = buildPerGlyphTextAxes('ab\ncd', 7, axes, controls)
    expect(axesList).toHaveLength(4)
    expect(axesList[0]).toEqual(glyphAxes('a', 7, axes, controls, 0))
    expect(axesList[1]).toEqual(glyphAxes('b', 7, axes, controls, 0))
    expect(axesList[2]).toEqual(glyphAxes('c', 7, axes, controls, 0))
    expect(axesList[3]).toEqual(glyphAxes('d', 7, axes, controls, 0))
  })

  it('is stable for the same seed and text', () => {
    expect(buildPerGlyphTextAxes('555555', 7, axes, controls))
      .toEqual(buildPerGlyphTextAxes('555555', 7, axes, controls))
  })
})
