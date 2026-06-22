import { describe, it, expect } from 'vitest'
import opentype from 'opentype.js'
import { buildCompareOverlaySvg } from './fontCompare'

// A tiny synthetic font so charToGlyph / getBoundingBox work without a real file.
function box(w, h) {
  const p = new opentype.Path()
  p.moveTo(0, 0); p.lineTo(w, 0); p.lineTo(w, h); p.lineTo(0, h); p.close()
  return p
}
function makeFont() {
  const glyphs = [
    new opentype.Glyph({ name: '.notdef', advanceWidth: 650, path: new opentype.Path() }),
    new opentype.Glyph({ name: 'H', unicode: 72, advanceWidth: 650, path: box(600, 700) }),
    new opentype.Glyph({ name: 'x', unicode: 120, advanceWidth: 520, path: box(500, 500) }),
  ]
  return new opentype.Font({ familyName: 'T', styleName: 'R', unitsPerEm: 1000, ascender: 800, descender: -200, glyphs })
}

const dactyl = {
  unitsPerEm: 1000,
  glyphs: [
    { unicode: 72, advanceWidth: 700, pathData: 'M 0,0 L 700,0 L 700,700 L 0,700 Z' },
    { unicode: 120, advanceWidth: 600, pathData: 'M 0,0 L 600,0 L 600,500 L 0,500 Z' },
  ],
}

describe('buildCompareOverlaySvg', () => {
  it('emits a well-formed SVG with three columns per glyph', () => {
    const svg = buildCompareOverlaySvg(dactyl, makeFont(), 'Hx', 'cap', 'Test')
    expect(svg.startsWith('<svg')).toBe(true)
    expect(svg.trimEnd().endsWith('</svg>')).toBe(true)
    // 2 chars × (Dactyl + comparison + overlay-red + overlay-blue) = 8 paths
    expect((svg.match(/<path/g) || []).length).toBe(8)
  })

  it('renders for every alignment metric', () => {
    for (const align of ['cap', 'x', 'em']) {
      const svg = buildCompareOverlaySvg(dactyl, makeFont(), 'Hx', align, 'Test')
      expect((svg.match(/<path/g) || []).length).toBe(8)
    }
  })

  it('scales the comparison font so cap-heights match (H box 700 vs 700 → ~1x)', () => {
    // Dactyl H cap = 700, comparison H cap = 700, so the comparison scale is 1.
    const svg = buildCompareOverlaySvg(dactyl, makeFont(), 'H', 'cap', 'Test')
    expect(svg).toContain('scale(1,-1)')
  })

  it('skips glyphs missing from Dactyl without crashing', () => {
    const svg = buildCompareOverlaySvg(dactyl, makeFont(), 'Hz', 'cap', 'Test')
    // 'z' is absent from both → only the H column-set (4) plus nothing for z.
    expect((svg.match(/<path/g) || []).length).toBe(4)
  })
})
