import { describe, it, expect } from 'vitest'
import opentype from 'opentype.js'
import { buildCompareOverlaySvg } from './fontCompare'

// A tiny synthetic font so charToGlyph / getBoundingBox work without a real file.
function box(w, h) {
  const p = new opentype.Path()
  p.moveTo(0, 0); p.lineTo(w, 0); p.lineTo(w, h); p.lineTo(0, h); p.close()
  return p
}
function makeFont({ cap = 700, xh = 500, upm = 1000 } = {}) {
  const glyphs = [
    new opentype.Glyph({ name: '.notdef', advanceWidth: 650, path: new opentype.Path() }),
    new opentype.Glyph({ name: 'H', unicode: 72, advanceWidth: 650, path: box(600, cap) }),
    new opentype.Glyph({ name: 'x', unicode: 120, advanceWidth: 520, path: box(500, xh) }),
  ]
  return new opentype.Font({ familyName: 'T', styleName: 'R', unitsPerEm: upm, ascender: 800, descender: -200, glyphs })
}

// Extract the scale factor applied to the comparison font. The comparison
// glyph is the second <g> in the first cell (Dactyl is first).
function comparisonScale(svg) {
  const m = [...svg.matchAll(/scale\(([-\d.]+),/g)]
  // groups: [0]=Dactyl col1, [1]=comparison col2, ...
  return Number(m[1][1])
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

  it('cap-height align: scales comparison by dactylCap / fontCap', () => {
    // Dactyl H cap = 700, comparison H cap = 350 → scale 2.
    const svg = buildCompareOverlaySvg(dactyl, makeFont({ cap: 350 }), 'H', 'cap', 'T')
    expect(comparisonScale(svg)).toBeCloseTo(2, 5)
  })

  it('x-height align: scales comparison by dactylX / fontX', () => {
    // Dactyl x x-height = 500, comparison x x-height = 250 → scale 2.
    const svg = buildCompareOverlaySvg(dactyl, makeFont({ xh: 250 }), 'x', 'x', 'T')
    expect(comparisonScale(svg)).toBeCloseTo(2, 5)
  })

  it('em align: scales comparison by dactylUPM / fontUPM', () => {
    // Dactyl em = 1000, comparison em = 500 → scale 2.
    const svg = buildCompareOverlaySvg(dactyl, makeFont({ upm: 500 }), 'H', 'em', 'T')
    expect(comparisonScale(svg)).toBeCloseTo(2, 5)
  })

  it('Dactyl side is always drawn at native scale (1)', () => {
    const svg = buildCompareOverlaySvg(dactyl, makeFont({ cap: 350 }), 'H', 'cap', 'T')
    const firstScale = Number(svg.match(/scale\(([-\d.]+),/)[1])
    expect(firstScale).toBe(1)
  })

  it('escapes XML-special characters in the comparison label', () => {
    const svg = buildCompareOverlaySvg(dactyl, makeFont(), 'H', 'cap', 'A & B <font>')
    expect(svg).toContain('A &amp; B &lt;font&gt;')
    expect(svg).not.toContain('A & B <font>')
  })
})
