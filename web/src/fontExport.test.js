import { describe, it, expect } from 'vitest'
import { parseSvgPath, unionPath, buildFont } from './fontExport'

describe('parseSvgPath', () => {
  it('parses M command', () => {
    expect(parseSvgPath('M 100,200')).toEqual([{ type: 'M', x: 100, y: 200 }])
  })

  it('parses L command', () => {
    expect(parseSvgPath('L 300,400')).toEqual([{ type: 'L', x: 300, y: 400 }])
  })

  it('parses C command', () => {
    expect(parseSvgPath('C 10,20 30,40 50,60')).toEqual([
      { type: 'C', x1: 10, y1: 20, x2: 30, y2: 40, x: 50, y: 60 },
    ])
  })

  it('parses Z command', () => {
    expect(parseSvgPath('Z')).toEqual([{ type: 'Z' }])
  })

  it('parses a full closed contour', () => {
    const path = 'M 0,0 L 100,0 C 100,50 50,100 0,100 Z'
    expect(parseSvgPath(path)).toEqual([
      { type: 'M', x: 0, y: 0 },
      { type: 'L', x: 100, y: 0 },
      { type: 'C', x1: 100, y1: 50, x2: 50, y2: 100, x: 0, y: 100 },
      { type: 'Z' },
    ])
  })

  it('parses multiple contours', () => {
    const path = 'M 0,0 L 10,10 Z M 20,20 L 30,30 Z'
    const cmds = parseSvgPath(path)
    expect(cmds).toHaveLength(6)
    expect(cmds[0]).toEqual({ type: 'M', x: 0, y: 0 })
    expect(cmds[3]).toEqual({ type: 'M', x: 20, y: 20 })
  })

  it('handles negative coordinates', () => {
    expect(parseSvgPath('M 100,-200')).toEqual([{ type: 'M', x: 100, y: -200 }])
  })

  it('returns empty array for empty string', () => {
    expect(parseSvgPath('')).toEqual([])
    expect(parseSvgPath(null)).toEqual([])
  })

  it('parses output matching the F# generator format (integer coords)', () => {
    // Matches sprintf "M %.0f,%.0f" and "C %.0f,%.0f %.0f,%.0f %.0f,%.0f"
    const path = 'M 50,0 C 150,0 200,100 200,200 C 200,300 150,400 50,400 Z'
    const cmds = parseSvgPath(path)
    expect(cmds).toHaveLength(4)
    expect(cmds[0]).toEqual({ type: 'M', x: 50, y: 0 })
    expect(cmds[1]).toEqual({ type: 'C', x1: 150, y1: 0, x2: 200, y2: 100, x: 200, y: 200 })
    expect(cmds[3]).toEqual({ type: 'Z' })
  })
})

// Approximate signed area of a contour list via the shoelace formula on the
// on-curve anchor points (good enough to tell solid contours from holes).
function signedArea(cmds) {
  const pts = cmds.filter(c => c.type === 'M' || c.type === 'C').map(c => [c.x, c.y])
  let a = 0
  for (let i = 0; i < pts.length; i++) {
    const [x1, y1] = pts[i]
    const [x2, y2] = pts[(i + 1) % pts.length]
    a += x1 * y2 - x2 * y1
  }
  return a / 2
}

describe('unionPath', () => {
  it('returns empty array for empty input', () => {
    expect(unionPath('')).toEqual([])
    expect(unionPath(null)).toEqual([])
  })

  it('leaves a single non-overlapping contour as one closed path', () => {
    const cmds = unionPath('M 0,0 L 0,100 L 100,100 L 100,0 Z')
    expect(cmds.filter(c => c.type === 'M')).toHaveLength(1)
    expect(cmds.filter(c => c.type === 'Z')).toHaveLength(1)
  })

  it('merges two overlapping ribbons into a single contour', () => {
    // A tall stem and a horizontal bar that overlap in the middle — the kind of
    // join that occurs in 'a', 'b', 'd', 'h', etc.
    const stem = 'M 0,0 L 0,400 L 40,400 L 40,0 Z'
    const bar = 'M 20,180 L 20,220 L 300,220 L 300,180 Z'
    const cmds = unionPath(`${stem} ${bar}`)
    // Overlap removed -> exactly one outer contour, no interior crossing edges.
    expect(cmds.filter(c => c.type === 'M')).toHaveLength(1)
    expect(cmds.filter(c => c.type === 'Z')).toHaveLength(1)
  })

  it('preserves a counter as a separate, opposite-wound contour', () => {
    // Outer square with an inner reversed square (a hole), like the bowl of 'o'.
    const outer = 'M 0,0 L 0,300 L 300,300 L 300,0 Z'
    const hole = 'M 100,100 L 200,100 L 200,200 L 100,200 Z' // opposite winding
    const cmds = unionPath(`${outer} ${hole}`)
    const contours = []
    let cur = []
    for (const c of cmds) {
      cur.push(c)
      if (c.type === 'Z') { contours.push(cur); cur = [] }
    }
    expect(contours).toHaveLength(2)
    // The two contours must wind in opposite directions for the hole to render.
    const areas = contours.map(signedArea)
    expect(Math.sign(areas[0])).not.toBe(Math.sign(areas[1]))
  })

  it('falls back to the raw contours when the boolean op would change the fill', () => {
    // A single self-intersecting contour with winding number 2 in the centre
    // (a "pinwheel" the boolean engine can mis-resolve). Whatever happens, the
    // output must still describe a valid, non-empty, closed outline rather than
    // a dropped or corrupted region — unionPath verifies the fill and falls back
    // to the raw contour when the union diverges.
    const raw = 'M 0,0 L 300,100 L 0,200 L 200,200 L 200,-100 L 100,300 Z'
    const cmds = unionPath(raw)
    expect(cmds.length).toBeGreaterThan(0)
    expect(cmds[0].type).toBe('M')
    expect(cmds[cmds.length - 1].type).toBe('Z')
  })
})

describe('buildFont em normalisation', () => {
  // The generator's em (unitsPerEm) varies with the axes, but CFF/OTF fonts must
  // be exported at unitsPerEm = 1000 or they render blank on Windows (GDI / Font
  // Viewer / Office) and in Adobe apps — see opentype.js issue #115.  buildFont
  // must rescale every coordinate and metric to a fixed 1000-unit em regardless
  // of the generator's em.
  const glyphData = (unitsPerEm) => ({
    unitsPerEm,
    ascender: unitsPerEm * 0.7,
    descender: -unitsPerEm * 0.3,
    glyphs: [{ unicode: 65, advanceWidth: unitsPerEm * 0.4, pathData: 'M 0,0 L 100,0 L 100,200 L 0,200 Z' }],
  })

  it('always exports unitsPerEm = 1000', () => {
    for (const em of [960, 1010, 1150, 2048]) {
      expect(buildFont(glyphData(em)).unitsPerEm).toBe(1000)
    }
  })

  it('scales advance widths and metrics to the 1000-unit em', () => {
    const font = buildFont(glyphData(2048))
    expect(font.ascender).toBe(Math.round(2048 * 0.7 * 1000 / 2048)) // 700
    expect(font.descender).toBe(Math.round(-2048 * 0.3 * 1000 / 2048)) // -300
    const a = font.glyphs.get(font.charToGlyphIndex('A'))
    expect(a.advanceWidth).toBe(Math.round(2048 * 0.4 * 1000 / 2048)) // 400
  })

  it('scales glyph outline coordinates to the 1000-unit em', () => {
    // At em = 2000 the scale is exactly 0.5, so the 200-unit-tall box → 100.
    const font = buildFont(glyphData(2000))
    const a = font.glyphs.get(font.charToGlyphIndex('A'))
    const bb = a.getBoundingBox()
    expect(bb.x2 - bb.x1).toBe(50)  // 100 * 0.5
    expect(bb.y2 - bb.y1).toBe(100) // 200 * 0.5
  })
})

describe('buildFont metadata (fontbakery requirements)', () => {
  // Glyph data including a space (U+0020), so buildFont adds the required
  // no-break space and picks up a real advance width for it.
  const glyphData = {
    unitsPerEm: 1000,
    ascender: 700,
    descender: -300,
    glyphs: [
      { unicode: 32, advanceWidth: 250, pathData: '' },
      { unicode: 65, advanceWidth: 400, pathData: 'M 0,0 L 100,0 L 100,200 L 0,200 Z' },
      { unicode: 66, advanceWidth: 520, pathData: 'M 0,0 L 100,0 L 100,200 L 0,200 Z' },
    ],
  }

  // opentype.js 1.3.4 defaults every unset name field to a single space, which
  // fontbakery flags as empty / trailing-whitespace records.
  it('emits no empty or whitespace-only name records', () => {
    const names = buildFont(glyphData).names
    for (const [key, langs] of Object.entries(names)) {
      for (const value of Object.values(langs)) {
        expect(value, key).toBeTruthy()
        expect(value.trim(), key).not.toBe('')
      }
    }
  })

  it('sets matching version strings and a unique id', () => {
    const names = buildFont(glyphData).names
    expect(names.version.en).toBe('Version 1.000')
    expect(names.uniqueID.en).toContain('1.000')
  })

  it('embeds the OFL copyright with the reserved font name', () => {
    const copyright = buildFont(glyphData).names.copyright.en
    expect(copyright).toContain('Terry Spitz')
    expect(copyright).toContain('Reserved Font Name "Dactyl"')
  })

  it('links the designer URL to the live site and the repo in the copyright', () => {
    const names = buildFont(glyphData).names
    expect(names.designerURL.en).toBe('https://terryspitz.github.io/dactyl-font')
    expect(names.copyright.en).toContain('https://github.com/terryspitz/dactyl-font')
  })

  it('includes a U+00A0 no-break space matching the space advance', () => {
    const font = buildFont(glyphData)
    const space = font.glyphs.get(font.charToGlyphIndex(' '))
    const nbsp = font.glyphs.get(font.charToGlyphIndex(' '))
    expect(nbsp).toBeTruthy()
    expect(nbsp.unicode).toBe(0x00a0)
    expect(nbsp.advanceWidth).toBe(space.advanceWidth)
  })

  it('gives .notdef a real outline and no wider advance than the widest glyph', () => {
    const font = buildFont(glyphData)
    const notdef = font.glyphs.get(0)
    expect(notdef.name).toBe('.notdef')
    expect(notdef.path.commands.length).toBeGreaterThan(0)
    // Widest character glyph here is 'B' at 520; .notdef must not exceed it, or
    // hmtx and hhea.advanceWidthMax disagree.
    expect(notdef.advanceWidth).toBe(520)
  })
})
