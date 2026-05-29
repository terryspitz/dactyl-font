import { describe, it, expect } from 'vitest'
import { parseSvgPath, unionPath } from './fontExport'

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
})
