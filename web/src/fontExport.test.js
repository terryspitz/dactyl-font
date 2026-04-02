import { describe, it, expect } from 'vitest'
import { parseSvgPath } from './fontExport'

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
