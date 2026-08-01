import { describe, it, expect } from 'vitest'
import { defaultAxes } from './lib/fable/Api.js'
import { generateTextureSvg } from './textureSvg'

// Exercises the full Texture-mode back end (the same path the 'texture'
// worker message runs): real glyph backbones → solved spines → mask field →
// pattern algorithm → SVG.  Deterministic, no browser/GPU needed.  RD steps
// are kept low here (vs the UI default of 3000) purely for test speed —
// texture.test.js covers the algorithms themselves in more depth.

const axes = { ...defaultAxes, dactyl_spline: true }

describe('generateTextureSvg', () => {
    it('returns empty string for blank text', () => {
        expect(generateTextureSvg('', axes, { style: 'rd' })).toBe('')
        expect(generateTextureSvg('   ', axes, { style: 'maze' })).toBe('')
    })

    it('rd style produces a filled pattern over the classic backbone', () => {
        const svg = generateTextureSvg('o', axes, { style: 'rd', steps: 500, backbone: true })
        expect(svg.startsWith('<svg')).toBe(true)
        expect(svg).toContain('viewBox=')
        // Two filled paths: the backbone silhouette, then the pattern on top.
        expect((svg.match(/<path/g) || []).length).toBe(2)
        expect(svg).toContain('fill-rule="evenodd"')
    }, 30000)

    it('rd style omits the backbone when disabled', () => {
        const svg = generateTextureSvg('o', axes, { style: 'rd', steps: 500, backbone: false })
        expect((svg.match(/<path/g) || []).length).toBe(1)
    }, 30000)

    it('maze style emits a single stroked wall path', () => {
        const svg = generateTextureSvg('o', axes, { style: 'maze', backbone: false })
        expect(svg.startsWith('<svg')).toBe(true)
        expect(svg).toContain('stroke=')
        expect(svg).toContain('fill="none"')
    }, 20000)

    it('circuit style emits trace paths and pad rects', () => {
        const svg = generateTextureSvg('o', axes, { style: 'circuit', backbone: false })
        expect(svg.startsWith('<svg')).toBe(true)
        expect(svg).toContain('<path')
        expect(svg).toContain('<rect')
    }, 20000)

    it('is deterministic for a given seed (all three styles)', () => {
        for (const style of ['rd', 'maze', 'circuit']) {
            const params = { style, seed: 3, steps: 500 }
            const a = generateTextureSvg('o', axes, params)
            const b = generateTextureSvg('o', axes, params)
            expect(a).toBe(b)
        }
    }, 30000)

    it('respects custom colours', () => {
        const svg = generateTextureSvg('o', axes, { style: 'rd', steps: 500, color: '#123456', backbone: false })
        expect(svg).toContain('#123456')
    }, 20000)
})
