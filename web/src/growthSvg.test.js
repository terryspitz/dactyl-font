import { describe, it, expect } from 'vitest'
import { defaultAxes } from './lib/fable/Api.js'
import { generateGrowthSvg, generateGrowthField } from './growthSvg'
import { DOPP_CAP } from './growth'

// Exercises the full Grow-tab back end (the same path the 'growth' /
// 'growthField' worker messages run): real glyph backbones → solved spines →
// growth field → layered SVG.  Deterministic, no browser/GPU needed.

const axes = { ...defaultAxes, dactyl_spline: true }

describe('generateGrowthSvg', () => {
    it('produces a layered SVG for a word', () => {
        const svg = generateGrowthSvg('oa', axes, { grow: 0.6, gap: 25, layers: true })
        expect(svg.startsWith('<svg')).toBe(true)
        expect(svg).toContain('viewBox=')
        expect(svg).toMatch(/<path d="M[^"]+" fill=/)
        // Four keyline colours in layers mode, ink core (#f4fbff) painted last.
        for (const c of ['#f4fbff', '#7ec4ee', '#1660c8', '#0a0a14']) {
            expect(svg).toContain(c)
        }
        expect(svg.lastIndexOf('#f4fbff')).toBeGreaterThan(svg.lastIndexOf('#0a0a14'))
    }, 30000)

    it('emits a single black layer when layers are off', () => {
        const svg = generateGrowthSvg('o', axes, { grow: 0.5, gap: 25, layers: false })
        expect(svg).toContain('fill="black"')
        expect(svg).not.toContain('#7ec4ee')
    }, 30000)

    it('returns empty string for blank text', () => {
        expect(generateGrowthSvg('', axes, {})).toBe('')
        expect(generateGrowthSvg('   ', axes, {})).toBe('')
    })

    it('grows wider as grow increases', () => {
        // The grown stem is fatter at high grow, so its ink spans a wider
        // x-range (field padding is fixed, so compare the path data, not the viewBox).
        const xrange = (svg) => {
            const xs = [...svg.matchAll(/[ML](-?[\d.]+) /g)].map(m => parseFloat(m[1]))
            return Math.max(...xs) - Math.min(...xs)
        }
        const low = xrange(generateGrowthSvg('l', axes, { grow: 0.1, gap: 25, layers: false }))
        const high = xrange(generateGrowthSvg('l', axes, { grow: 0.9, gap: 25, layers: false }))
        expect(high).toBeGreaterThan(low)
    }, 30000)
})

describe('generateGrowthField', () => {
    it('builds a two-channel field with sane dimensions', () => {
        const f = generateGrowthField('o', axes)
        expect(f).not.toBeNull()
        expect(f.nx).toBeGreaterThan(0)
        expect(f.ny).toBeGreaterThan(0)
        expect(f.rg.length).toBe(f.nx * f.ny * 2)
        expect(f.thickness).toBe(axes.thickness)
        // d1 is 0 on the spine and positive away from it, within the cap.
        let minD1 = Infinity, maxD1 = -Infinity
        for (let k = 0; k < f.nx * f.ny; k++) {
            minD1 = Math.min(minD1, f.rg[k * 2])
            maxD1 = Math.max(maxD1, f.rg[k * 2])
        }
        expect(minD1).toBeLessThan(f.cell) // some cell sits on/near a spine
        expect(maxD1).toBeLessThanOrEqual(DOPP_CAP)
    }, 30000)

    it('returns null for blank text', () => {
        expect(generateGrowthField('', axes)).toBeNull()
    })
})
