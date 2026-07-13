import { describe, it, expect } from 'vitest'
import { growStrokes, contoursToPath, buildGrowthField, DOPP_CAP } from './growth'

const hline = (x0, x1, y) => ({ pts: [[x0, y], [x1, y]], closed: false })

const bbox = (contours) => {
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity
    for (const c of contours) for (const [x, y] of c) {
        if (x < minX) minX = x
        if (x > maxX) maxX = x
        if (y < minY) minY = y
        if (y > maxY) maxY = y
    }
    return { minX, minY, maxX, maxY }
}

describe('growStrokes', () => {
    it('grow=0 reproduces the classic constant offset', () => {
        const g = growStrokes([hline(0, 300, 0)], { thickness: 30, grow: 0, gap: 20, cell: 3, smoothPasses: 0 })
        expect(g.levels[0].contours.length).toBe(1)
        const b = bbox(g.levels[0].contours)
        // Round-capped stroke of half-width 15: [-15, 315] × [-15, 15], within a cell
        expect(b.minX).toBeGreaterThan(-15 - 4)
        expect(b.maxX).toBeLessThan(315 + 4)
        expect(b.minY).toBeGreaterThan(-15 - 4)
        expect(b.maxY).toBeLessThan(15 + 4)
    })

    it('grow=1 inflates an isolated stroke to rMax', () => {
        const g = growStrokes([hline(0, 300, 0)], { thickness: 30, grow: 1, growScale: 100, gap: 20, cell: 3, smoothPasses: 0 })
        const b = bbox(g.levels[0].contours)
        // rMax = 15 + 100 = 115
        expect(b.maxY).toBeGreaterThan(115 - 6)
        expect(b.maxY).toBeLessThan(115 + 6)
    })

    it('two opposing strokes keep the gap channel and stay separate', () => {
        const strokes = [hline(0, 300, 0), hline(0, 300, 120)]
        const g = growStrokes(strokes, { thickness: 30, grow: 1, growScale: 100, gap: 40, cell: 3, smoothPasses: 0 })
        expect(g.levels[0].contours.length).toBe(2)
        // Between the spines (120 apart) each may grow to (120-40)/2 = 40:
        // lower stroke tops out near y=40, upper bottoms out near y=80.
        let lowerTop = -Infinity, upperBottom = Infinity
        for (const c of g.levels[0].contours) {
            const b = bbox([c])
            if (b.minY < 60) lowerTop = Math.max(lowerTop, b.maxY)
            else upperBottom = Math.min(upperBottom, b.minY)
        }
        expect(lowerTop).toBeLessThan(40 + 6)
        expect(upperBottom).toBeGreaterThan(80 - 6)
    })

    it('fuse melts cross-glyph strokes together while keeping them apart at fuse=0', () => {
        // Two vertical bars 120 apart, tagged as different glyphs.
        const strokes = [
            { pts: [[0, 300], [0, -300]], closed: false, glyph: 0 },
            { pts: [[120, 300], [120, -300]], closed: false, glyph: 1 },
        ]
        const base = { thickness: 30, grow: 1, growScale: 100, gap: 40, cell: 3, smoothPasses: 0 }
        // fuse=0: the gap channel survives, so two separate outlines.
        expect(growStrokes(strokes, { ...base, fuse: 0 }).levels[0].contours.length).toBe(2)
        // fuse=1: cross-glyph gap relaxes and overlaps, merging into one.
        expect(growStrokes(strokes, { ...base, fuse: 1 }).levels[0].contours.length).toBe(1)
    })

    it('fuse never collapses a counter within a single glyph', () => {
        // A closed bowl belonging to one glyph: full fuse must keep the hole.
        const sq = { pts: [[0, 0], [240, 0], [240, 240], [0, 240]], closed: true, glyph: 0 }
        const g = growStrokes([sq], { thickness: 30, grow: 1, growScale: 100, gap: 40, fuse: 1, cell: 3, smoothPasses: 0 })
        expect(g.levels[0].contours.length).toBe(2)
    })

    it('a closed bowl keeps its counter open', () => {
        // A closed square "bowl" 240 wide: the counter must survive full growth.
        const sq = { pts: [[0, 0], [240, 0], [240, 240], [0, 240]], closed: true }
        const g = growStrokes([sq], { thickness: 30, grow: 1, growScale: 100, gap: 40, cell: 3, smoothPasses: 0 })
        // Outer boundary + inner hole
        expect(g.levels[0].contours.length).toBe(2)
    })

    it('joined strokes do not pinch at the junction', () => {
        // A T-joint: vertical stem meeting a horizontal bar (connected network).
        const strokes = [hline(0, 300, 300), { pts: [[150, 0], [150, 300]], closed: false }]
        const g = growStrokes(strokes, { thickness: 30, grow: 0.3, gap: 40, cell: 3, smoothPasses: 0 })
        // Connected parts merge into a single outline: no gap channel at the joint.
        expect(g.levels[0].contours.length).toBe(1)
    })

    it('jump-flooded d1 matches brute-force segment distance', () => {
        const strokes = [
            hline(0, 200, 0),
            { pts: [[100, -100], [100, 100]], closed: false },
            { pts: [[300, 0], [400, 0], [400, 100], [300, 100]], closed: true },
        ]
        const cell = 4
        const field = buildGrowthField(strokes, { thickness: 30, cell })
        expect(field).not.toBeNull()

        const segDist = (px, py, x1, y1, x2, y2) => {
            const dx = x2 - x1, dy = y2 - y1
            const t = Math.max(0, Math.min(1, ((px - x1) * dx + (py - y1) * dy) / (dx * dx + dy * dy)))
            return Math.hypot(px - (x1 + t * dx), py - (y1 + t * dy))
        }
        const bruteD1 = (px, py) => {
            let best = Infinity
            for (const s of strokes) {
                const n = s.pts.length
                const segs = s.closed ? n : n - 1
                for (let i = 0; i < segs; i++) {
                    const [x1, y1] = s.pts[i]
                    const [x2, y2] = s.pts[(i + 1) % n]
                    best = Math.min(best, segDist(px, py, x1, y1, x2, y2))
                }
            }
            return best
        }

        let maxErr = 0
        for (let iy = 0; iy < field.ny; iy += 3) {
            for (let ix = 0; ix < field.nx; ix += 3) {
                const px = field.x0 + ix * cell
                const py = field.y0 + iy * cell
                const d1 = field.rg[(iy * field.nx + ix) * 3]
                if (d1 >= DOPP_CAP) continue
                maxErr = Math.max(maxErr, Math.abs(d1 - bruteD1(px, py)))
            }
        }
        // Sampling the spine at `cell` spacing bounds the point-vs-segment
        // discrepancy by ~cell/2; JFA seed errors are rarer and smaller.
        expect(maxErr).toBeLessThan(cell * 0.75)
    })

    it('emits outward keyline bands as extra iso levels', () => {
        const g = growStrokes([hline(0, 300, 0)], { thickness: 30, grow: 0.5, gap: 20, cell: 3, isoLevels: [0, -20] })
        expect(g.levels.length).toBe(2)
        const inner = bbox(g.levels[0].contours)
        const outer = bbox(g.levels[1].contours)
        expect(outer.maxY).toBeGreaterThan(inner.maxY + 20 - 6)
        expect(contoursToPath(g.levels[1].contours)).toMatch(/^M/)
    })
})
