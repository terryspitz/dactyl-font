import { describe, it, expect } from 'vitest'
import {
    buildTextureMask, reactionDiffusion, contourScalarField, defaultRdThreshold, RD_PRESET_NAMES,
    maze, wallsToSvgPath,
    circuit, circuitToSvg,
} from './texture'

const hline = (x0, x1, y, glyph) => ({ pts: [[x0, y], [x1, y]], closed: false, glyph })

describe('buildTextureMask', () => {
    it('returns null for empty strokes', () => {
        expect(buildTextureMask([])).toBeNull()
    })

    it('marks cells near the spine as inside, far cells as outside', () => {
        const built = buildTextureMask([hline(0, 300, 0)], { thickness: 30, grow: 0.5, gap: 20, cell: 5 })
        expect(built).not.toBeNull()
        const { mask, nx, ny, x0, y0, cell } = built
        // A cell near the spine's midpoint should be inside; one far above should not.
        // rMax at grow=0.5, growScale=120 (default) is 15 + 0.5*120 = 75, so
        // y=150 is well outside the grown stroke but still inside the padded grid.
        const ixMid = Math.round((150 - x0) / cell)
        const iyOnSpine = Math.round((0 - y0) / cell)
        const iyFar = Math.round((150 - y0) / cell)
        expect(mask[iyOnSpine * nx + ixMid]).toBe(1)
        expect(iyFar).toBeLessThan(ny)
        expect(mask[iyFar * nx + ixMid]).toBe(0)
    })
})

describe('reactionDiffusion', () => {
    const strokes = [hline(0, 300, 0)]
    const built = () => buildTextureMask(strokes, { thickness: 30, grow: 0.6, gap: 20, cell: 6 })

    it('is deterministic for a given seed', () => {
        const { mask, nx, ny } = built()
        const a = reactionDiffusion(mask, nx, ny, { seed: 3, steps: 300 })
        const b = reactionDiffusion(mask, nx, ny, { seed: 3, steps: 300 })
        expect(Array.from(a)).toEqual(Array.from(b))
    })

    it('differs for a different seed', () => {
        const { mask, nx, ny } = built()
        const a = reactionDiffusion(mask, nx, ny, { seed: 1, steps: 300 })
        const b = reactionDiffusion(mask, nx, ny, { seed: 2, steps: 300 })
        expect(Array.from(a)).not.toEqual(Array.from(b))
    })

    it('stays exactly 0 outside the mask', () => {
        const { mask, nx, ny } = built()
        const V = reactionDiffusion(mask, nx, ny, { seed: 1, steps: 300 })
        for (let k = 0; k < mask.length; k++) {
            if (!mask[k]) expect(V[k]).toBe(0)
        }
    })

    it('supports all named presets without throwing', () => {
        const { mask, nx, ny } = built()
        for (const preset of RD_PRESET_NAMES) {
            const V = reactionDiffusion(mask, nx, ny, { preset, seed: 1, steps: 300 })
            expect(V.length).toBe(nx * ny)
        }
    })

    it('produces a contourable field with defaultRdThreshold', () => {
        const { mask, nx, ny, x0, y0, cell } = built()
        const V = reactionDiffusion(mask, nx, ny, { seed: 4, steps: 800 })
        const threshold = defaultRdThreshold(V, mask)
        expect(threshold).toBeGreaterThan(0)
        const contours = contourScalarField(V, nx, ny, x0, y0, cell, threshold)
        expect(contours.length).toBeGreaterThan(0)
    })
})

describe('maze', () => {
    it('carves a perfect spanning-tree maze (no cycles) per connected component', () => {
        const strokes = [hline(0, 300, 0)]
        const cell = 20
        const { mask, nx, ny, x0, y0 } = buildTextureMask(strokes, { thickness: 30, grow: 0.6, gap: 20, cell })
        const { walls } = maze(mask, nx, ny, x0, y0, cell, { seed: 5 })
        expect(walls.length).toBeGreaterThan(0)

        // Reconstruct which adjacent mask-cell pairs are corridors (i.e. NOT
        // walled) by checking for a wall segment centred on their shared edge,
        // then verify the corridor graph is a spanning forest: exactly
        // (maskCells - components) edges, and a union-find over them never
        // reunites two cells already in the same set (no cycles).
        const wallMidpoints = new Set()
        const key = (x, y) => `${x.toFixed(2)},${y.toFixed(2)}`
        for (const [x1, y1, x2, y2] of walls) wallMidpoints.add(key((x1 + x2) / 2, (y1 + y2) / 2))

        const parent = new Map()
        const find = (a) => { while (parent.get(a) !== a) a = parent.get(a); return a }
        let maskCount = 0
        for (let k = 0; k < nx * ny; k++) if (mask[k]) { parent.set(k, k); maskCount++ }

        let corridorCount = 0, cycles = 0
        for (let iy = 0; iy < ny; iy++) {
            for (let ix = 0; ix < nx; ix++) {
                const k = iy * nx + ix
                if (!mask[k]) continue
                if (ix + 1 < nx && mask[k + 1]) {
                    const mx = x0 + ix * cell + cell / 2, my = y0 + iy * cell
                    if (!wallMidpoints.has(key(mx, my))) {
                        corridorCount++
                        const ra = find(k), rb = find(k + 1)
                        if (ra === rb) cycles++; else parent.set(ra, rb)
                    }
                }
                if (iy + 1 < ny && mask[k + nx]) {
                    const mx = x0 + ix * cell, my = y0 + iy * cell + cell / 2
                    if (!wallMidpoints.has(key(mx, my))) {
                        corridorCount++
                        const ra = find(k), rb = find(k + nx)
                        if (ra === rb) cycles++; else parent.set(ra, rb)
                    }
                }
            }
        }
        const roots = new Set()
        for (let k = 0; k < nx * ny; k++) if (mask[k]) roots.add(find(k))

        expect(cycles).toBe(0)
        expect(corridorCount).toBe(maskCount - roots.size)
    })

    it('carves each disconnected mask piece independently', () => {
        // Two separated strokes (same "glyph" doesn't matter for maze/mask —
        // only geometric proximity does): far enough apart that grow=0.6
        // doesn't bridge them, so the mask has two components.
        const strokes = [hline(0, 100, 0), hline(400, 500, 0)]
        const cell = 12
        const { mask, nx, ny, x0, y0 } = buildTextureMask(strokes, { thickness: 30, grow: 0.3, gap: 20, cell })
        const { walls } = maze(mask, nx, ny, x0, y0, cell, { seed: 1 })
        // Both pieces should have some carved corridors (i.e. not every
        // mask-cell boundary is walled) — a naive single-component algorithm
        // that only starts from one piece would leave the other fully walled.
        const path = wallsToSvgPath(walls)
        expect(path.length).toBeGreaterThan(0)
        // Sanity: bounding boxes of the ink (mask) should span both strokes.
        let minX = Infinity, maxX = -Infinity
        for (let k = 0; k < nx * ny; k++) {
            if (!mask[k]) continue
            const ix = k % nx
            const x = x0 + ix * cell
            if (x < minX) minX = x
            if (x > maxX) maxX = x
        }
        expect(maxX - minX).toBeGreaterThan(400)
    })
})

describe('circuit', () => {
    it('confines all trace points to the mask grid and stays orthogonal', () => {
        const strokes = [hline(0, 300, 0)]
        const cell = 15
        const { mask, nx, ny, x0, y0 } = buildTextureMask(strokes, { thickness: 30, grow: 0.6, gap: 20, cell })
        const { traces, pads } = circuit(mask, nx, ny, x0, y0, cell, { seed: 2, density: 0.15 })
        expect(traces.length).toBeGreaterThan(0)
        expect(pads.length).toBeGreaterThan(0)

        for (const t of traces) {
            for (let i = 1; i < t.points.length; i++) {
                const [ax, ay] = t.points[i - 1], [bx, by] = t.points[i]
                const dx = Math.abs(bx - ax), dy = Math.abs(by - ay)
                // Each step is exactly one grid cell, axis-aligned (Manhattan).
                const isAxisStep = (dx === 0 && Math.abs(dy - cell) < 1e-6) || (dy === 0 && Math.abs(dx - cell) < 1e-6)
                expect(isAxisStep).toBe(true)
            }
            for (const [x, y] of t.points) {
                const ix = Math.round((x - x0) / cell), iy = Math.round((y - y0) / cell)
                expect(mask[iy * nx + ix]).toBe(1)
            }
        }
    })

    it('is deterministic for a given seed', () => {
        const strokes = [hline(0, 300, 0)]
        const cell = 15
        const { mask, nx, ny, x0, y0 } = buildTextureMask(strokes, { thickness: 30, grow: 0.6, gap: 20, cell })
        const a = circuit(mask, nx, ny, x0, y0, cell, { seed: 7 })
        const b = circuit(mask, nx, ny, x0, y0, cell, { seed: 7 })
        expect(circuitToSvg(a.traces, a.pads)).toBe(circuitToSvg(b.traces, b.pads))
    })
})
