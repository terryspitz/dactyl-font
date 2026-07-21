// Branching growth off the spine (brainstorm Idea 5): space colonisation.
//
// Attractor points are scattered in the counters and margins around each
// glyph's spine.  Twigs grow from spine-seeded root nodes toward the nearest
// attractors, consuming ("killing") attractors as they're reached — the
// classic space-colonisation / venation algorithm (Runions et al., "Modeling
// Trees with a Space Colonization Algorithm").  At low strength this reads as
// flared serifs sprouting off the letterform; at high strength as ivy
// overtaking it.
//
// The spine itself supplies the root nodes (one every `influence * 0.6` font
// units of arc length), so twigs emerge directly from the backbone, same as
// the SDF growth field in growth.js reuses the spine as its seed geometry.

import { SampleGrid } from './growth.js'

/// Deterministic PRNG (mulberry32) so a `seed` param keeps visual tests stable.
function mulberry32(seed) {
    let a = seed >>> 0
    return () => {
        a = (a + 0x6D2B79F5) | 0
        let t = Math.imul(a ^ (a >>> 15), 1 | a)
        t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
        return ((t ^ (t >>> 14)) >>> 0) / 4294967296
    }
}

/// Resample every stroke's spine at ~`spacing` into root nodes (depth 0, no
/// parent) — the fixed trunk twigs are allowed to sprout from.
function seedNodes(strokes, spacing) {
    const nodes = []
    for (const stroke of strokes) {
        const pts = stroke.pts
        if (pts.length === 0) continue
        if (pts.length === 1) { nodes.push({ x: pts[0][0], y: pts[0][1], parent: -1, depth: 0 }); continue }
        nodes.push({ x: pts[0][0], y: pts[0][1], parent: -1, depth: 0 })
        let acc = 0
        const segs = stroke.closed ? pts.length : pts.length - 1
        for (let i = 0; i < segs; i++) {
            const [x1, y1] = pts[i]
            const [x2, y2] = pts[(i + 1) % pts.length]
            acc += Math.hypot(x2 - x1, y2 - y1)
            if (acc >= spacing) {
                nodes.push({ x: x2, y: y2, parent: -1, depth: 0 })
                acc = 0
            }
        }
    }
    return nodes
}

/// Scatter attractor points on a jittered grid (spacing `density`) over
/// `bbox`, keeping only points between `minGap` (clear of the ink) and
/// `maxReach` (still within reach of some spine sample) of the nearest spine
/// sample — i.e. seeded in the counters and margins, not on top of a stroke
/// or out in empty space nothing will ever grow into.
function scatterAttractors(spineGrid, opts, rng) {
    const { density, minGap, maxReach, bbox } = opts
    const attractors = []
    for (let cy = bbox.y0; cy < bbox.y1; cy += density) {
        for (let cx = bbox.x0; cx < bbox.x1; cx += density) {
            const x = cx + (rng() - 0.5) * density
            const y = cy + (rng() - 0.5) * density
            const near = spineGrid.nearest(x, y, maxReach)
            if (near < 0) continue
            const d = Math.hypot(spineGrid.xs[near] - x, spineGrid.ys[near] - y)
            if (d < minGap || d > maxReach) continue
            attractors.push({ x, y, alive: true })
        }
    }
    return attractors
}

/// Run space colonisation over a set of spine strokes.
///
/// strokes: [{ pts: [[x,y],...], closed: bool }] in font units (y up), as
/// produced by glyphSpines.js.
/// opts:
///   density      – attractor spacing / target sparsity (default 26)
///   influence    – radius within which a node "sees" an attractor (default 55)
///   killDistance – radius within which a reached attractor is consumed (default 14)
///   stepSize     – twig segment length per growth iteration (default 9)
///   iterations   – max growth steps (default 60)
///   maxReach     – farthest an attractor may sit from the spine (default 140)
///   minGap       – closest an attractor may sit to the spine, clears the ink
///                  (default thickness * 0.7)
///   thickness    – classic stroke thickness, used for minGap's default
///   seed         – RNG seed (default 1)
///
/// Returns { nodes, edges, attractors }; edges are
/// [{ x1, y1, x2, y2, depth }] (depth = steps from the nearest root, for tapering).
export function growBranches(strokes, opts = {}) {
    const density = opts.density ?? 26
    const influence = opts.influence ?? 55
    const killDistance = opts.killDistance ?? 14
    const stepSize = opts.stepSize ?? 9
    const iterations = opts.iterations ?? 60
    const maxReach = opts.maxReach ?? 140
    const minGap = opts.minGap ?? (opts.thickness ?? 30) * 0.7
    const seed = opts.seed ?? 1
    const rng = mulberry32(seed)

    const sx = [], sy = []
    for (const s of strokes) for (const [x, y] of s.pts) { sx.push(x); sy.push(y) }
    if (sx.length === 0) return { nodes: [], edges: [], attractors: [] }
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity
    for (let i = 0; i < sx.length; i++) {
        if (sx[i] < x0) x0 = sx[i]; if (sx[i] > x1) x1 = sx[i]
        if (sy[i] < y0) y0 = sy[i]; if (sy[i] > y1) y1 = sy[i]
    }
    const spineGrid = new SampleGrid(sx, sy, Math.max(16, density))

    const attractors = scatterAttractors(spineGrid, {
        density, minGap, maxReach,
        bbox: { x0: x0 - maxReach, y0: y0 - maxReach, x1: x1 + maxReach, y1: y1 + maxReach },
    }, rng)

    const nodes = seedNodes(strokes, Math.max(influence * 0.6, 12))
    let liveCount = attractors.length

    for (let iter = 0; iter < iterations && liveCount > 0; iter++) {
        const nx = nodes.map(n => n.x), ny = nodes.map(n => n.y)
        const nodeGrid = new SampleGrid(nx, ny, Math.max(16, influence))

        // Each live attractor pulls its single nearest node within `influence`.
        const pull = new Map() // nodeIdx -> { dx, dy }
        for (const a of attractors) {
            if (!a.alive) continue
            const ni = nodeGrid.nearest(a.x, a.y, influence)
            if (ni < 0) continue
            const dx = a.x - nodes[ni].x, dy = a.y - nodes[ni].y
            const d = Math.hypot(dx, dy) || 1e-6
            const acc = pull.get(ni) ?? { dx: 0, dy: 0 }
            acc.dx += dx / d; acc.dy += dy / d
            pull.set(ni, acc)
        }
        if (pull.size === 0) break

        // Grow one step from every pulled node, toward the averaged direction
        // of its influencing attractors.
        for (const [ni, acc] of pull) {
            const len = Math.hypot(acc.dx, acc.dy) || 1e-6
            const parent = nodes[ni]
            nodes.push({
                x: parent.x + (acc.dx / len) * stepSize,
                y: parent.y + (acc.dy / len) * stepSize,
                parent: ni,
                depth: parent.depth + 1,
            })
        }

        // Kill attractors reached by the (now larger) tree.
        const kx = nodes.map(n => n.x), ky = nodes.map(n => n.y)
        const killGrid = new SampleGrid(kx, ky, Math.max(16, killDistance))
        liveCount = 0
        for (const a of attractors) {
            if (!a.alive) continue
            if (killGrid.nearest(a.x, a.y, killDistance) >= 0) a.alive = false
            else liveCount++
        }
    }

    const edges = []
    for (const n of nodes) {
        if (n.parent < 0) continue
        const p = nodes[n.parent]
        edges.push({ x1: p.x, y1: p.y, x2: n.x, y2: n.y, depth: n.depth })
    }
    return { nodes, edges, attractors }
}

/// Render edges as tapered SVG strokes: bucket by depth (shallow = thick,
/// near a root; deep = thin, near a twig tip) into a handful of <path>
/// elements sharing one stroke-width each.  SVG has no true variable-width
/// stroke, so this bands the taper in `bands` steps rather than blending it
/// continuously — fine at typical twig lengths, visibly stepped on very long
/// runs.
export function branchesToSvgPaths(edges, opts = {}) {
    const baseRadius = opts.baseRadius ?? (opts.thickness ?? 30) * 0.32
    const minRadius = opts.minRadius ?? 1.2
    const maxDepthForTaper = opts.maxDepthForTaper ?? 14
    const color = opts.color ?? '#2f7a3d'
    const bands = 8

    let paths = ''
    for (let b = 0; b < bands; b++) {
        const dLo = Math.floor((b / bands) * maxDepthForTaper)
        const dHi = b === bands - 1 ? Infinity : Math.floor(((b + 1) / bands) * maxDepthForTaper)
        const t = (b + 0.5) / bands
        const radius = Math.max(minRadius, baseRadius * (1 - t) + minRadius * t)
        let d = ''
        for (const e of edges) {
            if (e.depth < dLo || e.depth >= dHi) continue
            d += `M${e.x1.toFixed(1)} ${(-e.y1).toFixed(1)}L${e.x2.toFixed(1)} ${(-e.y2).toFixed(1)}`
        }
        if (d) paths += `<path d="${d}" stroke="${color}" stroke-width="${(radius * 2).toFixed(1)}" stroke-linecap="round" fill="none"/>`
    }
    return paths
}
