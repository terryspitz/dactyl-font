// Generative growth: constant-gap inflation of glyph spines.
//
// Instead of offsetting each spine by a fixed thickness, every stroke swells
// into the surrounding whitespace until the channel between it and the nearest
// *opposing* spine narrows to `gap`.  Opposing means far enough away from the
// point that claimed this cell that it reads as a different part of the
// letterform (a different stroke, the far side of a bowl) rather than the
// same stroke a few samples along — that keeps joints (n, e) from pinching
// while counters (o, a, e) still push back and stay open.
//
// The rule per field cell x, with d1 = distance to the nearest spine sample q1
// and dOpp = distance to the nearest sample *opposing* q1:
//
//   rAllowed(x) = max(rMin, min(rMax, dOpp(x) - gap))
//   ink iff d1 <= rAllowed          (field f = rAllowed - d1, ink iff f >= 0)
//
// Opposition is geodesic: two samples oppose when the shortest path between
// them along the spine network (chains along each stroke, plus junction links
// where strokes touch) is longer than `counterK`.  Euclidean proximity is not
// enough — the two limbs of an 'n' joint are euclidean-close *and* connected,
// so they must not pinch, while the two sides of an 'o' bowl are
// euclidean-close at the top but geodesically half the bowl apart, so they
// must keep the counter open.
//
// Two strokes growing toward each other both stop at d1 = dOpp - gap, i.e.
// mid-channel with `gap` of whitespace left between them.  rMax caps how far
// strokes bulge into fully open space; with grow=0, rMax = rMin = thickness/2
// and the result is the classic round-capped constant offset.
//
// The field is sampled on a regular grid and contoured with marching squares;
// because f is continuous, linear interpolation gives smooth outlines.
// Nested "keyline" layers are just contours of the same field at negative
// iso values (bands outside the ink), which fuse between neighbouring glyphs
// exactly like Y2K logotype outlines.

/// Resample a polyline (array of [x,y]) at roughly `spacing`, preserving vertices.
function resample(pts, closed, spacing) {
    const out = []
    const n = pts.length
    if (n === 0) return out
    out.push([pts[0][0], pts[0][1]])
    const segs = closed ? n : n - 1
    for (let i = 0; i < segs; i++) {
        const [x1, y1] = pts[i]
        const [x2, y2] = pts[(i + 1) % n]
        const len = Math.hypot(x2 - x1, y2 - y1)
        const steps = Math.max(1, Math.ceil(len / spacing))
        for (let s = 1; s <= steps; s++) {
            const t = s / steps
            out.push([x1 + (x2 - x1) * t, y1 + (y2 - y1) * t])
        }
    }
    return out
}

/// Uniform-grid spatial hash over point samples for nearest-neighbour queries.
class SampleGrid {
    constructor(xs, ys, bucket) {
        this.xs = xs
        this.ys = ys
        this.bucket = bucket
        let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity
        for (let i = 0; i < xs.length; i++) {
            if (xs[i] < minX) minX = xs[i]
            if (xs[i] > maxX) maxX = xs[i]
            if (ys[i] < minY) minY = ys[i]
            if (ys[i] > maxY) maxY = ys[i]
        }
        this.minX = minX
        this.minY = minY
        this.nx = Math.max(1, Math.floor((maxX - minX) / bucket) + 1)
        this.ny = Math.max(1, Math.floor((maxY - minY) / bucket) + 1)
        this.cells = new Array(this.nx * this.ny)
        for (let i = 0; i < xs.length; i++) {
            const cx = Math.min(this.nx - 1, Math.max(0, Math.floor((xs[i] - minX) / bucket)))
            const cy = Math.min(this.ny - 1, Math.max(0, Math.floor((ys[i] - minY) / bucket)))
            const k = cy * this.nx + cx
            if (!this.cells[k]) this.cells[k] = []
            this.cells[k].push(i)
        }
    }

    /// Nearest sample to (x, y) within maxR satisfying `accept(i)` (or any, if null).
    /// Returns { idx, d } with idx = -1 when nothing qualifies.
    nearest(x, y, maxR, accept) {
        const { xs, ys, bucket, nx, ny } = this
        const cx = Math.floor((x - this.minX) / bucket)
        const cy = Math.floor((y - this.minY) / bucket)
        const maxRing = Math.ceil(maxR / bucket) + 1
        let bestD2 = maxR * maxR
        let best = -1
        for (let ring = 0; ring <= maxRing; ring++) {
            // No sample in a farther ring can beat the current best.
            const ringMin = (ring - 1) * bucket
            if (ringMin > 0 && ringMin * ringMin > bestD2) break
            const x0 = cx - ring, x1 = cx + ring
            const y0 = cy - ring, y1 = cy + ring
            for (let gy = y0; gy <= y1; gy++) {
                if (gy < 0 || gy >= ny) continue
                const onYEdge = gy === y0 || gy === y1
                for (let gx = x0; gx <= x1; gx++) {
                    if (gx < 0 || gx >= nx) continue
                    if (!onYEdge && gx !== x0 && gx !== x1) continue // ring perimeter only
                    const cell = this.cells[gy * nx + gx]
                    if (!cell) continue
                    for (const i of cell) {
                        if (accept && !accept(i)) continue
                        const dx = xs[i] - x, dy = ys[i] - y
                        const d2 = dx * dx + dy * dy
                        if (d2 < bestD2) { bestD2 = d2; best = i }
                    }
                }
            }
        }
        return { idx: best, d: best >= 0 ? Math.sqrt(bestD2) : Infinity }
    }

    /// All sample indices within radius r of (x, y).
    within(x, y, r) {
        const { xs, ys, bucket, nx, ny } = this
        const out = []
        const r2 = r * r
        const gx0 = Math.max(0, Math.floor((x - r - this.minX) / bucket))
        const gx1 = Math.min(nx - 1, Math.floor((x + r - this.minX) / bucket))
        const gy0 = Math.max(0, Math.floor((y - r - this.minY) / bucket))
        const gy1 = Math.min(ny - 1, Math.floor((y + r - this.minY) / bucket))
        for (let gy = gy0; gy <= gy1; gy++) {
            for (let gx = gx0; gx <= gx1; gx++) {
                const cell = this.cells[gy * nx + gx]
                if (!cell) continue
                for (const i of cell) {
                    const dx = xs[i] - x, dy = ys[i] - y
                    if (dx * dx + dy * dy <= r2) out.push(i)
                }
            }
        }
        return out
    }
}

/// Marching squares at iso value `iso` over field f (ny rows × nx cols, grid
/// spacing h, origin x0/y0).  Returns closed contours as arrays of [x,y].
/// Assumes the field border is below every iso used (padded), so all contours close.
function marchingSquares(f, nx, ny, x0, y0, h, iso) {
    // Key each contour segment by its start/end cell-edge so successive
    // segments chain into polylines.  Edge id: (edge orientation, ix, iy).
    const segStarts = new Map() // edgeKey -> { to: edgeKey, pt: [x,y] }
    const ptOf = new Map()      // edgeKey -> interpolated point

    const edgeKey = (ix, iy, horiz) => (iy * (nx + 1) + ix) * 2 + (horiz ? 1 : 0)

    const interp = (va, vb, xa, ya, xb, yb) => {
        const t = (iso - va) / (vb - va)
        return [xa + (xb - xa) * t, ya + (yb - ya) * t]
    }

    for (let iy = 0; iy < ny - 1; iy++) {
        for (let ix = 0; ix < nx - 1; ix++) {
            const v00 = f[iy * nx + ix]        // bottom-left  (grid y up)
            const v10 = f[iy * nx + ix + 1]    // bottom-right
            const v01 = f[(iy + 1) * nx + ix]  // top-left
            const v11 = f[(iy + 1) * nx + ix + 1]
            let idx = 0
            if (v00 >= iso) idx |= 1
            if (v10 >= iso) idx |= 2
            if (v11 >= iso) idx |= 4
            if (v01 >= iso) idx |= 8
            if (idx === 0 || idx === 15) continue

            const X = x0 + ix * h, Y = y0 + iy * h
            // Cell edges: B(ottom) horiz at iy, T(op) horiz at iy+1,
            //             L(eft) vert at ix, R(ight) vert at ix+1
            const eB = edgeKey(ix, iy, true)
            const eT = edgeKey(ix, iy + 1, true)
            const eL = edgeKey(ix, iy, false)
            const eR = edgeKey(ix + 1, iy, false)
            const pB = () => interp(v00, v10, X, Y, X + h, Y)
            const pT = () => interp(v01, v11, X, Y + h, X + h, Y + h)
            const pL = () => interp(v00, v01, X, Y, X, Y + h)
            const pR = () => interp(v10, v11, X + h, Y, X + h, Y + h)

            // Emit directed segments (inside on the left) so chaining is unambiguous.
            const emit = (eFrom, pFrom, eTo, pTo) => {
                ptOf.set(eFrom, pFrom)
                ptOf.set(eTo, pTo)
                segStarts.set(eFrom, eTo)
            }
            switch (idx) {
                case 1: emit(eL, pL(), eB, pB()); break
                case 2: emit(eB, pB(), eR, pR()); break
                case 3: emit(eL, pL(), eR, pR()); break
                case 4: emit(eR, pR(), eT, pT()); break
                case 5: { // saddle: use centre value to disambiguate
                    const c = (v00 + v10 + v01 + v11) / 4
                    if (c >= iso) { emit(eL, pL(), eT, pT()); emit(eR, pR(), eB, pB()) }
                    else { emit(eL, pL(), eB, pB()); emit(eR, pR(), eT, pT()) }
                    break
                }
                case 6: emit(eB, pB(), eT, pT()); break
                case 7: emit(eL, pL(), eT, pT()); break
                case 8: emit(eT, pT(), eL, pL()); break
                case 9: emit(eT, pT(), eB, pB()); break
                case 10: { // saddle
                    const c = (v00 + v10 + v01 + v11) / 4
                    if (c >= iso) { emit(eB, pB(), eL, pL()); emit(eT, pT(), eR, pR()) }
                    else { emit(eB, pB(), eR, pR()); emit(eT, pT(), eL, pL()) }
                    break
                }
                case 11: emit(eT, pT(), eR, pR()); break
                case 12: emit(eR, pR(), eL, pL()); break
                case 13: emit(eR, pR(), eB, pB()); break
                case 14: emit(eB, pB(), eL, pL()); break
            }
        }
    }

    // Chain directed segments into closed loops.
    const contours = []
    const visited = new Set()
    for (const start of segStarts.keys()) {
        if (visited.has(start)) continue
        const loop = []
        let e = start
        while (e !== undefined && !visited.has(e)) {
            visited.add(e)
            loop.push(ptOf.get(e))
            e = segStarts.get(e)
        }
        if (loop.length >= 3) contours.push(loop)
    }
    return contours
}

/// Per-sample sets of geodesically-near samples ("same part of the letterform").
/// Samples are chained along each stroke; strokes that touch are linked where
/// they come within `junctionEps`.  A bounded Dijkstra from each sample marks
/// everything within geodesic distance `counterK` as same-part; all other
/// samples oppose it.
function buildSamePartSets(xs, ys, strokeOf, strokeRanges, closedFlags, grid, junctionEps, counterK) {
    const n = xs.length
    const adj = Array.from({ length: n }, () => [])
    const link = (a, b) => {
        const w = Math.hypot(xs[a] - xs[b], ys[a] - ys[b])
        adj[a].push([b, w])
        adj[b].push([a, w])
    }
    // Chain edges along each stroke.
    for (const [start, end, closed] of strokeRanges.map((r, i) => [r[0], r[1], closedFlags[i]])) {
        for (let i = start; i < end - 1; i++) link(i, i + 1)
        if (closed && end - start > 2) link(end - 1, start)
    }
    // Junction edges between different strokes that touch.
    const eps2 = junctionEps * junctionEps
    for (let i = 0; i < n; i++) {
        const near = grid.within(xs[i], ys[i], junctionEps)
        for (const j of near) {
            if (j > i && strokeOf[j] !== strokeOf[i]) {
                const dx = xs[i] - xs[j], dy = ys[i] - ys[j]
                if (dx * dx + dy * dy <= eps2) link(i, j)
            }
        }
    }
    // Bounded Dijkstra from every sample (the reachable set is small: a
    // counterK-long stretch of chain plus junction spill-over).
    const sets = new Array(n)
    const dist = new Map()
    for (let src = 0; src < n; src++) {
        dist.clear()
        dist.set(src, 0)
        const heap = [[0, src]] // tiny local frontier: array-as-heap is fine
        const members = new Set([src])
        while (heap.length) {
            let mi = 0
            for (let i = 1; i < heap.length; i++) if (heap[i][0] < heap[mi][0]) mi = i
            const [d, u] = heap.splice(mi, 1)[0]
            if (d > (dist.get(u) ?? Infinity)) continue
            for (const [v, w] of adj[u]) {
                const nd = d + w
                if (nd <= counterK && nd < (dist.get(v) ?? Infinity)) {
                    dist.set(v, nd)
                    members.add(v)
                    heap.push([nd, v])
                }
            }
        }
        sets[src] = members
    }
    return sets
}

/// In-place 3×3 binomial blur of the field (repeated `passes` times).  The raw
/// field has small discontinuities where the nearest sample flips between
/// distant parts of the skeleton; blurring removes the resulting contour tears.
function blurField(f, nx, ny, passes) {
    if (passes <= 0) return f
    let src = f
    let dst = new Float32Array(f.length)
    const tmp = new Float32Array(f.length)
    for (let p = 0; p < passes; p++) {
        // horizontal [1 2 1]/4
        for (let iy = 0; iy < ny; iy++) {
            const row = iy * nx
            for (let ix = 0; ix < nx; ix++) {
                const l = src[row + Math.max(0, ix - 1)]
                const r = src[row + Math.min(nx - 1, ix + 1)]
                tmp[row + ix] = (l + 2 * src[row + ix] + r) / 4
            }
        }
        // vertical [1 2 1]/4
        for (let iy = 0; iy < ny; iy++) {
            const up = Math.max(0, iy - 1) * nx
            const dn = Math.min(ny - 1, iy + 1) * nx
            const row = iy * nx
            for (let ix = 0; ix < nx; ix++) {
                dst[row + ix] = (tmp[up + ix] + 2 * tmp[row + ix] + tmp[dn + ix]) / 4
            }
        }
        ;[src, dst] = [dst, src]
    }
    return src
}

/// Ramer–Douglas–Peucker simplification of a closed contour.
function simplify(pts, eps) {
    if (pts.length < 8) return pts
    const keep = new Uint8Array(pts.length)
    keep[0] = 1
    // Split a closed loop at its farthest point from pts[0] to get two open runs.
    let far = 0, farD = -1
    for (let i = 1; i < pts.length; i++) {
        const d = (pts[i][0] - pts[0][0]) ** 2 + (pts[i][1] - pts[0][1]) ** 2
        if (d > farD) { farD = d; far = i }
    }
    keep[far] = 1
    const stack = [[0, far], [far, pts.length]] // second run wraps to index 0
    while (stack.length) {
        const [a, b] = stack.pop()
        const pb = pts[b % pts.length]
        const pa = pts[a]
        const dx = pb[0] - pa[0], dy = pb[1] - pa[1]
        const len = Math.hypot(dx, dy) || 1e-12
        let maxD = -1, maxI = -1
        for (let i = a + 1; i < b; i++) {
            const d = Math.abs((pts[i][0] - pa[0]) * dy - (pts[i][1] - pa[1]) * dx) / len
            if (d > maxD) { maxD = d; maxI = i }
        }
        if (maxD > eps) {
            keep[maxI] = 1
            stack.push([a, maxI], [maxI, b])
        }
    }
    const out = []
    for (let i = 0; i < pts.length; i++) if (keep[i]) out.push(pts[i])
    return out
}

/// Grow strokes into a field and contour it.
///
/// strokes: [{ pts: [[x,y],...], closed: bool }] in font units (y up).
/// params:
///   thickness  – classic stroke thickness (rMin = thickness/2)
///   grow       – 0..1: how far strokes may bulge beyond classic (0 = classic offset)
///   growScale  – max extra radius at grow=1 (default 120)
///   gap        – whitespace channel preserved between opposing strokes
///   counterK   – samples farther apart than this (geodesically, along the spine
///                network) oppose each other (default 2.2*thickness + gap)
///   cell       – field grid spacing in font units (default 4)
///   isoLevels  – field iso values to contour, e.g. [0, -14, -30]; 0 is the ink edge,
///                negative values are outward keyline bands (default [0])
///   smoothPasses – 3×3 blur passes over the field before contouring (default 2)
///
/// Returns { levels: [{ iso, contours }], bbox: {x0,y0,x1,y1}, stats }.
export function growStrokes(strokes, params = {}) {
    const thickness = params.thickness ?? 30
    const grow = params.grow ?? 0.5
    const growScale = params.growScale ?? 120
    const gap = params.gap ?? thickness * 0.8
    const counterK = params.counterK ?? 2.2 * thickness + gap
    const cell = params.cell ?? 4
    const isoLevels = params.isoLevels ?? [0]
    const smoothPasses = params.smoothPasses ?? 2

    const rMin = thickness / 2
    const rMax = rMin + grow * growScale
    const outward = Math.max(0, -Math.min(...isoLevels)) // farthest outward band
    const fieldRange = rMax + outward + 2 * cell         // beyond this, f is "far outside"
    const farValue = -(fieldRange + cell)

    // --- Sample all spines ---
    const sx = [], sy = [], sid = []
    const strokeRanges = []
    const closedFlags = []
    for (const stroke of strokes) {
        const pts = stroke.pts.length >= 2
            ? resample(stroke.pts, stroke.closed, cell)
            : stroke.pts // single point (a Dot)
        const start = sx.length
        for (const [x, y] of pts) { sx.push(x); sy.push(y); sid.push(strokeRanges.length) }
        strokeRanges.push([start, sx.length])
        closedFlags.push(!!stroke.closed)
    }
    if (sx.length === 0) return { levels: isoLevels.map(iso => ({ iso, contours: [] })), bbox: null, stats: {} }
    const xs = Float64Array.from(sx)
    const ys = Float64Array.from(sy)
    const strokeOf = Int32Array.from(sid)
    const grid = new SampleGrid(xs, ys, Math.max(16, cell * 6))

    // Geodesic same-part sets: strokes touching within ~a sample spacing are
    // joined; everything beyond counterK along the network opposes.
    const samePart = buildSamePartSets(
        xs, ys, strokeOf, strokeRanges, closedFlags, grid, cell * 2.2, counterK)

    // --- Field bounds (pad so every contour closes inside the grid) ---
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity
    for (let i = 0; i < xs.length; i++) {
        if (xs[i] < minX) minX = xs[i]
        if (xs[i] > maxX) maxX = xs[i]
        if (ys[i] < minY) minY = ys[i]
        if (ys[i] > maxY) maxY = ys[i]
    }
    const pad = fieldRange + 2 * cell
    const x0 = minX - pad, y0 = minY - pad
    const nx = Math.ceil((maxX + pad - x0) / cell) + 1
    const ny = Math.ceil((maxY + pad - y0) / cell) + 1

    // --- Evaluate f = rAllowed - d1 on the grid ---
    let f = new Float32Array(nx * ny).fill(farValue)
    // dOpp only constrains while dOpp - gap < rMax; search a little past that.
    const oppSearchR = rMax + gap + fieldRange
    let cellsEvaluated = 0
    for (let iy = 0; iy < ny; iy++) {
        const y = y0 + iy * cell
        for (let ix = 0; ix < nx; ix++) {
            const x = x0 + ix * cell
            const n1 = grid.nearest(x, y, fieldRange + cell, null)
            if (n1.idx < 0) continue
            cellsEvaluated++
            const mine = samePart[n1.idx]
            const opp = grid.nearest(x, y, oppSearchR, (i) => !mine.has(i))
            const rAllowed = Math.max(rMin, Math.min(rMax, opp.d - gap))
            f[iy * nx + ix] = rAllowed - n1.d
        }
    }

    f = blurField(f, nx, ny, smoothPasses)

    // --- Contour each requested level ---
    // Drop speck contours (grid-noise islands a couple of cells across).
    const minArea = (3 * cell) ** 2
    const area = (c) => {
        let a = 0
        for (let i = 0; i < c.length; i++) {
            const [x1, y1] = c[i]
            const [x2, y2] = c[(i + 1) % c.length]
            a += x1 * y2 - x2 * y1
        }
        return Math.abs(a) / 2
    }
    const levels = isoLevels.map(iso => ({
        iso,
        contours: marchingSquares(f, nx, ny, x0, y0, cell, iso)
            .filter(c => area(c) >= minArea)
            .map(c => simplify(c, cell * 0.12)),
    }))

    return {
        levels,
        bbox: { x0: minX - pad, y0: minY - pad, x1: maxX + pad, y1: maxY + pad },
        stats: { samples: xs.length, gridCells: nx * ny, cellsEvaluated, rMin, rMax },
    }
}

/// Contours (font units, y up) → SVG path data (y flipped to SVG coords).
export function contoursToPath(contours, decimals = 1) {
    const fmt = (v) => v.toFixed(decimals).replace(/\.0$/, '')
    let d = ''
    for (const c of contours) {
        d += `M${fmt(c[0][0])} ${fmt(-c[0][1])}`
        for (let i = 1; i < c.length; i++) d += `L${fmt(c[i][0])} ${fmt(-c[i][1])}`
        d += 'Z'
    }
    return d
}
