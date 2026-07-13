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
// The field also carries a third channel, `cross` in [0,1]: whether the
// opposition at a cell straddles a glyph boundary (the nearest sample and its
// opponent belong to different glyphs) rather than two parts of the same
// glyph.  It lets `fuse` melt neighbouring letters into a logotype — the gap
// is only relaxed where opposition is cross-glyph, so an 'o' counter never
// collapses even at full fuse.  It is blurred like dOpp so the boundary is a
// soft ramp, not a hard 0/1 step.
//
// (d1, dOpp, cross) form a quasi-SDF of the letterform, computed once per
// text/axes change with a jump-flood transform (buildGrowthField).  Both
// consumers derive f from it: growStrokes contours f with marching squares
// for vector output, and GrowCanvas.jsx thresholds it in a fragment shader
// where grow/gap/fuse/layers are just uniforms.

// dOpp is capped here: values beyond every reachable rMax + gap are
// equivalent (the growth rule clamps to rMax first), and a finite cap keeps
// the blur pass from smearing "no opponent found" sentinels into real data.
export const DOPP_CAP = 400

// Techno-Drive-style keyline stack, innermost (ink core, iso 0) first.
export const LAYER_COLORS = ['#f4fbff', '#7ec4ee', '#1660c8', '#0a0a14']

/// Iso values matching LAYER_COLORS: the ink edge plus outward keyline bands.
export const layerIsoLevels = (thickness) =>
    [0, -0.4 * thickness, -0.9 * thickness, -1.5 * thickness]

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

/// Uniform-grid spatial hash over point samples, used for junction detection.
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

/// Build the opposition structure: two samples are "same part" when the
/// shortest path between them along the spine network (chains along each
/// stroke, plus junction links where strokes touch) is at most `counterK`.
///
/// The common case — same stroke, arc distance ≤ counterK — is answered in
/// O(1) from cumulative arc positions.  Pairs that are only same-part via a
/// junction shortcut (a different stroke, or the far side of a bowl reached
/// through a crossbar) are enumerated by a bounded Dijkstra and stored as
/// per-sample sorted "extras".  (Geodesic distance is symmetric, so the
/// relation is too.)
function buildOpposition(xs, ys, arcPos, strokeOf, strokeTotal, strokeClosed, strokeRanges, grid, junctionEps, counterK) {
    const n = xs.length

    const arcNear = (a, b) => {
        const s = strokeOf[a]
        let d = Math.abs(arcPos[a] - arcPos[b])
        if (strokeClosed[s]) d = Math.min(d, strokeTotal[s] - d)
        return d <= counterK
    }

    const adj = Array.from({ length: n }, () => [])
    const link = (a, b) => {
        const w = Math.hypot(xs[a] - xs[b], ys[a] - ys[b])
        adj[a].push([b, w])
        adj[b].push([a, w])
    }
    // Chain edges along each stroke.
    for (let si = 0; si < strokeRanges.length; si++) {
        const [start, end] = strokeRanges[si]
        for (let i = start; i < end - 1; i++) link(i, i + 1)
        if (strokeClosed[si] && end - start > 2) link(end - 1, start)
    }
    // Junction edges between different strokes that touch.
    const eps2 = junctionEps * junctionEps
    let junctions = 0
    for (let i = 0; i < n; i++) {
        for (const j of grid.within(xs[i], ys[i], junctionEps)) {
            if (j > i && strokeOf[j] !== strokeOf[i]) {
                const dx = xs[i] - xs[j], dy = ys[i] - ys[j]
                if (dx * dx + dy * dy <= eps2) { link(i, j); junctions++ }
            }
        }
    }

    // Bounded Dijkstra from every sample; record only pairs the O(1) arc rule
    // can't answer.  With no junctions the whole letterform has no extras.
    const extras = new Array(n).fill(null)
    if (junctions > 0) {
        const dist = new Map()
        for (let src = 0; src < n; src++) {
            dist.clear()
            dist.set(src, 0)
            const heap = [[0, src]] // tiny local frontier: array-as-heap is fine
            let members = null
            while (heap.length) {
                let mi = 0
                for (let i = 1; i < heap.length; i++) if (heap[i][0] < heap[mi][0]) mi = i
                const [d, u] = heap.splice(mi, 1)[0]
                if (d > (dist.get(u) ?? Infinity)) continue
                for (const [v, w] of adj[u]) {
                    const nd = d + w
                    if (nd <= counterK && nd < (dist.get(v) ?? Infinity)) {
                        if (!dist.has(v) && (strokeOf[v] !== strokeOf[src] || !arcNear(src, v))) {
                            if (!members) members = []
                            members.push(v)
                        }
                        dist.set(v, nd)
                        heap.push([nd, v])
                    }
                }
            }
            if (members) extras[src] = Int32Array.from(members).sort()
        }
    }

    /// Binary search in a sorted Int32Array.
    const inSorted = (arr, v) => {
        let lo = 0, hi = arr.length - 1
        while (lo <= hi) {
            const mid = (lo + hi) >> 1
            if (arr[mid] === v) return true
            if (arr[mid] < v) lo = mid + 1
            else hi = mid - 1
        }
        return false
    }

    return (a, b) => {
        if (strokeOf[a] === strokeOf[b] && arcNear(a, b)) return true
        const ex = extras[a]
        return ex !== null && inSorted(ex, b)
    }
}

/// Two-channel distance transform over the field grid: for every cell, the
/// nearest sample i1 and the nearest sample opposing i1 (i2, geodesically far
/// from i1 per `samePart`).  Uses dead-reckoning sweeps — the cache-friendly
/// cousin of jump flooding: two forward/backward rounds propagating seed ids
/// from already-visited neighbours, with distances computed exactly to the
/// chosen seed.  Only *which* seed wins is approximate, with rare, small
/// errors (and the dOpp blur downstream hides seed flips anyway).
function distanceSweep(xs, ys, samePart, nx, ny, x0, y0, cell, onProgress) {
    const nCells = nx * ny
    const i1 = new Int32Array(nCells).fill(-1)
    const i2 = new Int32Array(nCells).fill(-1)
    const d1sq = new Float64Array(nCells).fill(Infinity)
    const d2sq = new Float64Array(nCells).fill(Infinity)

    /// Offer candidate sample c to cell k (centre cx, cy).
    const offer = (k, cx, cy, c) => {
        const a = i1[k]
        if (c === a || c === i2[k]) return
        const dx = xs[c] - cx, dy = ys[c] - cy
        const dc = dx * dx + dy * dy
        if (a < 0) { i1[k] = c; d1sq[k] = dc; return }
        if (dc < d1sq[k]) {
            // c dethrones a; a may become the opposition candidate.
            if (!samePart(c, a)) {
                if (i2[k] < 0 || d1sq[k] < d2sq[k] || samePart(c, i2[k])) { i2[k] = a; d2sq[k] = d1sq[k] }
            } else if (i2[k] >= 0 && samePart(c, i2[k])) {
                i2[k] = -1; d2sq[k] = Infinity // old opposition is same-part with the new winner
            }
            i1[k] = c
            d1sq[k] = dc
        } else if (dc < d2sq[k] && !samePart(a, c)) {
            i2[k] = c
            d2sq[k] = dc
        }
    }

    // Seed: every sample offers itself to its own cell.
    for (let s = 0; s < xs.length; s++) {
        const ix = Math.min(nx - 1, Math.max(0, Math.round((xs[s] - x0) / cell)))
        const iy = Math.min(ny - 1, Math.max(0, Math.round((ys[s] - y0) / cell)))
        offer(iy * nx + ix, x0 + ix * cell, y0 + iy * cell, s)
    }

    const gather = (k, cx, cy, j) => {
        const c1 = i1[j]
        if (c1 >= 0) offer(k, cx, cy, c1)
        const c2 = i2[j]
        if (c2 >= 0) offer(k, cx, cy, c2)
    }

    // 4 passes (2 rounds × forward/backward) over ny rows; report row progress.
    const totalPasses = 4
    let pass = 0
    const reportRow = (iy) => {
        if (onProgress && (iy & 15) === 0) onProgress((pass + iy / ny) / totalPasses)
    }

    for (let round = 0; round < 2; round++) {
        // Forward: gather from W, NW, N, NE (already visited this pass).
        for (let iy = 0; iy < ny; iy++) {
            reportRow(iy)
            const cy = y0 + iy * cell
            const row = iy * nx
            for (let ix = 0; ix < nx; ix++) {
                const k = row + ix
                const cx = x0 + ix * cell
                if (ix > 0) gather(k, cx, cy, k - 1)
                if (iy > 0) {
                    const up = k - nx
                    if (ix > 0) gather(k, cx, cy, up - 1)
                    gather(k, cx, cy, up)
                    if (ix < nx - 1) gather(k, cx, cy, up + 1)
                }
            }
        }
        pass++
        // Backward: gather from E, SE, S, SW.
        for (let iy = ny - 1; iy >= 0; iy--) {
            reportRow(ny - 1 - iy)
            const cy = y0 + iy * cell
            const row = iy * nx
            for (let ix = nx - 1; ix >= 0; ix--) {
                const k = row + ix
                const cx = x0 + ix * cell
                if (ix < nx - 1) gather(k, cx, cy, k + 1)
                if (iy < ny - 1) {
                    const dn = k + nx
                    if (ix < nx - 1) gather(k, cx, cy, dn + 1)
                    gather(k, cx, cy, dn)
                    if (ix > 0) gather(k, cx, cy, dn - 1)
                }
            }
        }
        pass++
    }
    return { i1, i2, d1sq, d2sq }
}

/// In-place 3×3 binomial blur of one channel of an interleaved field with
/// `stride` channels per cell (repeated `passes` times).  The dOpp and cross
/// channels have small discontinuities where the nearest sample flips between
/// distant parts of the skeleton; blurring removes the resulting contour tears
/// (and turns cross into a soft 0..1 ramp across glyph boundaries).
function blurChannel(rg, channel, nx, ny, passes, stride) {
    if (passes <= 0) return
    const tmp = new Float32Array(nx * ny)
    const out = new Float32Array(nx * ny)
    for (let p = 0; p < passes; p++) {
        for (let iy = 0; iy < ny; iy++) {
            const row = iy * nx
            for (let ix = 0; ix < nx; ix++) {
                const l = rg[(row + Math.max(0, ix - 1)) * stride + channel]
                const c = rg[(row + ix) * stride + channel]
                const r = rg[(row + Math.min(nx - 1, ix + 1)) * stride + channel]
                tmp[row + ix] = (l + 2 * c + r) / 4
            }
        }
        for (let iy = 0; iy < ny; iy++) {
            const up = Math.max(0, iy - 1) * nx
            const dn = Math.min(ny - 1, iy + 1) * nx
            const row = iy * nx
            for (let ix = 0; ix < nx; ix++) {
                out[row + ix] = (tmp[up + ix] + 2 * tmp[row + ix] + tmp[dn + ix]) / 4
            }
        }
        for (let k = 0; k < nx * ny; k++) rg[k * stride + channel] = out[k]
    }
}

/// 3×3 binomial blur of a plain field array, repeated `passes` times.
function blurField(f, nx, ny, passes) {
    if (passes <= 0) return f
    let src = f
    let dst = new Float32Array(f.length)
    const tmp = new Float32Array(f.length)
    for (let p = 0; p < passes; p++) {
        for (let iy = 0; iy < ny; iy++) {
            const row = iy * nx
            for (let ix = 0; ix < nx; ix++) {
                const l = src[row + Math.max(0, ix - 1)]
                const r = src[row + Math.min(nx - 1, ix + 1)]
                tmp[row + ix] = (l + 2 * src[row + ix] + r) / 4
            }
        }
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

/// Build the three-channel growth field for a set of strokes: for every grid
/// cell, rg[3k] = d1 (distance to the nearest spine sample), rg[3k+1] = dOpp
/// (distance to the nearest sample opposing it, capped at DOPP_CAP), and
/// rg[3k+2] = cross (blurred 0..1 flag: the opposition straddles a glyph
/// boundary).  The field is independent of grow/gap/fuse, so consumers can
/// vary those freely (a fragment shader treats them as uniforms; growStrokes
/// recontours).
///
/// strokes: [{ pts: [[x,y],...], closed: bool, glyph?: int }] in font units
/// (y up).  `glyph` tags which letter a stroke belongs to (default 0); it
/// drives the cross channel and hence `fuse`.
/// opts:
///   thickness  – classic stroke thickness (sets junction scale and padding)
///   growScale  – max extra radius the field must accommodate (default 120)
///   counterK   – geodesic opposition threshold (default 2.2*thickness + 40)
///   cell       – field grid spacing in font units (default 4)
///   maxOutward – farthest outward keyline band to leave room for
///                (default 1.5*thickness)
///   onProgress – optional (0..1) callback reported across the field build
export function buildGrowthField(strokes, opts = {}) {
    const thickness = opts.thickness ?? 30
    const growScale = opts.growScale ?? 120
    const counterK = opts.counterK ?? 2.2 * thickness + 40
    const cell = opts.cell ?? 4
    const maxOutward = opts.maxOutward ?? 1.5 * thickness
    const onProgress = opts.onProgress

    // --- Sample all spines, tracking cumulative arc position per sample ---
    const sx = [], sy = [], sid = [], sarc = [], sglyph = []
    const strokeRanges = []
    const closedFlags = []
    const totals = []
    for (const stroke of strokes) {
        const pts = stroke.pts.length >= 2
            ? resample(stroke.pts, stroke.closed, cell)
            : stroke.pts // single point (a Dot)
        const start = sx.length
        const glyph = stroke.glyph ?? 0
        let arc = 0
        for (let i = 0; i < pts.length; i++) {
            if (i > 0) arc += Math.hypot(pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1])
            sx.push(pts[i][0])
            sy.push(pts[i][1])
            sid.push(strokeRanges.length)
            sarc.push(arc)
            sglyph.push(glyph)
        }
        // Closed strokes wrap: total includes the closing segment.
        if (stroke.closed && pts.length > 2) {
            arc += Math.hypot(pts[0][0] - pts[pts.length - 1][0], pts[0][1] - pts[pts.length - 1][1])
        }
        strokeRanges.push([start, sx.length])
        closedFlags.push(!!stroke.closed)
        totals.push(Math.max(arc, 1e-9))
    }
    if (sx.length === 0) return null
    const xs = Float64Array.from(sx)
    const ys = Float64Array.from(sy)
    const strokeOf = Int32Array.from(sid)
    const glyphOf = Int32Array.from(sglyph)
    const arcPos = Float64Array.from(sarc)
    const strokeTotal = Float64Array.from(totals)
    const strokeClosed = Uint8Array.from(closedFlags.map(c => c ? 1 : 0))
    const grid = new SampleGrid(xs, ys, Math.max(16, cell * 6))

    // Geodesic same-part relation: strokes touching within ~a sample spacing
    // are joined; everything beyond counterK along the network opposes.
    // (Opposition is a modest, hard-to-instrument share; the sweep dominates.)
    if (onProgress) onProgress(0.1)
    const samePart = buildOpposition(
        xs, ys, arcPos, strokeOf, strokeTotal, strokeClosed, strokeRanges, grid, cell * 2.2, counterK)
    if (onProgress) onProgress(0.2)

    // --- Field bounds: pad enough for full growth plus the outermost band,
    // so every contour closes inside the grid at any grow value. ---
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity
    for (let i = 0; i < xs.length; i++) {
        if (xs[i] < minX) minX = xs[i]
        if (xs[i] > maxX) maxX = xs[i]
        if (ys[i] < minY) minY = ys[i]
        if (ys[i] > maxY) maxY = ys[i]
    }
    const pad = thickness / 2 + growScale + maxOutward + 4 * cell
    const x0 = minX - pad, y0 = minY - pad
    const nx = Math.ceil((maxX + pad - x0) / cell) + 1
    const ny = Math.ceil((maxY + pad - y0) / cell) + 1

    // --- Distance sweeps, then extract distances to the winning seeds ---
    // The sweep is the bulk of the field build: map it to the 0.2..1.0 range.
    const sweepProgress = onProgress ? (f => onProgress(0.2 + 0.8 * f)) : undefined
    const { i1, i2, d1sq, d2sq } = distanceSweep(xs, ys, samePart, nx, ny, x0, y0, cell, sweepProgress)
    const rg = new Float32Array(nx * ny * 3)
    for (let k = 0; k < nx * ny; k++) {
        const a = i1[k], b = i2[k]
        rg[k * 3] = a < 0 ? DOPP_CAP : Math.sqrt(d1sq[k])
        rg[k * 3 + 1] = b < 0 ? DOPP_CAP : Math.min(DOPP_CAP, Math.sqrt(d2sq[k]))
        // cross = opposition straddles a glyph boundary (drives `fuse`).
        rg[k * 3 + 2] = (a >= 0 && b >= 0 && glyphOf[a] !== glyphOf[b]) ? 1 : 0
    }
    // Smooth the opposition and cross channels: d1 is a true distance and
    // already continuous, but dOpp jumps where the winning seed flips (and the
    // sweep's occasional wrong-seed picks land in the same discontinuities),
    // and cross is a hard 0/1 that reads better as a soft boundary ramp.
    blurChannel(rg, 1, nx, ny, 2, 3)
    blurChannel(rg, 2, nx, ny, 2, 3)

    return { rg, nx, ny, x0, y0, cell, channels: 3, samples: xs.length, thickness, growScale }
}

/// Marching squares at iso value `iso` over field f (ny rows × nx cols, grid
/// spacing h, origin x0/y0).  Returns closed contours as arrays of [x,y].
/// Assumes the field border is below every iso used (padded), so all contours close.
function marchingSquares(f, nx, ny, x0, y0, h, iso) {
    // Key each contour segment by its start/end cell-edge so successive
    // segments chain into polylines.  Edge id: (edge orientation, ix, iy).
    const segStarts = new Map() // edgeKey -> next edgeKey
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

/// Grow strokes into a field and contour it (the vector path: SVG, and later
/// OTF export).  For interactive preview prefer buildGrowthField + the
/// GrowCanvas shader, which shares the same field and rule.
///
/// strokes: [{ pts: [[x,y],...], closed: bool }] in font units (y up).
/// params:
///   thickness  – classic stroke thickness (rMin = thickness/2)
///   grow       – 0..1: how far strokes may bulge beyond classic (0 = classic offset)
///   growScale  – max extra radius at grow=1 (default 120)
///   gap        – whitespace channel preserved between opposing strokes
///   fuse       – 0..1: melt neighbouring glyphs together by relaxing the gap
///                (and forcing overlap) only where opposition is cross-glyph;
///                counters within a glyph stay open (default 0)
///   counterK   – geodesic opposition threshold (default 2.2*thickness + 40)
///   cell       – field grid spacing in font units (default 4)
///   isoLevels  – field iso values to contour, e.g. [0, -14, -30]; 0 is the ink edge,
///                negative values are outward keyline bands (default [0])
///   smoothPasses – extra 3×3 blur passes over f before contouring (default 1;
///                the dOpp channel is already smoothed once at field build)
///   onProgress – optional (0..1) callback across the field build (the bulk)
///
/// Returns { levels: [{ iso, contours }], bbox: {x0,y0,x1,y1}, stats }.
export function growStrokes(strokes, params = {}) {
    const thickness = params.thickness ?? 30
    const grow = params.grow ?? 0.5
    const growScale = params.growScale ?? 120
    const gap = params.gap ?? thickness * 0.8
    const fuse = params.fuse ?? 0
    const cell = params.cell ?? 4
    const isoLevels = params.isoLevels ?? [0]
    const smoothPasses = params.smoothPasses ?? 1
    const onProgress = params.onProgress

    const field = buildGrowthField(strokes, {
        thickness,
        growScale,
        counterK: params.counterK,
        cell,
        maxOutward: Math.max(1.5 * thickness, -Math.min(...isoLevels)),
        // Field build is the bulk; leave a tail for contouring.
        onProgress: onProgress ? (f => onProgress(0.95 * f)) : undefined,
    })
    if (!field) return { levels: isoLevels.map(iso => ({ iso, contours: [] })), bbox: null, stats: {} }

    const { rg, nx, ny, x0, y0 } = field
    const rMin = thickness / 2
    const rMax = rMin + grow * growScale
    // At full fuse, cross-glyph cells drop the gap and overlap by fuseMerge so
    // the two fronts guarantee a merged blob rather than merely kissing.
    const fuseMerge = rMin

    let f = new Float32Array(nx * ny)
    for (let k = 0; k < nx * ny; k++) {
        const cross = rg[k * 3 + 2]
        const g = gap - cross * fuse * (gap + fuseMerge)
        const rAllowed = Math.max(rMin, Math.min(rMax, rg[k * 3 + 1] - g))
        f[k] = rAllowed - rg[k * 3]
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
    if (onProgress) onProgress(1)

    return {
        levels,
        bbox: { x0, y0, x1: x0 + (nx - 1) * cell, y1: y0 + (ny - 1) * cell },
        stats: { samples: field.samples, gridCells: nx * ny, rMin, rMax },
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
