// Texture mode (brainstorm Idea 3 + extensions): patterns confined to the
// interior of a grown letterform, using the Bubble mode's SDF field purely as
// a *domain mask* — "run reaction-diffusion inside the SDF band so patterns
// hug the letterform" (docs/growth-brainstorm.md, SDF direction #6), extended
// here to two more grid-restricted styles beyond reaction-diffusion:
//
//   - reactionDiffusion: classic Gray-Scott, confined to the mask, producing
//     organic coral/spot/stripe/mitosis textures.
//   - maze: a perfect maze (recursive-backtracker) carved through the mask's
//     cell grid, rendered as the *uncarved* walls — rectilinear corridors
//     that trace the letterform's shape.
//   - circuit: orthogonal random-walk traces with corner pads, Manhattan-
//     routed through the mask — a PCB-trace look.
//
// All three share one domain: buildTextureMask rasterises "inside the grown
// letterform" (the same f = rAllowed - d1 >= 0 rule as growStrokes, via
// growth.js's fieldToF) onto a grid, coarser than the SDF field's own `cell`
// for maze/circuit (so corridors/traces read as a pattern, not grid noise),
// but reusing the *same* field machinery — no new spine/distance code needed.

import { buildGrowthField, fieldToF, marchingSquares, simplify } from './growth.js'
import { mulberry32 } from './branching.js'

/// Rasterise "inside the grown letterform" as a Uint8Array mask on a grid.
/// Reuses buildGrowthField + fieldToF (the exact same rule growStrokes uses),
/// so the pattern's domain always matches what the Bubble mode would draw for
/// the same grow/gap/fuse.
///
/// strokes: [{ pts: [[x,y],...], closed: bool, glyph?: int }], as from glyphSpines.js.
/// opts:
///   thickness, growScale, gap, fuse, grow — same meanings as growStrokes (grow
///     default 0.6: some bulge, so counters/margins have room for a pattern).
///   cell — grid spacing in font units; coarser reads as a chunkier pattern
///     (default 4, but maze/circuit want much coarser — see their own opts).
///   onProgress — optional (0..1), spans the field build.
///
/// Returns { mask, f, nx, ny, x0, y0, cell } or null for empty strokes; `f` is
/// kept so callers can also contour the classic outline (iso 0) for a
/// "backbone" reference, without rebuilding the field.
export function buildTextureMask(strokes, opts = {}) {
    const thickness = opts.thickness ?? 30
    const grow = opts.grow ?? 0.6
    const growScale = opts.growScale ?? 120
    const gap = opts.gap ?? thickness * 0.8
    const fuse = opts.fuse ?? 0
    const cell = opts.cell ?? 4

    const field = buildGrowthField(strokes, {
        thickness, growScale, cell,
        counterK: opts.counterK,
        onProgress: opts.onProgress,
    })
    if (!field) return null

    const f = fieldToF(field, { thickness, grow, growScale, gap, fuse })
    const { nx, ny, x0, y0 } = field
    const mask = new Uint8Array(nx * ny)
    for (let k = 0; k < nx * ny; k++) mask[k] = f[k] >= 0 ? 1 : 0
    return { mask, f, nx, ny, x0, y0, cell }
}

// Named Gray-Scott parameter sets (feed rate F, kill rate K; diffusion rates
// Du/Dv are the standard 0.16/0.08 for all of them).  Chosen and verified by
// actually rendering each at full text scale and eyeballing the result, not
// copied blind from a textbook cheat sheet: Gray-Scott is notoriously
// sensitive to F/K *and* to seeding (isolated point seeds decay to nothing in
// most of the parameter space — only a band near `coral` self-propagates from
// sparse points; everywhere else needs a large-enough nucleus, and a hard,
// letterform-shaped mask boundary shifts what "large enough" means versus the
// usual periodic-domain demos).  These three all reliably fill the whole
// mask from the same simple sparse single-cell seeding (see
// reactionDiffusion), which is why the set stops at three rather than
// chasing textbook names like "mitosis" that didn't reproduce here.
const RD_PRESETS = {
    coral:    { F: 0.0545, K: 0.0620 }, // thin branching veins
    cells:    { F: 0.0620, K: 0.0610 }, // honeycomb / cellular
    stitches: { F: 0.0500, K: 0.0650 }, // scattered dashes and dots
}
export const RD_PRESET_NAMES = Object.keys(RD_PRESETS)

/// Gray-Scott reaction-diffusion, confined to `mask` (V and U are held at
/// their rest state 0/1 outside it every step — a hard wall, not a periodic
/// or reflecting boundary, so the pattern presses right up to the letterform
/// edge rather than tapering into it).
///
/// mask, nx, ny: as from buildTextureMask.
/// opts:
///   preset — one of RD_PRESET_NAMES (default 'coral').
///   steps — iterations to run (default 3000; more = more developed pattern,
///     roughly linear cost).
///   seedFrac — fraction of mask cells that start fully activated (default
///     0.05); the reaction only takes hold from a *fully saturated* nucleus
///     (V=1, U=0) — small-amplitude noise decays back to the trivial state
///     for all three presets, so this isn't a "jitter strength" knob, it's a
///     density of all-or-nothing starting points.
///   seed — RNG seed for the random seed points (default 1).
///   onProgress — optional (0..1) across the simulation.
///
/// Returns V, a Float32Array on the same grid (0..~1), for contouring or
/// direct raster display.
export function reactionDiffusion(mask, nx, ny, opts = {}) {
    const { F, K } = RD_PRESETS[opts.preset] ?? RD_PRESETS.coral
    const Du = 0.16, Dv = 0.08
    const steps = opts.steps ?? 3000
    const seedFrac = opts.seedFrac ?? 0.05
    const seed = opts.seed ?? 1
    const onProgress = opts.onProgress
    const rng = mulberry32(seed)

    const n = nx * ny
    const U = new Float32Array(n).fill(1)
    const V = new Float32Array(n).fill(0)

    // Sparse fully-saturated single-cell seeds, scattered across the whole
    // mask by per-cell probability so coverage scales with area (a single
    // glyph and a full word both end up with comparably dense seeding).
    for (let k = 0; k < n; k++) {
        if (mask[k] && rng() < seedFrac) { V[k] = 1; U[k] = 0 }
    }

    const lap = (arr, ix, iy) => {
        const k = iy * nx + ix
        const l = arr[iy * nx + (ix > 0 ? ix - 1 : ix)]
        const r = arr[iy * nx + (ix < nx - 1 ? ix + 1 : ix)]
        const u = arr[(iy > 0 ? iy - 1 : iy) * nx + ix]
        const d = arr[(iy < ny - 1 ? iy + 1 : iy) * nx + ix]
        return l + r + u + d - 4 * arr[k]
    }

    let Un = new Float32Array(n), Vn = new Float32Array(n)
    const reportEvery = Math.max(1, Math.floor(steps / 40))
    for (let s = 0; s < steps; s++) {
        for (let iy = 0; iy < ny; iy++) {
            for (let ix = 0; ix < nx; ix++) {
                const k = iy * nx + ix
                if (!mask[k]) { Un[k] = 1; Vn[k] = 0; continue }
                const u = U[k], v = V[k]
                const uvv = u * v * v
                Un[k] = u + (Du * lap(U, ix, iy) - uvv + F * (1 - u))
                Vn[k] = v + (Dv * lap(V, ix, iy) + uvv - (F + K) * v)
            }
        }
        U.set(Un); V.set(Vn)
        if (onProgress && (s % reportEvery) === 0) onProgress(s / steps)
    }
    if (onProgress) onProgress(1)
    return V
}

/// Contour the reaction-diffusion V field at `threshold` into closed vector
/// contours (font units, y up) — thin wrapper over growth.js's marching
/// squares + RDP simplify, reused as-is (V is just another scalar field).
export function contourScalarField(V, nx, ny, x0, y0, cell, threshold, simplifyEps) {
    return marchingSquares(V, nx, ny, x0, y0, cell, threshold)
        .map(c => simplify(c, simplifyEps ?? cell * 0.12))
}

/// A good default contour threshold for a reaction-diffusion V field: a
/// fraction of V's own mean over the mask.  V's peak is always close to 1
/// regardless of preset (Gray-Scott's saturation), but its *mean* — and so
/// the useful iso-level that traces the pattern's ink rather than either its
/// bare ridge crests or its whole (near-full) extent — varies with the
/// preset and even the seed, so a fixed constant threshold is wrong for some
/// presets: 0.5 (the "obvious" middle-of-range guess) only ever captured a
/// sliver near the letterform's outer boundary in testing, nowhere near the
/// interior pattern.  Computing it from the field itself is robust to that.
export function defaultRdThreshold(V, mask) {
    let sum = 0, count = 0
    for (let k = 0; k < V.length; k++) {
        if (mask[k]) { sum += V[k]; count++ }
    }
    return count > 0 ? (sum / count) * 0.8 : 0.2
}

/// Label 4-connected components of `mask` (nx×ny).  Returns { comp, count }:
/// comp[k] is the component index of mask cell k (-1 if not mask); cells of
/// the same component are reachable from each other via mask-only N/S/E/W
/// steps.  Shared by maze and circuit — both need to run their own
/// grid-restricted algorithm independently per disconnected letterform piece
/// (e.g. the two strokes of an 'i', or separate letters that haven't fused).
function connectedComponents(mask, nx, ny) {
    const n = nx * ny
    const comp = new Int32Array(n).fill(-1)
    const cellsByComp = []
    const stack = []
    for (let start = 0; start < n; start++) {
        if (!mask[start] || comp[start] !== -1) continue
        const c = cellsByComp.length
        const cells = []
        comp[start] = c
        stack.length = 0
        stack.push(start)
        while (stack.length) {
            const k = stack.pop()
            cells.push(k)
            const ix = k % nx, iy = (k / nx) | 0
            if (ix > 0 && mask[k - 1] && comp[k - 1] === -1) { comp[k - 1] = c; stack.push(k - 1) }
            if (ix < nx - 1 && mask[k + 1] && comp[k + 1] === -1) { comp[k + 1] = c; stack.push(k + 1) }
            if (iy > 0 && mask[k - nx] && comp[k - nx] === -1) { comp[k - nx] = c; stack.push(k - nx) }
            if (iy < ny - 1 && mask[k + nx] && comp[k + nx] === -1) { comp[k + nx] = c; stack.push(k + nx) }
        }
        cellsByComp.push(cells)
    }
    return { comp, cellsByComp }
}

/// A perfect maze (recursive backtracker / randomised DFS) carved through
/// `mask`'s cell grid, rendered as the *uncarved* walls — so the maze
/// corridors are the letterform's own shape, and every disconnected piece of
/// the mask (e.g. the two strokes of an 'i', or separate un-fused letters)
/// gets its own independent maze, since a spanning tree can't cross between
/// them.
///
/// mask, nx, ny: as from buildTextureMask.  x0, y0, cell: the same grid's
/// font-unit origin/spacing, to emit walls directly in font units.
/// opts.seed — RNG seed (default 1).
///
/// Returns { walls: [[x1,y1,x2,y2], ...] } — unordered straight wall
/// segments in font units (y up); every mask-cell boundary that ISN'T a
/// carved corridor, including the letterform's own outer silhouette.
export function maze(mask, nx, ny, x0, y0, cell, opts = {}) {
    const rng = mulberry32(opts.seed ?? 1)
    const { comp, cellsByComp } = connectedComponents(mask, nx, ny)

    // Recursive backtracker per component; corridors recorded as a Set of
    // "min,max" linear-index edge keys (undirected, so lookup is symmetric).
    const edgeKey = (a, b) => (a < b ? a + ',' + b : b + ',' + a)
    const corridors = new Set()
    const visited = new Uint8Array(nx * ny)
    for (const cells of cellsByComp) {
        if (cells.length < 2) continue // an isolated single-cell speck: no corridor to carve
        const start = cells[0]
        visited[start] = 1
        const stack = [start]
        while (stack.length) {
            const cur = stack[stack.length - 1]
            const ix = cur % nx, iy = (cur / nx) | 0
            const options = []
            if (ix > 0 && mask[cur - 1] && !visited[cur - 1]) options.push(cur - 1)
            if (ix < nx - 1 && mask[cur + 1] && !visited[cur + 1]) options.push(cur + 1)
            if (iy > 0 && mask[cur - nx] && !visited[cur - nx]) options.push(cur - nx)
            if (iy < ny - 1 && mask[cur + nx] && !visited[cur + nx]) options.push(cur + nx)
            if (options.length === 0) { stack.pop(); continue }
            const next = options[Math.floor(rng() * options.length)]
            corridors.add(edgeKey(cur, next))
            visited[next] = 1
            stack.push(next)
        }
    }

    // Walls: every mask-cell edge that ISN'T a carved corridor.  East/North
    // are checked for both internal (neighbour is mask) and boundary
    // (neighbour isn't) walls — canonical direction so an internal wall
    // between two mask cells is emitted once, from its west/south side only.
    // West/South are checked for boundary walls only (the matching internal
    // wall there was already emitted by the neighbour's East/North check).
    const px = (ix) => x0 + ix * cell
    const py = (iy) => y0 + iy * cell
    const walls = []
    for (let iy = 0; iy < ny; iy++) {
        for (let ix = 0; ix < nx; ix++) {
            const k = iy * nx + ix
            if (!mask[k]) continue
            const cxp = px(ix), cyp = py(iy), h = cell / 2

            const eIx = ix + 1, eK = iy * nx + eIx
            if (eIx >= nx || !mask[eK]) walls.push([cxp + h, cyp - h, cxp + h, cyp + h])
            else if (!corridors.has(edgeKey(k, eK))) walls.push([cxp + h, cyp - h, cxp + h, cyp + h])

            const nIy = iy + 1, nK = nIy * nx + ix
            if (nIy >= ny || !mask[nK]) walls.push([cxp - h, cyp + h, cxp + h, cyp + h])
            else if (!corridors.has(edgeKey(k, nK))) walls.push([cxp - h, cyp + h, cxp + h, cyp + h])

            const wIx = ix - 1
            if (wIx < 0 || !mask[iy * nx + wIx]) walls.push([cxp - h, cyp - h, cxp - h, cyp + h])

            const sIy = iy - 1
            if (sIy < 0 || !mask[sIy * nx + ix]) walls.push([cxp - h, cyp - h, cxp + h, cyp - h])
        }
    }
    return { walls }
}

/// Render wall segments as a single SVG <path> of disconnected M/L subpaths
/// (font units, y flipped to SVG coords, mirroring contoursToPath).
export function wallsToSvgPath(walls, decimals = 1) {
    const fmt = (v) => v.toFixed(decimals).replace(/\.0$/, '')
    let d = ''
    for (const [x1, y1, x2, y2] of walls) {
        d += `M${fmt(x1)} ${fmt(-y1)}L${fmt(x2)} ${fmt(-y2)}`
    }
    return d
}

const DIRS4 = [[1, 0], [-1, 0], [0, 1], [0, -1]]

/// Fisher-Yates shuffle (in place) using `rng` (a () => [0,1) function).
function shuffle(arr, rng) {
    for (let i = arr.length - 1; i > 0; i--) {
        const j = Math.floor(rng() * (i + 1))
            ;[arr[i], arr[j]] = [arr[j], arr[i]]
    }
    return arr
}

/// PCB-trace-style pattern: orthogonal random-walk traces confined to `mask`,
/// Manhattan-routed (each step N/S/E/W) with a bias to keep going straight —
/// occasional turns instead of a pure random walk, so traces read as routed
/// rather than jittery.  Traces claim their cells (no two traces cross), and
/// pads mark every trace's endpoints and corners.
///
/// mask, nx, ny, x0, y0, cell: as from buildTextureMask.
/// opts:
///   density — fraction of mask cells tried as trace starts (default 0.12).
///   maxLength — longest a trace can grow, in grid steps (default 60).
///   straightBias — probability of continuing the current direction each
///     step rather than considering a new one (default 0.82).
///   minLength — traces shorter than this (grid steps) are discarded as
///     visual noise (default 3).
///   seed — RNG seed (default 1).
///
/// Returns { traces: [{ points: [[x,y],...] }], pads: [{x,y}] } in font units
/// (y up).
export function circuit(mask, nx, ny, x0, y0, cell, opts = {}) {
    const rng = mulberry32(opts.seed ?? 1)
    const density = opts.density ?? 0.12
    const maxLength = opts.maxLength ?? 60
    const straightBias = opts.straightBias ?? 0.82
    const minLength = opts.minLength ?? 3

    const n = nx * ny
    const occupied = new Uint8Array(n)
    const maskCells = []
    for (let k = 0; k < n; k++) if (mask[k]) maskCells.push(k)
    shuffle(maskCells, rng)
    const numStarts = Math.max(1, Math.round(maskCells.length * density))

    const paths = []
    for (let s = 0; s < numStarts; s++) {
        const start = maskCells[s]
        if (occupied[start]) continue
        let ix = start % nx, iy = (start / nx) | 0
        let dir = DIRS4[Math.floor(rng() * 4)]
        occupied[start] = 1
        const path = [[ix, iy]]

        for (let step = 0; step < maxLength; step++) {
            const preferred = rng() < straightBias ? dir : DIRS4[Math.floor(rng() * 4)]
            const order = shuffle(DIRS4.filter(d => d !== preferred), rng)
            order.unshift(preferred)
            let moved = false
            for (const [dx, dy] of order) {
                const nix = ix + dx, niy = iy + dy
                if (nix < 0 || nix >= nx || niy < 0 || niy >= ny) continue
                const nk = niy * nx + nix
                if (!mask[nk] || occupied[nk]) continue
                ix = nix; iy = niy; dir = [dx, dy]
                occupied[nk] = 1
                path.push([ix, iy])
                moved = true
                break
            }
            if (!moved) break
        }
        if (path.length >= minLength) paths.push(path)
    }

    const px = (ix) => x0 + ix * cell, py = (iy) => y0 + iy * cell
    const traces = []
    const pads = []
    for (const path of paths) {
        const points = path.map(([ix, iy]) => [px(ix), py(iy)])
        traces.push({ points })
        pads.push({ x: points[0][0], y: points[0][1] })
        pads.push({ x: points[points.length - 1][0], y: points[points.length - 1][1] })
        // Corner pads: grid points where the incoming/outgoing step direction changes.
        for (let i = 1; i < path.length - 1; i++) {
            const [ax, ay] = path[i - 1], [bx, by] = path[i], [cx, cy] = path[i + 1]
            if (bx - ax !== cx - bx || by - ay !== cy - by) pads.push({ x: px(bx), y: py(by) })
        }
    }
    return { traces, pads }
}

// Default PCB-ish palette: copper traces, darker pads.
export const DEFAULT_CIRCUIT_TRACE_COLOR = '#b5703c'
export const DEFAULT_CIRCUIT_PAD_COLOR = '#6b3f22'

/// Render circuit traces + pads to SVG markup (font units, y flipped).
export function circuitToSvg(traces, pads, opts = {}) {
    const traceColor = opts.traceColor ?? DEFAULT_CIRCUIT_TRACE_COLOR
    const padColor = opts.padColor ?? DEFAULT_CIRCUIT_PAD_COLOR
    const strokeWidth = opts.strokeWidth ?? 4
    const padSize = opts.padSize ?? strokeWidth * 2

    let d = ''
    for (const t of traces) {
        if (t.points.length < 2) continue
        d += `M${t.points[0][0].toFixed(1)} ${(-t.points[0][1]).toFixed(1)}`
        for (let i = 1; i < t.points.length; i++) {
            d += `L${t.points[i][0].toFixed(1)} ${(-t.points[i][1]).toFixed(1)}`
        }
    }
    const traceSvg = d
        ? `<path d="${d}" stroke="${traceColor}" stroke-width="${strokeWidth}" fill="none" stroke-linecap="round" stroke-linejoin="round"/>`
        : ''
    let padSvg = ''
    for (const p of pads) {
        const h = padSize / 2
        padSvg += `<rect x="${(p.x - h).toFixed(1)}" y="${(-p.y - h).toFixed(1)}" width="${padSize.toFixed(1)}" height="${padSize.toFixed(1)}" fill="${padColor}"/>`
    }
    return traceSvg + padSvg
}
