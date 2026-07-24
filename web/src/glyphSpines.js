// Extract solved glyph spines as dense polylines, the seed geometry for growth.js.
//
// Uses the same Fable API surface as the Splines tab: parseGlyphToControlPoints
// gives each glyph's backbone control points, solveSplineEditor solves them into
// cubic bezier spines (including free "fit" coordinates), and we then flatten
// the cubics into polylines.  generateFontGlyphData supplies advance widths so
// glyphs can be laid out along a baseline.

import { parseGlyphToControlPoints, solveSplineEditor, generateFontGlyphData } from './lib/fable/Api.js'
import { DControlPoint } from './lib/fable/generator/DactylSpline.js'

/// Evaluate the cubic bezier segment between two solved BezierPoints at parameter t.
function bezEval(p1, p2, t) {
    const c1x = p1.x + p1.rd * Math.cos(p1.th_out)
    const c1y = p1.y + p1.rd * Math.sin(p1.th_out)
    const c2x = p2.x - p2.ld * Math.cos(p2.th_in)
    const c2y = p2.y - p2.ld * Math.sin(p2.th_in)
    const u = 1 - t
    return [
        u * u * u * p1.x + 3 * u * u * t * c1x + 3 * u * t * t * c2x + t * t * t * p2.x,
        u * u * u * p1.y + 3 * u * u * t * c1y + 3 * u * t * t * c2y + t * t * t * p2.y,
    ]
}

/// Flatten solved bezier points into a polyline sampled at roughly `spacing`.
function bezierToPolyline(bezPts, isClosed, spacing) {
    const n = bezPts.length
    if (n === 0) return []
    if (n === 1) return [[bezPts[0].x, bezPts[0].y]]
    const pts = [[bezPts[0].x, bezPts[0].y]]
    const segs = isClosed ? n : n - 1
    for (let i = 0; i < segs; i++) {
        const p1 = bezPts[i]
        const p2 = bezPts[(i + 1) % n]
        // Control-polygon length overestimates arc length: good enough for step count.
        const approxLen = p1.rd + p2.ld + Math.hypot(p2.x - p1.x, p2.y - p1.y)
        const steps = Math.min(64, Math.max(2, Math.ceil(approxLen / spacing)))
        for (let s = 1; s <= steps; s++) pts.push(bezEval(p1, p2, s / steps))
    }
    return pts
}

/// Solve one glyph's backbone curves into spine polylines (glyph coords, y up).
export function glyphToSpines(char, axes, spacing = 8) {
    const curves = parseGlyphToControlPoints(char, axes)
    const strokes = []
    for (const curve of curves) {
        const raw = Array.from(curve.points)
        if (raw.length === 1) {
            // A Dot element: single-point stroke.
            strokes.push({ pts: [[raw[0].x, raw[0].y]], closed: false, dot: true })
            continue
        }
        const ctrlPts = raw.map(p => new DControlPoint(
            p.ty,
            p.x == null ? undefined : p.x,
            p.y == null ? undefined : p.y,
            p.th_in == null ? undefined : p.th_in,
            p.th_out == null ? undefined : p.th_out,
        ))
        const solved = solveSplineEditor(ctrlPts, curve.isClosed, axes.max_spline_iter, axes.flatness, axes.end_flatness)
        const pts = bezierToPolyline(Array.from(solved.bezierPoints), curve.isClosed, spacing)
        if (pts.length > 0) strokes.push({ pts, closed: curve.isClosed, dot: false })
    }
    return strokes
}

/// Lay out a line of text along the baseline; returns growth-ready strokes
/// tagged with their glyph index, plus per-glyph advance data.
/// onProgress(frac 0..1) is called after each glyph is solved (solving each
/// glyph's splines via the Fable API is the dominant cost of growth).
export function textToStrokes(text, axes, spacing = 8, onProgress) {
    const fontData = generateFontGlyphData(axes, undefined)
    const advance = new Map()
    for (const g of fontData.glyphs) advance.set(g.unicode, g.advanceWidth)

    const spineCache = new Map()
    const strokes = []
    let x = 0
    for (let gi = 0; gi < text.length; gi++) {
        const ch = text[gi]
        if (!spineCache.has(ch)) spineCache.set(ch, glyphToSpines(ch, axes, spacing))
        if (onProgress) onProgress((gi + 1) / text.length)
        for (const stroke of spineCache.get(ch)) {
            strokes.push({
                pts: stroke.pts.map(([px, py]) => [px + x, py]),
                closed: stroke.closed,
                dot: stroke.dot,
                glyph: gi,
            })
        }
        x += advance.get(ch.charCodeAt(0)) ?? axes.width + axes.tracking
    }
    return { strokes, width: x, fontData }
}
