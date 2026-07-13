// Grow tab back end (runs in the worker): extract spines for each text line
// and either build the two-channel growth field for the GPU preview
// (generateGrowthField) or grow + contour to a full SVG (generateGrowthSvg,
// the vector path and non-WebGL fallback).

import { textToStrokes } from './glyphSpines.js'
import { growStrokes, contoursToPath, buildGrowthField, layerIsoLevels, LAYER_COLORS } from './growth.js'

const GROW_SCALE = 120

/// Lay out all lines of text and collect growth-ready strokes.
/// Lines share one field so neighbouring lines interact (and keep `gap`)
/// just like neighbouring glyphs; spacing assumes full growth so the layout
/// doesn't shift as `grow` changes.
/// onProgress(frac 0..1) is reported across all glyphs of all lines.
function collectStrokes(text, axes, cell, onProgress) {
    const lines = text.split('\n')
    const totalChars = lines.reduce((s, l) => s + (l.trim() ? l.length : 0), 0) || 1
    let doneChars = 0
    const allStrokes = []
    lines.forEach((line, li) => {
        if (!line.trim()) return
        const lineChars = line.length
        const res = textToStrokes(line, axes, cell * 2,
            onProgress ? (f => onProgress((doneChars + f * lineChars) / totalChars)) : undefined)
        doneChars += lineChars
        const lineH = (res.fontData.ascender - res.fontData.descender) + axes.leading + 2 * GROW_SCALE
        const yOff = -li * lineH
        for (const s of res.strokes) {
            allStrokes.push({ ...s, pts: s.pts.map(([x, y]) => [x, y + yOff]) })
        }
    })
    return allStrokes
}

// Spine extraction (per-glyph Fable solving) dominates over the field build,
// so it gets the larger slice of the progress bar.
const EXTRACT_SHARE = 0.6

/// Pick the field resolution: finer for short texts, coarser so long texts
/// stay responsive.
function cellFor(text) {
    const chars = text.replace(/\s/g, '').length
    return chars <= 10 ? 3 : chars <= 30 ? 4 : 6
}

/// Build the (d1, dOpp) field for the GPU preview.  Returns null for empty
/// text.  The result's rg buffer is transferable.  onProgress(0..1) spans
/// spine extraction then the field build.
export function generateGrowthField(text, axes, params = {}, onProgress) {
    const cell = params.cell ?? cellFor(text)
    const strokes = collectStrokes(text, axes, cell,
        onProgress ? (f => onProgress(EXTRACT_SHARE * f)) : undefined)
    if (strokes.length === 0) return null
    return buildGrowthField(strokes, {
        thickness: axes.thickness,
        growScale: GROW_SCALE,
        cell,
        onProgress: onProgress ? (f => onProgress(EXTRACT_SHARE + (1 - EXTRACT_SHARE) * f)) : undefined,
    })
}

/// Grow + contour to a standalone SVG string (non-WebGL fallback and the
/// offline sample renderer).  onProgress(0..1) spans extraction then growth.
export function generateGrowthSvg(text, axes, params = {}, onProgress) {
    const grow = params.grow ?? 0.7
    const gap = params.gap ?? 30
    const layers = params.layers ?? true
    const thickness = axes.thickness
    const cell = params.cell ?? cellFor(text)

    const allStrokes = collectStrokes(text, axes, cell,
        onProgress ? (f => onProgress(EXTRACT_SHARE * f)) : undefined)
    if (allStrokes.length === 0) return ''

    const isoLevels = layers ? layerIsoLevels(thickness) : [0]
    const colors = layers ? LAYER_COLORS : ['black']

    const g = growStrokes(allStrokes, {
        thickness, grow, growScale: GROW_SCALE, gap, cell, isoLevels,
        onProgress: onProgress ? (f => onProgress(EXTRACT_SHARE + (1 - EXTRACT_SHARE) * f)) : undefined,
    })
    if (!g.bbox) return ''

    // Outermost level first so the ink core paints on top.
    let paths = ''
    for (let i = g.levels.length - 1; i >= 0; i--) {
        const d = contoursToPath(g.levels[i].contours)
        if (d) paths += `<path d="${d}" fill="${colors[i]}" fill-rule="evenodd"/>`
    }

    const { x0, y0, x1, y1 } = g.bbox
    const w = x1 - x0
    const h = y1 - y0
    // contoursToPath flips y for SVG, so the viewBox y range flips too.
    return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${x0.toFixed(1)} ${(-y1).toFixed(1)} ${w.toFixed(1)} ${h.toFixed(1)}" width="${(w / 2).toFixed(0)}" height="${(h / 2).toFixed(0)}">${paths}</svg>`
}
