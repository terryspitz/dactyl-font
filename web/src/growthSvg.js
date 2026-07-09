// Assemble a full SVG for the Grow tab: extract spines for each text line,
// grow them with the constant-gap inflation field, and paint the iso levels.

import { textToStrokes } from './glyphSpines.js'
import { growStrokes, contoursToPath } from './growth.js'

// Techno-Drive-style layer stack, innermost (ink core, iso 0) first.
const LAYER_COLORS = ['#f4fbff', '#7ec4ee', '#1660c8', '#0a0a14']

export function generateGrowthSvg(text, axes, params = {}) {
    const grow = params.grow ?? 0.7
    const gap = params.gap ?? 30
    const layers = params.layers ?? true
    const thickness = axes.thickness

    const lines = text.split('\n')
    const totalChars = lines.reduce((s, l) => s + l.length, 0)
    if (totalChars === 0) return ''
    // Finer field for short texts, coarser so long texts stay responsive.
    const cell = params.cell ?? (totalChars <= 10 ? 3 : totalChars <= 30 ? 4 : 6)

    // Gather strokes for all lines into one field so neighbouring lines
    // interact (and keep `gap`) just like neighbouring glyphs.
    const growScale = 120
    const allStrokes = []
    let fontData = null
    lines.forEach((line, li) => {
        if (!line) return
        const res = textToStrokes(line, axes, cell * 2)
        fontData = fontData ?? res.fontData
        const lineH = (res.fontData.ascender - res.fontData.descender) + axes.leading + 2 * grow * growScale
        const yOff = -li * lineH
        for (const s of res.strokes) {
            allStrokes.push({ ...s, pts: s.pts.map(([x, y]) => [x, y + yOff]) })
        }
    })
    if (allStrokes.length === 0) return ''

    const isoLevels = layers
        ? [0, -0.4 * thickness, -0.9 * thickness, -1.5 * thickness]
        : [0]
    const colors = layers ? LAYER_COLORS : ['black']

    const g = growStrokes(allStrokes, { thickness, grow, growScale, gap, cell, isoLevels })
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
