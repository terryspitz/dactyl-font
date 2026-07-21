// Branch tab back end (runs in the worker): space-colonisation branching off
// the glyph spines (brainstorm docs/growth-brainstorm.md, Idea 5), rendered
// as twigs over the classic (grow=0) letterform outline.

import { collectStrokes } from './growthSvg.js'
import { growStrokes, contoursToPath } from './growth.js'
import { growBranches, branchesToSvgPaths } from './branching.js'

/// Pick the spine sampling resolution: finer for short texts, coarser so
/// long texts stay responsive (same rule as the Grow tab).
function cellFor(text) {
    const chars = text.replace(/\s/g, '').length
    return chars <= 10 ? 3 : chars <= 30 ? 4 : 6
}

/// Render text's glyph spines plus their grown branches to a standalone SVG
/// string.  onProgress(0..1) spans spine extraction, the classic outline,
/// and the branch simulation.
export function generateBranchSvg(text, axes, params = {}, onProgress) {
    const cell = params.cell ?? cellFor(text)
    const maxReach = params.maxReach ?? 140
    const thickness = axes.thickness

    const strokes = collectStrokes(text, axes, cell, maxReach,
        onProgress ? (f => onProgress(0.5 * f)) : undefined)
    if (strokes.length === 0) return ''

    const letter = growStrokes(strokes, { thickness, grow: 0, gap: thickness * 0.8, isoLevels: [0], cell })
    const letterPath = contoursToPath(letter.levels[0].contours)
    if (onProgress) onProgress(0.6)

    const { nodes, edges } = growBranches(strokes, { ...params, thickness, maxReach })
    const branchPaths = branchesToSvgPaths(edges, { thickness, color: params.color })
    if (onProgress) onProgress(1)

    let x0 = letter.bbox?.x0 ?? 0, y0 = letter.bbox?.y0 ?? 0
    let x1 = letter.bbox?.x1 ?? 0, y1 = letter.bbox?.y1 ?? 0
    for (const n of nodes) {
        if (n.x < x0) x0 = n.x; if (n.x > x1) x1 = n.x
        if (n.y < y0) y0 = n.y; if (n.y > y1) y1 = n.y
    }
    const pad = 8
    x0 -= pad; y0 -= pad; x1 += pad; y1 += pad
    const w = x1 - x0, h = y1 - y0
    if (w <= 0 || h <= 0) return ''

    return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${x0.toFixed(1)} ${(-y1).toFixed(1)} ${w.toFixed(1)} ${h.toFixed(1)}" width="${(w / 2).toFixed(0)}" height="${(h / 2).toFixed(0)}">` +
        (letterPath ? `<path d="${letterPath}" fill="black" fill-rule="evenodd"/>` : '') +
        branchPaths +
        `</svg>`
}
