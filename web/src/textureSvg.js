// Generate tab's Texture mode back end (runs in the worker): patterns
// confined to the interior of a grown letterform (docs/growth-brainstorm.md,
// Idea 3 + extensions — see texture.js for the algorithms).  Like Grow
// (branching) mode, this is worker-computed SVG only: reaction-diffusion,
// maze-carving, and circuit-routing are all iterative/combinatorial, not a
// simple per-pixel threshold, so there's no WebGL preview path here.

import { collectStrokes } from './growthSvg.js'
import { contoursToPath } from './growth.js'
import {
    buildTextureMask, reactionDiffusion, contourScalarField, defaultRdThreshold,
    maze, wallsToSvgPath,
    circuit, circuitToSvg, DEFAULT_CIRCUIT_TRACE_COLOR, DEFAULT_CIRCUIT_PAD_COLOR,
} from './texture.js'

const GROW_SCALE = 120

/// Pick the mask's grid resolution: reaction-diffusion wants a fine field for
/// organic detail; maze/circuit want a much coarser one so corridors/traces
/// read as a pattern instead of grid noise (and stay fast — both are O(cells)
/// but a maze/circuit cell is "worth" far more visually at low cell counts
/// than an RD cell is).
function cellFor(text, style) {
    const chars = text.replace(/\s/g, '').length
    if (style === 'rd') return chars <= 10 ? 4 : chars <= 30 ? 5 : 7
    return chars <= 10 ? 14 : chars <= 30 ? 18 : 24
}

/// Render text's grown-letterform texture pattern to a standalone SVG
/// string.  onProgress(0..1) spans spine extraction, the mask field build,
/// and the pattern algorithm itself (reaction-diffusion's simulation is the
/// only one of the three with a meaningfully long tail).
///
/// params:
///   style — 'rd' | 'maze' | 'circuit' (default 'rd').
///   grow, gap, fuse, growScale — shape the mask domain (growStrokes rule).
///   backbone — show the classic (grow=0) outline underneath as context
///     (default true).
///   backboneColor, color — outline fill / pattern colour (style-dependent
///     defaults; maze uses `color` for its wall stroke, rd for its fill).
///   RD: preset, steps, seedFrac, seed, threshold (default: derived from the
///     field's own mean — see texture.js's defaultRdThreshold).
///   maze: seed, strokeWidth.
///   circuit: seed, density, maxLength, straightBias, traceColor, padColor,
///     strokeWidth, padSize.
export function generateTextureSvg(text, axes, params = {}, onProgress) {
    const style = params.style ?? 'rd'
    const grow = params.grow ?? 0.6
    const gap = params.gap ?? 25
    const fuse = params.fuse ?? 0
    const growScale = params.growScale ?? GROW_SCALE
    const thickness = axes.weight
    const cell = params.cell ?? cellFor(text, style)
    const showBackbone = params.backbone ?? true
    const backboneColor = params.backboneColor ?? '#e8e8e8'

    const EXTRACT_SHARE = 0.4
    const strokes = collectStrokes(text, axes, cell, growScale,
        onProgress ? (f => onProgress(EXTRACT_SHARE * f)) : undefined)
    if (strokes.length === 0) return ''

    const built = buildTextureMask(strokes, {
        thickness, grow, growScale, gap, fuse, cell,
        onProgress: onProgress ? (f => onProgress(EXTRACT_SHARE + 0.2 * f)) : undefined,
    })
    if (!built) return ''
    const { mask, f, nx, ny, x0, y0 } = built

    const backboneContours = showBackbone
        ? contourScalarField(f, nx, ny, x0, y0, cell, 0)
        : []
    const backboneSvg = backboneContours.length
        ? `<path d="${contoursToPath(backboneContours)}" fill="${backboneColor}" fill-rule="evenodd"/>`
        : ''

    let patternSvg = ''
    const patternProgress = onProgress ? (frac => onProgress(0.6 + 0.4 * frac)) : undefined
    if (style === 'maze') {
        const { walls } = maze(mask, nx, ny, x0, y0, cell, { seed: params.seed })
        const color = params.color ?? '#222222'
        const strokeWidth = params.strokeWidth ?? Math.max(1.5, cell * 0.16)
        patternSvg = walls.length
            ? `<path d="${wallsToSvgPath(walls)}" stroke="${color}" stroke-width="${strokeWidth}" fill="none" stroke-linecap="square"/>`
            : ''
        if (patternProgress) patternProgress(1)
    } else if (style === 'circuit') {
        const { traces, pads } = circuit(mask, nx, ny, x0, y0, cell, {
            seed: params.seed, density: params.density, maxLength: params.maxLength,
            straightBias: params.straightBias,
        })
        patternSvg = circuitToSvg(traces, pads, {
            traceColor: params.traceColor ?? DEFAULT_CIRCUIT_TRACE_COLOR,
            padColor: params.padColor ?? DEFAULT_CIRCUIT_PAD_COLOR,
            strokeWidth: params.strokeWidth ?? Math.max(2, cell * 0.28),
            padSize: params.padSize,
        })
        if (patternProgress) patternProgress(1)
    } else {
        const V = reactionDiffusion(mask, nx, ny, {
            preset: params.preset, steps: params.steps, seedFrac: params.seedFrac, seed: params.seed,
            onProgress: patternProgress,
        })
        const threshold = params.threshold ?? defaultRdThreshold(V, mask)
        const contours = contourScalarField(V, nx, ny, x0, y0, cell, threshold)
        const color = params.color ?? '#ff6f4a'
        patternSvg = contours.length ? `<path d="${contoursToPath(contours)}" fill="${color}" fill-rule="evenodd"/>` : ''
    }
    if (onProgress) onProgress(1)

    const w = (nx - 1) * cell, h = (ny - 1) * cell
    if (w <= 0 || h <= 0) return ''
    return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${x0.toFixed(1)} ${(-(y0 + h)).toFixed(1)} ${w.toFixed(1)} ${h.toFixed(1)}" width="${(w / 2).toFixed(0)}" height="${(h / 2).toFixed(0)}">` +
        backboneSvg + patternSvg +
        `</svg>`
}
