// Render sample SVGs of the constant-gap growth prototype.
//
//   node scripts/growth-samples.mjs [outDir]
//
// Writes: grow-sweep.svg (grow axis 0→1), gap-sweep.svg, layers.svg (Y2K
// layered keylines à la Techno Drive), and spines.svg (the raw seed spines).

import { mkdirSync, writeFileSync } from 'node:fs'
import { join } from 'node:path'
import { defaultAxes } from '../src/lib/fable/Api.js'
import { textToStrokes } from '../src/glyphSpines.js'
import { growStrokes, contoursToPath } from '../src/growth.js'

const outDir = process.argv[2] ?? 'growth-samples'
mkdirSync(outDir, { recursive: true })

const axes = { ...defaultAxes, dactyl_spline: true }

function svgDoc(inner, x0, y0, w, h, bg = 'white') {
    return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${x0} ${y0} ${w} ${h}" width="${Math.round(w / 2)}" height="${Math.round(h / 2)}">
<rect x="${x0}" y="${y0}" width="${w}" height="${h}" fill="${bg}"/>
${inner}
</svg>`
}

function label(x, y, text, size = 40, fill = '#888') {
    return `<text x="${x}" y="${y}" font-family="sans-serif" font-size="${size}" fill="${fill}">${text}</text>`
}

const t0 = Date.now()

// --- 1. grow sweep -----------------------------------------------------------
{
    const text = 'grown'
    const { strokes, width } = textToStrokes(text, axes)
    let inner = ''
    const rowH = 1300
    const grows = [0, 0.2, 0.4, 0.6, 0.8, 1.0]
    grows.forEach((grow, i) => {
        const g = growStrokes(strokes, { thickness: axes.thickness, grow, gap: 25, cell: 4 })
        const path = contoursToPath(g.levels[0].contours)
        const yOff = i * rowH
        inner += `<g transform="translate(150 ${yOff + 900})"><path d="${path}" fill="black" fill-rule="evenodd"/></g>`
        inner += label(40, yOff + 250, `grow=${grow}`, 48)
        console.log(`grow=${grow}: ${g.stats.samples} samples, ${g.stats.gridCells} cells, rMax=${g.stats.rMax}`)
    })
    writeFileSync(join(outDir, 'grow-sweep.svg'), svgDoc(inner, 0, 0, width + 400, grows.length * rowH))
    console.log('grow-sweep.svg done', Date.now() - t0, 'ms')
}

// --- 2. gap sweep -------------------------------------------------------------
{
    const text = 'adhesion'
    const { strokes, width } = textToStrokes(text, axes)
    let inner = ''
    const rowH = 1300
    const gaps = [10, 25, 45, 70]
    gaps.forEach((gap, i) => {
        const g = growStrokes(strokes, { thickness: axes.thickness, grow: 0.9, gap, cell: 4 })
        const path = contoursToPath(g.levels[0].contours)
        const yOff = i * rowH
        inner += `<g transform="translate(150 ${yOff + 900})"><path d="${path}" fill="black" fill-rule="evenodd"/></g>`
        inner += label(40, yOff + 250, `gap=${gap}`, 48)
    })
    writeFileSync(join(outDir, 'gap-sweep.svg'), svgDoc(inner, 0, 0, width + 400, gaps.length * rowH))
    console.log('gap-sweep.svg done', Date.now() - t0, 'ms')
}

// --- 2b. fuse sweep (melt neighbouring letters into a logotype) --------------
{
    const text = 'techno'
    const { strokes, width } = textToStrokes(text, axes)
    let inner = ''
    const rowH = 1300
    const fuses = [0, 0.35, 0.7, 1.0]
    fuses.forEach((fuse, i) => {
        const g = growStrokes(strokes, { thickness: axes.thickness, grow: 0.9, gap: 30, fuse, cell: 4 })
        const path = contoursToPath(g.levels[0].contours)
        const yOff = i * rowH
        inner += `<g transform="translate(150 ${yOff + 900})"><path d="${path}" fill="black" fill-rule="evenodd"/></g>`
        inner += label(40, yOff + 250, `fuse=${fuse}`, 48)
    })
    writeFileSync(join(outDir, 'fuse-sweep.svg'), svgDoc(inner, 0, 0, width + 400, fuses.length * rowH))
    console.log('fuse-sweep.svg done', Date.now() - t0, 'ms')
}

// --- 3. layered Y2K keylines ---------------------------------------------------
{
    const text = 'dactyl'
    const { strokes, width } = textToStrokes(text, axes)
    const g = growStrokes(strokes, {
        thickness: axes.thickness, grow: 0.85, gap: 30, cell: 3,
        isoLevels: [0, -12, -26, -44],
    })
    // Painter's algorithm: outermost band (last iso) first, ink core (iso 0)
    // last.  colors[i] matches isoLevels[i]: near-white core, light blue, dark
    // blue, black keyline — the Techno Drive stack.
    const colors = ['#f4fbff', '#7ec4ee', '#1660c8', '#0a0a14']
    let inner = ''
    for (let i = g.levels.length - 1; i >= 0; i--) {
        const path = contoursToPath(g.levels[i].contours)
        inner += `<path d="${path}" fill="${colors[i]}" fill-rule="evenodd"/>`
    }
    writeFileSync(join(outDir, 'layers.svg'),
        svgDoc(`<g transform="translate(150 900)">${inner}</g>`, 0, 0, width + 300, 1500, 'white'))
    console.log('layers.svg done', Date.now() - t0, 'ms')
}

// --- 4. raw spines (what the growth starts from) -------------------------------
{
    const text = 'grown'
    const { strokes, width } = textToStrokes(text, axes)
    let inner = ''
    for (const s of strokes) {
        const d = s.pts.map(([x, y], i) => `${i ? 'L' : 'M'}${x.toFixed(1)} ${(-y).toFixed(1)}`).join('') + (s.closed ? 'Z' : '')
        inner += s.pts.length === 1
            ? `<circle cx="${s.pts[0][0]}" cy="${-s.pts[0][1]}" r="8" fill="crimson"/>`
            : `<path d="${d}" fill="none" stroke="crimson" stroke-width="4"/>`
    }
    writeFileSync(join(outDir, 'spines.svg'),
        svgDoc(`<g transform="translate(150 750)">${inner}</g>`, 0, 0, width + 300, 1100))
    console.log('spines.svg done', Date.now() - t0, 'ms')
}
