// Pure-SVG rebuild of the Spline Grid tab's report (see SplineGrid.jsx for
// the on-screen HTML-table version) for export. Nested <svg> per cell is
// plain, well-formed SVG (unlike a <foreignObject>-wrapped DOM screenshot,
// which — confirmed by hand — unconditionally taints the canvas in Chromium,
// even with no embedded images at all), so this renders straight to a canvas
// via the same svgToPngBlob pipeline every other tab's export already uses.

const TYPE_SHORT = ['∧', 'G2', 'L→C', 'C→L'] // ∧, G2, L→C, C→L
const CELL_VIEWBOX = '-50 -280 400 310'
const CELL_W = 90
const CELL_H = 70
const ROW_LABEL_W = 100
const HEADER_H = 28
const COLS_PER_GROUP = 4

const isValidPath = (p) => p && !p.includes('E+') && !p.includes('E-') && !p.includes('NaN') && !p.includes('Infinity')

const esc = (s) => String(s ?? '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')

function cellSvg(x, y, cell) {
    const pathSvg = cell?.pathSvg ?? ''
    const error = cell?.error ?? ''
    const valid = isValidPath(pathSvg)
    let inner
    if (error) {
        inner = `<text x="150" y="-110" text-anchor="middle" font-size="70" fill="#cc0000">✗</text>` +
            `<text x="150" y="-55" text-anchor="middle" font-size="25" fill="#cc0000">${esc(error.split(':')[0])}</text>`
    } else if (valid) {
        inner = `<g transform="scale(1,-1)"><path d="${esc(pathSvg)}" fill="none" stroke="#3366cc" stroke-width="5"/></g>`
    } else {
        inner = `<text x="150" y="-100" text-anchor="middle" font-size="80" fill="#000">✗</text>`
    }
    return `<svg x="${x}" y="${y}" width="${CELL_W}" height="${CELL_H}" viewBox="${CELL_VIEWBOX}">` +
        `<rect x="-50" y="-280" width="400" height="310" fill="#ffffff"/>` +
        `<polygon points="50,-50 250,-50 150,-200" fill="none" stroke="#e0e0e0" stroke-width="3" stroke-dasharray="6,4"/>` +
        inner +
        `<circle cx="50" cy="-50" r="5" fill="#cc3333"/>` +
        `<circle cx="250" cy="-50" r="5" fill="#cc3333"/>` +
        `<circle cx="150" cy="-200" r="5" fill="#33aa33"/>` +
        `</svg>`
}

/// Build a standalone SVG string mirroring the Spline Grid tab's report:
/// two sections (no-tangent / with-tangent), each an 16-row x 8-col grid of
/// tiny triangle+curve diagrams. `gridData` is solveSplineGrid's raw result
/// (the same data SplineGrid.jsx renders as an HTML table).
export function buildSplineGridSvg(gridData) {
    const getCell = (isClosed, withTangent, t0, t1, t2) =>
        gridData.find(d => d.isClosed === isClosed && d.withTangent === withTangent &&
            d.types[0] === t0 && d.types[1] === t1 && d.types[2] === t2)

    const gridW = ROW_LABEL_W + COLS_PER_GROUP * 2 * CELL_W
    const sectionHeaderH = 40
    const colHeaderH = HEADER_H * 2
    const rowsH = 16 * CELL_H
    const sectionH = sectionHeaderH + colHeaderH + rowsH
    const topTextH = 110
    const sectionGap = 30
    const totalH = topTextH + sectionH * 2 + sectionGap

    let body = ''
    body += `<text x="0" y="24" font-size="22" font-weight="bold" fill="#111">Spline Grid</text>`
    body += `<text x="0" y="50" font-size="13" fill="#333">All 4³=64 combinations of point types on an isosceles triangle. P0=bottom-left (red), P1=bottom-right (red), P2=apex (green).</text>`
    body += `<text x="0" y="70" font-size="12" fill="#555">Types: ∧=Corner · G2=Smooth · L→C=LineToCurve · C→L=CurveToLine</text>`

    const sections = [
        { withTangent: false, label: 'No Tangent' },
        { withTangent: true, label: 'With Horizontal Tangent at Apex' },
    ]

    sections.forEach((section, si) => {
        const y0 = topTextH + si * (sectionH + sectionGap)
        body += `<text x="0" y="${y0 + 20}" font-size="16" font-weight="bold" fill="#111">${esc(section.label)}</text>`
        const headerY = y0 + sectionHeaderH
        // Group headers: Open / Closed
        body += `<rect x="${ROW_LABEL_W}" y="${headerY}" width="${COLS_PER_GROUP * CELL_W}" height="${HEADER_H}" fill="#d0dff0" stroke="#ccc"/>`
        body += `<text x="${ROW_LABEL_W + COLS_PER_GROUP * CELL_W / 2}" y="${headerY + HEADER_H * 0.7}" text-anchor="middle" font-size="12" font-weight="bold" fill="#111">Open</text>`
        body += `<rect x="${ROW_LABEL_W + COLS_PER_GROUP * CELL_W}" y="${headerY}" width="${COLS_PER_GROUP * CELL_W}" height="${HEADER_H}" fill="#d0f0d8" stroke="#ccc"/>`
        body += `<text x="${ROW_LABEL_W + COLS_PER_GROUP * CELL_W * 1.5}" y="${headerY + HEADER_H * 0.7}" text-anchor="middle" font-size="12" font-weight="bold" fill="#111">Closed</text>`
        // Column headers: P2=<type> per open/closed
        for (let g = 0; g < 2; g++) {
            for (let c = 0; c < COLS_PER_GROUP; c++) {
                const cx = ROW_LABEL_W + (g * COLS_PER_GROUP + c) * CELL_W
                const bg = g === 0 ? '#e8f0fc' : '#e8f5ec'
                body += `<rect x="${cx}" y="${headerY + HEADER_H}" width="${CELL_W}" height="${HEADER_H}" fill="${bg}" stroke="#ccc"/>`
                body += `<text x="${cx + CELL_W / 2}" y="${headerY + HEADER_H * 1.7}" text-anchor="middle" font-size="11" font-weight="bold" fill="#111">P2=${esc(TYPE_SHORT[c])}</text>`
            }
        }
        // Row label header cell
        body += `<rect x="0" y="${headerY}" width="${ROW_LABEL_W}" height="${colHeaderH}" fill="#e8e8e8" stroke="#ccc"/>`
        body += `<text x="8" y="${headerY + colHeaderH * 0.6}" font-size="12" font-weight="bold" fill="#111">P0/P1</text>`

        const rowsY0 = headerY + colHeaderH
        let r = 0
        for (let t0 = 0; t0 < 4; t0++) {
            for (let t1 = 0; t1 < 4; t1++) {
                const ry = rowsY0 + r * CELL_H
                const rowBg = t1 === 0 ? '#e8e8e8' : '#f8f8f8'
                body += `<rect x="0" y="${ry}" width="${ROW_LABEL_W}" height="${CELL_H}" fill="${rowBg}" stroke="#ccc"/>`
                body += `<text x="8" y="${ry + CELL_H / 2 + 4}" font-size="12" fill="#111">${esc(TYPE_SHORT[t0])}/${esc(TYPE_SHORT[t1])}</text>`
                for (let g = 0; g < 2; g++) {
                    const isClosed = g === 1
                    for (let t2 = 0; t2 < 4; t2++) {
                        const cx = ROW_LABEL_W + (g * COLS_PER_GROUP + t2) * CELL_W
                        const cell = getCell(isClosed, section.withTangent, t0, t1, t2)
                        body += cellSvg(cx, ry, cell)
                    }
                }
                r++
            }
        }
    })

    return `<svg xmlns="http://www.w3.org/2000/svg" width="${gridW}" height="${totalH}" viewBox="0 0 ${gridW} ${totalH}">` +
        `<rect width="${gridW}" height="${totalH}" fill="#ffffff"/>` +
        body +
        `</svg>`
}
