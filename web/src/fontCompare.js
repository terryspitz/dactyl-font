import opentype from 'opentype.js'
import { parseSvgPath } from './fontExport'

/**
 * Font comparison helpers for the Visual Diffs "external font" mode.
 *
 * Two render strategies are supported, picked automatically per source:
 *
 *  - 'outline': we have the actual font file bytes, so opentype.js gives us
 *    real glyph beziers and we overlay them on Dactyl's outlines as vectors.
 *    Used for uploads, font URLs, system fonts (Chromium Local Font Access
 *    blobs) and curated Google Fonts TTFs.
 *  - 'text': we can only name the font for CSS (a Google family loaded via the
 *    Google Fonts stylesheet, or a system font we couldn't read bytes for).
 *    Rendered as overlaid HTML text by the React view rather than as SVG.
 */

// ---------------------------------------------------------------------------
// Loading fonts from the various sources
// ---------------------------------------------------------------------------

function parseBuffer(buffer, displayName) {
  // opentype throws on woff2 (brotli) — surface a clear message in that case.
  const font = opentype.parse(buffer)
  return { kind: 'outline', font, displayName }
}

export async function loadFontFromFile(file) {
  const buffer = await file.arrayBuffer()
  return parseBuffer(buffer, file.name.replace(/\.[^.]+$/, ''))
}

export async function loadFontFromUrl(url, displayName) {
  const res = await fetch(url)
  if (!res.ok) throw new Error(`Fetch failed (${res.status})`)
  const buffer = await res.arrayBuffer()
  return parseBuffer(buffer, displayName || url.split('/').pop())
}

/**
 * Enumerate locally installed fonts via the Local Font Access API (Chromium,
 * behind a permission prompt). Returns one entry per family with a handle to
 * the regular face so its bytes can be read on demand. Throws if unsupported.
 */
export async function querySystemFonts() {
  if (typeof window.queryLocalFonts !== 'function') {
    throw new Error('Local Font Access API not available in this browser')
  }
  const faces = await window.queryLocalFonts()
  const byFamily = new Map()
  for (const f of faces) {
    const existing = byFamily.get(f.family)
    // Prefer a plain "Regular" face when several styles exist.
    if (!existing || /regular/i.test(f.style)) byFamily.set(f.family, f)
  }
  return [...byFamily.entries()]
    .map(([family, face]) => ({ family, face }))
    .sort((a, b) => a.family.localeCompare(b.family))
}

export async function loadSystemFont(face) {
  const blob = await face.blob()
  const buffer = await blob.arrayBuffer()
  return parseBuffer(buffer, face.family)
}

// A small curated set of popular Google Fonts with stable raw-TTF URLs from the
// google/fonts GitHub repo, so outline overlay works without an API key.
// Anything else can be entered as a direct TTF URL, or by family name (text).
export const GOOGLE_FONTS = [
  { name: 'Roboto', path: 'apache/roboto/Roboto[wdth,wght].ttf' },
  { name: 'Open Sans', path: 'ofl/opensans/OpenSans[wdth,wght].ttf' },
  { name: 'Lato', path: 'ofl/lato/Lato-Regular.ttf' },
  { name: 'Montserrat', path: 'ofl/montserrat/Montserrat[wght].ttf' },
  { name: 'Poppins', path: 'ofl/poppins/Poppins-Regular.ttf' },
  { name: 'Inter', path: 'ofl/inter/Inter[opsz,wght].ttf' },
  { name: 'Raleway', path: 'ofl/raleway/Raleway[wght].ttf' },
  { name: 'Oswald', path: 'ofl/oswald/Oswald[wght].ttf' },
  { name: 'Nunito', path: 'ofl/nunito/Nunito[wght].ttf' },
  { name: 'Work Sans', path: 'ofl/worksans/WorkSans[wght].ttf' },
  { name: 'Playfair Display', path: 'ofl/playfairdisplay/PlayfairDisplay[wght].ttf' },
  { name: 'Roboto Slab', path: 'apache/robotoslab/RobotoSlab[wght].ttf' },
  { name: 'PT Serif', path: 'ofl/ptserif/PTSerif-Regular.ttf' },
  { name: 'Libre Baskerville', path: 'ofl/librebaskerville/LibreBaskerville-Regular.ttf' },
  { name: 'Bebas Neue', path: 'ofl/bebasneue/BebasNeue-Regular.ttf' },
  { name: 'Comfortaa', path: 'ofl/comfortaa/Comfortaa[wght].ttf' },
  { name: 'Josefin Sans', path: 'ofl/josefinsans/JosefinSans[wght].ttf' },
  { name: 'Dancing Script', path: 'ofl/dancingscript/DancingScript[wght].ttf' },
]

const GOOGLE_RAW = 'https://raw.githubusercontent.com/google/fonts/main/'

export async function loadGoogleFontOutline(entry) {
  // Bracketed variable-font filenames must be URL-encoded for the request.
  const url = GOOGLE_RAW + entry.path.split('/').map(encodeURIComponent).join('/')
  return loadFontFromUrl(url, entry.name)
}

/**
 * Inject the Google Fonts stylesheet for a family and return a 'text' source.
 * Reliable for any family name, but only supports CSS text rendering (no
 * vector outlines, since the delivered files are woff2).
 */
export function loadGoogleFontText(family) {
  const id = 'gf-' + family.replace(/\s+/g, '-').toLowerCase()
  if (!document.getElementById(id)) {
    const link = document.createElement('link')
    link.id = id
    link.rel = 'stylesheet'
    link.href = `https://fonts.googleapis.com/css2?family=${encodeURIComponent(family)}&display=swap`
    document.head.appendChild(link)
  }
  return { kind: 'text', fontFamily: `'${family}'`, displayName: family }
}

// ---------------------------------------------------------------------------
// Metrics & geometry
// ---------------------------------------------------------------------------

/** Bounding box of an SVG path string (Y-up font units), control points included. */
function svgPathBBox(d) {
  let minY = Infinity, maxY = -Infinity
  let minX = Infinity, maxX = -Infinity
  const upd = (x, y) => {
    if (x < minX) minX = x; if (x > maxX) maxX = x
    if (y < minY) minY = y; if (y > maxY) maxY = y
  }
  for (const c of parseSvgPath(d)) {
    if (c.type === 'C') { upd(c.x1, c.y1); upd(c.x2, c.y2); upd(c.x, c.y) }
    else if (c.type === 'M' || c.type === 'L') upd(c.x, c.y)
  }
  if (minY === Infinity) return null
  return { minX, minY, maxX, maxY }
}

/** Convert opentype path commands (Y-up font units) to an SVG path data string. */
function commandsToPathData(commands) {
  const r = (n) => Math.round(n * 100) / 100
  let d = ''
  for (const c of commands) {
    switch (c.type) {
      case 'M': d += `M ${r(c.x)} ${r(c.y)} `; break
      case 'L': d += `L ${r(c.x)} ${r(c.y)} `; break
      case 'C': d += `C ${r(c.x1)} ${r(c.y1)} ${r(c.x2)} ${r(c.y2)} ${r(c.x)} ${r(c.y)} `; break
      case 'Q': d += `Q ${r(c.x1)} ${r(c.y1)} ${r(c.x)} ${r(c.y)} `; break
      case 'Z': d += 'Z '; break
    }
  }
  return d.trim()
}

/** Reference metric (cap height / x-height / em) for the Dactyl glyph set. */
function dactylMetric(glyphMap, em, align) {
  if (align === 'em') return em
  const ch = align === 'x' ? 'x' : 'H'
  const g = glyphMap.get(ch.charCodeAt(0))
  const bbox = g && g.pathData ? svgPathBBox(g.pathData) : null
  if (bbox) return bbox.maxY
  return em * (align === 'x' ? 0.5 : 0.7)
}

/** Reference metric for an opentype font, preferring OS/2 table values. */
function fontMetric(font, align) {
  const em = font.unitsPerEm
  if (align === 'em') return em
  const os2 = font.tables && font.tables.os2
  if (align === 'x') {
    if (os2 && os2.sxHeight) return os2.sxHeight
    return glyphTop(font, 'x') ?? em * 0.5
  }
  if (os2 && os2.sCapHeight) return os2.sCapHeight
  return glyphTop(font, 'H') ?? em * 0.7
}

function glyphTop(font, ch) {
  try {
    const g = font.charToGlyph(ch)
    const b = g.getBoundingBox()
    return isFinite(b.y2) ? b.y2 : null
  } catch { return null }
}

// ---------------------------------------------------------------------------
// SVG overlay
// ---------------------------------------------------------------------------

const COLOR_A = 'rgba(255, 0, 0, 0.5)'   // Dactyl (red)
const COLOR_B = 'rgba(0, 0, 255, 0.5)'   // comparison font (blue)

/**
 * Build the 3-column-per-cell overlay SVG (Dactyl | comparison | overlaid),
 * mirroring the existing axis Visual Diffs layout. Dactyl is drawn at its
 * native scale; the comparison font is normalized to the chosen metric and
 * then multiplied by `sizeScale` (the user's Size slider, 1 = exact match).
 */
export function buildCompareOverlaySvg(dactylGlyphData, font, text, align = 'cap', labelB = 'Font', sizeScale = 1) {
  const em = dactylGlyphData.unitsPerEm
  const glyphMap = new Map(dactylGlyphData.glyphs.map(g => [g.unicode, g]))

  const metricD = dactylMetric(glyphMap, em, align)
  const metricE = fontMetric(font, align)
  const scaleD = 1
  const baseScaleE = metricE > 0 ? metricD / metricE : em / font.unitsPerEm
  const scaleE = baseScaleE * sizeScale

  // Columns are sized close to the glyph advance (plus a small margin) so the
  // glyphs fill the cell, matching the axis Visual Diffs layout instead of
  // floating in whitespace. The whole SVG is scaled to the container width, so
  // tighter cells render the glyphs larger.
  const subColW = em * 0.8
  const cellGap = em * 0.22
  const cellW = subColW * 3 + cellGap
  const cellH = em * 1.3
  const baseline = em * 0.98  // baseline offset from the top of a cell
  const pad = em * 0.08       // left pad inside each sub-column
  const cols = 5

  const chars = [...(text || '').replace(/[\n\r]/g, '')]
  const rows = Math.max(1, Math.ceil(chars.length / cols))

  // Reusable group for one glyph at (x) with the baseline at (y) and a Y-flip.
  const draw = (d, x, y, scale, color) =>
    d ? `<g transform="translate(${x.toFixed(1)},${y.toFixed(1)}) scale(${scale},${-scale})"><path d="${d}" fill="${color}" fill-rule="nonzero"/></g>` : ''

  const parts = []
  const keyY = em * 0.45
  parts.push(
    `<text x="0" y="${keyY.toFixed(0)}" font-size="${(em * 0.2).toFixed(0)}" fill="#444">` +
    `Key: Left = Dactyl, Middle = ${escapeXml(labelB)}, Right = Overlaid (Red = Dactyl, Blue = ${escapeXml(labelB)})</text>`
  )

  chars.forEach((ch, i) => {
    const col = i % cols
    const row = Math.floor(i / cols)
    const cellX = col * cellW
    const cellY = (row + 1) * cellH      // +1 leaves a row at top for the key
    const by = cellY + baseline

    const code = ch.codePointAt(0)
    const dg = glyphMap.get(code)
    const dPath = dg ? dg.pathData : ''

    let ePath = ''
    try { ePath = commandsToPathData(font.charToGlyph(ch).path.commands) } catch { /* missing glyph */ }

    const xA = cellX + pad
    const xB = cellX + subColW + pad
    const xO = cellX + subColW * 2 + pad

    // Char label, top-left of the cell.
    parts.push(`<text x="${cellX.toFixed(0)}" y="${(cellY + em * 0.22).toFixed(0)}" font-size="${(em * 0.16).toFixed(0)}" fill="#999">${escapeXml(ch)}</text>`)

    parts.push(draw(dPath, xA, by, scaleD, 'black'))           // col 1: Dactyl
    parts.push(draw(ePath, xB, by, scaleE, 'black'))           // col 2: comparison
    parts.push(draw(dPath, xO, by, scaleD, COLOR_A))           // col 3: overlay (Dactyl red)
    parts.push(draw(ePath, xO, by, scaleE, COLOR_B))           //          + comparison blue
  })

  const width = cols * cellW
  const height = (rows + 1) * cellH
  return (
    `<svg xmlns="http://www.w3.org/2000/svg" viewBox="${(-em * 0.1).toFixed(0)} 0 ${(width + em * 0.2).toFixed(0)} ${height.toFixed(0)}" ` +
    `width="${(width + em * 0.2).toFixed(0)}" height="${height.toFixed(0)}">` +
    parts.join('') +
    `</svg>`
  )
}

function escapeXml(s) {
  return String(s).replace(/[<>&]/g, c => ({ '<': '&lt;', '>': '&gt;', '&': '&amp;' }[c]))
}
