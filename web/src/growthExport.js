// Save helpers for the Generate tab's two modes (Bubble and Grow).  Both
// modes' preview paths render from the same vector SVG the worker produces,
// so downloads are driven from that: deterministic, resolution-independent,
// and available even without WebGL2.  PNG is that SVG rasterised at high res.

/// Trigger a browser download of `blob` as `filename`.
export function downloadBlob(blob, filename) {
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = filename
    document.body.appendChild(a)
    a.click()
    a.remove()
    URL.revokeObjectURL(url)
}

/// Some F#-generated SVGs (e.g. the Visual Diffs / Font renderers) label a
/// glyph with its own raw character, including punctuation like `&` and `<`
/// themselves — e.g. `<text>&</text>` or `<text><</text>`, both invalid XML.
/// The on-screen render (dangerouslySetInnerHTML, parsed as lenient HTML)
/// tolerates this fine; loading the same string via <img> (for canvas
/// rasterisation) parses it as strict XML and fails outright ("SVG failed to
/// load as image" / "StartTag: invalid element name"), confirmed by hand.
/// Round-tripping through the lenient HTML parser and re-serialising with
/// XMLSerializer (which always produces well-formed XML) fixes this
/// generally, the same way the DOM already tolerates it on screen — cheaper
/// targeted regex fixes for just `&` don't extend to a bare `<`, which can't
/// be told apart from a real tag start without actually parsing.
function sanitizeSvgForXml(svg) {
    if (typeof document === 'undefined') return svg // unit tests run in a DOM-less Node env
    const container = document.createElement('div')
    container.innerHTML = svg
    const svgEl = container.querySelector('svg')
    return svgEl ? new XMLSerializer().serializeToString(svgEl) : svg
}

/// A downloadable Blob for an SVG string.
export function svgBlob(svg) {
    return new Blob([sanitizeSvgForXml(svg)], { type: 'image/svg+xml;charset=utf-8' })
}

/// Rasterise an SVG string to a PNG Blob.
///   scale       – device-pixel multiplier over the SVG's natural pixel size
///   background  – CSS colour to fill first, or null for transparent
///   maxDim      – clamp the longest side (keeps huge words from OOMing)
/// The SVG's natural pixel size, from its width/height attributes, or failing
/// that its viewBox.  The F# generator's toSvgDocument emits a viewBox only, so
/// without the fallback a wide line of text would rasterise into a 1000x1000
/// box and come out letterboxed with a slab of empty space.
export function svgPixelSize(svg, fallback = 1000) {
    const root = svg.match(/<svg[^>]*>/)?.[0] ?? ''
    const attr = (name) => {
        const m = root.match(new RegExp(`\\b${name}=["']([\\d.]+)`))
        return m ? parseFloat(m[1]) : null
    }
    const viewBox = root.match(/\bviewBox=["']\s*([-\d.]+)[\s,]+([-\d.]+)[\s,]+([\d.]+)[\s,]+([\d.]+)/)
    const vbW = viewBox ? parseFloat(viewBox[3]) : null
    const vbH = viewBox ? parseFloat(viewBox[4]) : null
    return {
        w: attr('width') ?? vbW ?? fallback,
        h: attr('height') ?? vbH ?? fallback,
    }
}

export function svgToPngBlob(svg, { scale = 3, background = null, maxDim = 4096 } = {}) {
    svg = sanitizeSvgForXml(svg)
    return new Promise((resolve, reject) => {
        const { w, h } = svgPixelSize(svg)
        let s = scale
        if (w * s > maxDim || h * s > maxDim) s = Math.min(maxDim / w, maxDim / h)
        const cw = Math.max(1, Math.round(w * s))
        const ch = Math.max(1, Math.round(h * s))

        // A blob: URL rather than a data: URI — data URIs silently fail to
        // load as an <img> past roughly 2MB (hit by SplineGrid/Tweens'
        // composite exports, which embed many rasterised sub-images), while
        // blob: URLs have no such limit and are same-origin (no canvas taint).
        const url = URL.createObjectURL(new Blob([svg], { type: 'image/svg+xml;charset=utf-8' }))
        const img = new Image()
        img.onload = () => {
            try {
                const canvas = document.createElement('canvas')
                canvas.width = cw
                canvas.height = ch
                const ctx = canvas.getContext('2d')
                if (background) {
                    ctx.fillStyle = background
                    ctx.fillRect(0, 0, cw, ch)
                }
                ctx.drawImage(img, 0, 0, cw, ch)
                canvas.toBlob(
                    (b) => (b ? resolve(b) : reject(new Error('canvas.toBlob returned null'))),
                    'image/png'
                )
            } catch (e) {
                reject(e)
            } finally {
                URL.revokeObjectURL(url)
            }
        }
        img.onerror = () => { URL.revokeObjectURL(url); reject(new Error('SVG failed to load as image')) }
        img.src = url
    })
}

/// A filesystem-safe basename: `prefix` plus a cleaned-up slice of `text`.
/// `suffix` is appended verbatim (already safe), e.g. a random seed.
/// e.g. filenameBase('dactyl-bubble', 'dactyl') -> 'dactyl-bubble-dactyl'.
export function filenameBase(prefix, text, suffix = '') {
    const cleaned = (text || '')
        .replace(/\s+/g, '-')
        .replace(/[^\w-]/g, '')
        .slice(0, 24)
        .replace(/^-+|-+$/g, '')
    return `${prefix}${cleaned ? '-' + cleaned : ''}${suffix ? '-' + suffix : ''}`
}

/// A filesystem-safe basename derived from the grown text.
export function growFilenameBase(text) {
    return filenameBase('dactyl-grow', text)
}
