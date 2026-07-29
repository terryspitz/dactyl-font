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

/// A downloadable Blob for an SVG string.
export function svgBlob(svg) {
    return new Blob([svg], { type: 'image/svg+xml;charset=utf-8' })
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
    return new Promise((resolve, reject) => {
        const { w, h } = svgPixelSize(svg)
        let s = scale
        if (w * s > maxDim || h * s > maxDim) s = Math.min(maxDim / w, maxDim / h)
        const cw = Math.max(1, Math.round(w * s))
        const ch = Math.max(1, Math.round(h * s))

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
            }
        }
        img.onerror = () => reject(new Error('SVG failed to load as image'))
        img.src = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(svg)
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
