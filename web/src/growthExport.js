// Save helpers for the Grow tab.  Both preview paths (GPU canvas and SVG
// fallback) render the same growth rule, so downloads are driven from the
// vector SVG the worker produces: it's deterministic, resolution-independent,
// and works even without WebGL2.  PNG is that SVG rasterised at high res.

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
export function svgToPngBlob(svg, { scale = 3, background = null, maxDim = 4096 } = {}) {
    return new Promise((resolve, reject) => {
        const root = svg.match(/<svg[^>]*>/)?.[0] ?? ''
        const wMatch = root.match(/\bwidth="([\d.]+)"/)
        const hMatch = root.match(/\bheight="([\d.]+)"/)
        const w = wMatch ? parseFloat(wMatch[1]) : 1000
        const h = hMatch ? parseFloat(hMatch[1]) : 1000
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

/// A filesystem-safe basename derived from the grown text.
export function growFilenameBase(text) {
    const cleaned = (text || '')
        .replace(/\s+/g, '-')
        .replace(/[^\w-]/g, '')
        .slice(0, 24)
        .replace(/^-+|-+$/g, '')
    return `dactyl-grow${cleaned ? '-' + cleaned : ''}`
}
