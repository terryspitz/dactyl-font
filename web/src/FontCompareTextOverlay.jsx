/**
 * Text-rendering comparison used when only a CSS font (not its outline bytes)
 * is available — e.g. a Google Font loaded via the stylesheet, or a system
 * font we could only reference by name. Mirrors the SVG overlay's three-column
 * cell layout: Dactyl | comparison | overlaid.
 *
 * Dactyl is rendered with the `dactylFamily` @font-face that App injects from
 * the current axes; the comparison font is rendered with `fontFamily`.
 */
export default function FontCompareTextOverlay({ text, fontFamily, dactylFamily, labelB, sizeScale = 1 }) {
  const chars = [...(text || '').replace(/[\n\r]/g, '')]
  const size = 90
  const cmpSize = size * sizeScale  // comparison font follows the Size slider

  const cell = (ch) => {
    const glyph = (family, color, glyphSize, extra) => (
      <span style={{ fontFamily: family, fontSize: glyphSize, lineHeight: 1, color, ...extra }}>{ch}</span>
    )
    return (
      <div key={ch} className="compare-text-cell">
        <div className="compare-text-col">{glyph(dactylFamily, '#000', size)}</div>
        <div className="compare-text-col">{glyph(fontFamily, '#000', cmpSize)}</div>
        <div className="compare-text-col compare-text-overlay">
          {glyph(dactylFamily, 'rgba(255,0,0,0.55)', size, { position: 'absolute' })}
          {glyph(fontFamily, 'rgba(0,0,255,0.55)', cmpSize, { position: 'absolute' })}
        </div>
      </div>
    )
  }

  return (
    <div className="compare-text-view">
      <div className="compare-text-key">
        Key: Left = Dactyl, Middle = {labelB}, Right = Overlaid (Red = Dactyl, Blue = {labelB})
      </div>
      <div className="compare-text-grid">{chars.map(cell)}</div>
    </div>
  )
}
