import { useState, useEffect, useRef } from 'react'
import {
  loadFontFromFile, loadFontFromUrl, loadGoogleFontOutline, loadGoogleFontText,
  querySystemFonts, loadSystemFont, GOOGLE_FONTS,
} from './fontCompare'

/**
 * Top-bar controls for the Visual Diffs tab. Lets the user either keep the
 * existing Dactyl-vs-Dactyl axis diff, or compare Dactyl against an external
 * font loaded from an upload, a Google Font, or a locally installed font.
 *
 * The axis-diff UI is passed in as `axisControls` so this component owns only
 * the new "compare font" surface.
 */
export default function FontCompareControls({
  mode, onModeChange,
  size, onSizeChange,
  font, onFontChange,
  onError,
  axisControls,
}) {
  const [source, setSource] = useState('google')
  const [busy, setBusy] = useState(false)
  const [systemFonts, setSystemFonts] = useState(null)
  const [googleSel, setGoogleSel] = useState(GOOGLE_FONTS[0].name)
  const [urlInput, setUrlInput] = useState('')
  const [familyInput, setFamilyInput] = useState('')
  // Guards against a stale request (e.g. an auto-load kicked off for a source
  // the user has since switched away from) clobbering a later, faster one.
  const requestIdRef = useRef(0)

  const run = async (fn) => {
    const id = ++requestIdRef.current
    setBusy(true)
    onError(null)
    try {
      const result = await fn()
      if (id !== requestIdRef.current) return
      if (result) onFontChange(result)
    } catch (e) {
      if (id !== requestIdRef.current) return
      onFontChange(null)
      onError(e.message || String(e))
    } finally {
      if (id === requestIdRef.current) setBusy(false)
    }
  }

  const handleFile = (e) => {
    const file = e.target.files && e.target.files[0]
    if (file) run(() => loadFontFromFile(file))
  }

  // Auto-load the selected curated Google Font whenever the source/selection changes.
  useEffect(() => {
    if (mode !== 'font' || source !== 'google') return
    const entry = GOOGLE_FONTS.find(g => g.name === googleSel)
    if (entry) run(() => loadGoogleFontOutline(entry))
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [mode, source, googleSel])

  const loadUrl = () => {
    const v = urlInput.trim()
    if (v) run(() => loadFontFromUrl(v))
  }

  const loadCssFamily = () => {
    const v = familyInput.trim()
    if (v) run(async () => loadGoogleFontText(v))
  }

  const loadSystemList = () =>
    run(async () => { setSystemFonts(await querySystemFonts()); return null })

  const pickSystemFont = (family) => {
    const entry = systemFonts.find(s => s.family === family)
    if (entry) run(() => loadSystemFont(entry.face))
  }

  return (
    <div className={`diff-config font-compare-controls${mode === 'font' ? ' wrap' : ''}`}>
      <label htmlFor="compare-mode">Mode:</label>
      <select id="compare-mode" value={mode} onChange={e => onModeChange(e.target.value)}>
        <option value="axis">Axis diff</option>
        <option value="font">Compare font</option>
      </select>
      <div className="controls-break" />

      {mode === 'axis' && axisControls}

      {mode === 'font' && (
        <>
          <select value={source} onChange={e => setSource(e.target.value)} title="Font source">
            <option value="google">Google Fonts</option>
            <option value="system">System fonts</option>
            <option value="url">Load URL</option>
            <option value="cssFamily">CSS Font Family</option>
            <option value="upload">Upload</option>
          </select>
          <div className="controls-break" />

          {source === 'upload' && (
            <input type="file" accept=".ttf,.otf,.woff" onChange={handleFile} title="Upload .ttf/.otf/.woff" />
          )}

          {source === 'google' && (
            <select value={googleSel} onChange={e => setGoogleSel(e.target.value)}>
              {GOOGLE_FONTS.map(g => <option key={g.name} value={g.name}>{g.name}</option>)}
            </select>
          )}

          {source === 'url' && (
            <input
              type="text"
              className="compare-text-input"
              placeholder="https://…/font.ttf"
              value={urlInput}
              onChange={e => setUrlInput(e.target.value)}
              onKeyDown={e => { if (e.key === 'Enter') loadUrl() }}
            />
          )}

          {source === 'cssFamily' && (
            <input
              type="text"
              className="compare-text-input"
              placeholder="Font family name"
              value={familyInput}
              onChange={e => setFamilyInput(e.target.value)}
              onKeyDown={e => { if (e.key === 'Enter') loadCssFamily() }}
            />
          )}

          {source === 'system' && (
            !systemFonts ? (
              <button className="compare-load-btn" onClick={loadSystemList}>List system fonts</button>
            ) : (
              <select
                defaultValue=""
                onChange={e => e.target.value && pickSystemFont(e.target.value)}
              >
                <option value="" disabled>Select a font…</option>
                {systemFonts.map(s => <option key={s.family} value={s.family}>{s.family}</option>)}
              </select>
            )
          )}

          <div className="controls-break" />
          <label htmlFor="compare-size">Size:</label>
          <input
            id="compare-size"
            type="range"
            min="0.6" max="1.5" step="0.05"
            value={size}
            onChange={e => onSizeChange(parseFloat(e.target.value))}
            title="Comparison font size, relative to a cap-height match (1.0×)"
          />
          <span className="compare-size-val">{size.toFixed(2)}×</span>

          {busy && <span className="compare-status">Loading…</span>}
          {!busy && font && (
            <span className="compare-status" title={font.kind === 'text' ? 'CSS text rendering (no outlines)' : 'Vector outline overlay'}>
              {font.displayName} {font.kind === 'text' ? '(text)' : ''}
            </span>
          )}
        </>
      )}
    </div>
  )
}
