import { useState } from 'react'
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
  align, onAlignChange,
  font, onFontChange,
  onError,
  axisControls,
}) {
  const [source, setSource] = useState('upload')
  const [busy, setBusy] = useState(false)
  const [systemFonts, setSystemFonts] = useState(null)
  const [googleSel, setGoogleSel] = useState(GOOGLE_FONTS[0].name)
  const [textInput, setTextInput] = useState('')

  const run = async (fn) => {
    setBusy(true)
    onError(null)
    try {
      const result = await fn()
      if (result) onFontChange(result)
    } catch (e) {
      onFontChange(null)
      onError(e.message || String(e))
    } finally {
      setBusy(false)
    }
  }

  const handleFile = (e) => {
    const file = e.target.files && e.target.files[0]
    if (file) run(() => loadFontFromFile(file))
  }

  const loadGoogleSelection = () => {
    const entry = GOOGLE_FONTS.find(g => g.name === googleSel)
    if (entry) run(() => loadGoogleFontOutline(entry))
  }

  // A free-text value can be a direct font URL (outline) or a family name (text).
  const loadFreeText = () => {
    const v = textInput.trim()
    if (!v) return
    if (/^https?:\/\//.test(v)) run(() => loadFontFromUrl(v))
    else run(async () => loadGoogleFontText(v))
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

      {mode === 'axis' && axisControls}

      {mode === 'font' && (
        <>
          <select value={source} onChange={e => setSource(e.target.value)} title="Font source">
            <option value="upload">Upload</option>
            <option value="google">Google Fonts</option>
            <option value="system">System fonts</option>
          </select>

          {source === 'upload' && (
            <input type="file" accept=".ttf,.otf,.woff" onChange={handleFile} title="Upload .ttf/.otf/.woff" />
          )}

          {source === 'google' && (
            <>
              <select value={googleSel} onChange={e => setGoogleSel(e.target.value)}>
                {GOOGLE_FONTS.map(g => <option key={g.name} value={g.name}>{g.name}</option>)}
              </select>
              <button className="compare-load-btn" onClick={loadGoogleSelection}>Load</button>
              <input
                type="text"
                className="compare-text-input"
                placeholder="…or family name / .ttf URL"
                value={textInput}
                onChange={e => setTextInput(e.target.value)}
                onKeyDown={e => { if (e.key === 'Enter') loadFreeText() }}
              />
            </>
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

          <label htmlFor="compare-align">Align:</label>
          <select id="compare-align" value={align} onChange={e => onAlignChange(e.target.value)} title="Normalize sizes by">
            <option value="cap">cap-height</option>
            <option value="x">x-height</option>
            <option value="em">em</option>
          </select>

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
