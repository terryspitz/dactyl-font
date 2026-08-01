import { useEffect, useRef, useState } from 'react'

// One menuOpen/setMenuOpen/menuRef triple per export button group, with the
// outside-click / Escape-to-close wiring that every tab's download menu
// needs — factored out so each new tab doesn't repeat the same effect.
export function useDownloadMenu() {
  const [menuOpen, setMenuOpen] = useState(false)
  const menuRef = useRef(null)
  useEffect(() => {
    if (!menuOpen) return
    const onDown = (e) => { if (menuRef.current && !menuRef.current.contains(e.target)) setMenuOpen(false) }
    const onKey = (e) => { if (e.key === 'Escape') setMenuOpen(false) }
    document.addEventListener('mousedown', onDown)
    document.addEventListener('keydown', onKey)
    return () => {
      document.removeEventListener('mousedown', onDown)
      document.removeEventListener('keydown', onKey)
    }
  }, [menuOpen])
  return [menuOpen, setMenuOpen, menuRef]
}

// Copy-to-clipboard + download(PNG/SVG-with-caret-menu) button pair for the
// canvas zoom toolbar — the same markup Font's export originally introduced,
// factored out so every tab can share it instead of re-duplicating ~40 lines
// of JSX per tab.
export default function ImageExportButtons({
  onCopy, onDownload, copied, saving, disabled,
  menuOpen, setMenuOpen, menuRef,
  copyTitle = 'Copy PNG to clipboard',
  pngTitle = 'Download PNG (transparent, high-res)',
  svgLabel = 'vector',
  showDivider = true,
}) {
  return (
    <>
      <button onClick={onCopy} disabled={saving || disabled} title={copyTitle}>
        <span className="material-symbols-outlined">{copied ? 'check' : 'content_copy'}</span>
      </button>
      {/* Download defaults to PNG; the caret opens a PNG/SVG menu. */}
      <span ref={menuRef} style={{ display: 'flex', alignItems: 'center', position: 'relative' }}>
        <button onClick={() => onDownload('png')} disabled={saving || disabled} title={pngTitle}>
          <span className="material-symbols-outlined">{saving ? 'hourglass_empty' : 'download'}</span>
        </button>
        {/* overflow:hidden keeps the caret glyph inside its button, so it
            can't sit on top of the download button and swallow its clicks */}
        <button
          onClick={() => setMenuOpen(o => !o)}
          disabled={saving || disabled}
          title="Choose download format"
          aria-haspopup="menu"
          aria-expanded={menuOpen}
          style={{ width: '20px', minWidth: '20px', padding: '6px 0', overflow: 'hidden' }}
        >
          <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>arrow_drop_down</span>
        </button>
        {menuOpen && (
          <div
            role="menu"
            style={{
              position: 'absolute', top: '100%', right: 0, marginTop: '8px', zIndex: 20,
              background: 'var(--panel-bg)', border: '1px solid var(--border-color)',
              borderRadius: 'var(--radius-md)', boxShadow: '0 4px 12px rgba(0,0,0,0.4)', overflow: 'hidden', minWidth: '160px',
            }}
          >
            <button className="grow-menu-item" role="menuitem" onClick={() => onDownload('png')}>
              <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>image</span>
              PNG <span style={{ opacity: 0.55, marginLeft: 'auto', fontSize: '0.8em' }}>transparent</span>
            </button>
            <button className="grow-menu-item" role="menuitem" onClick={() => onDownload('svg')}>
              <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>polyline</span>
              SVG <span style={{ opacity: 0.55, marginLeft: 'auto', fontSize: '0.8em' }}>{svgLabel}</span>
            </button>
          </div>
        )}
      </span>
      {showDivider && <span style={{ width: '1px', background: 'rgba(255,255,255,0.2)', margin: '2px 2px' }} />}
    </>
  )
}
