import { useState, useMemo } from 'react'
import { generateSvg, defaultAxes, controlDefinitions, generateSplineDebugSvg, generateTweenSvg, getGlyphDefs, allChars } from './lib/fable/Api' // Adjust path if needed
import './App.css'

function App() {
  const [tabTexts, setTabTexts] = useState({
    font: allChars,
    splines: 'font',
    tweens: 'a'
  })
  const [axes, setAxes] = useState({ ...defaultAxes })
  const [activeTab, setActiveTab] = useState('font')
  const [tabZooms, setTabZooms] = useState({
    font: 1.0,
    splines: 1.0,
    tweens: 1.0,
  })
  const zoom = tabZooms[activeTab]
  const setZoom = (newValFunc) => {
    setTabZooms(prev => {
      const current = prev[activeTab]
      const next = typeof newValFunc === 'function' ? newValFunc(current) : newValFunc
      return { ...prev, [activeTab]: next }
    })
  }

  const text = tabTexts[activeTab]
  const setText = (newVal) => setTabTexts(prev => ({ ...prev, [activeTab]: newVal }))

  // Group controls by category
  const controlsByCategory = useMemo(() => {
    const groups = {}
    controlDefinitions.forEach(ctrl => {
      const cat = ctrl.category || 'default'
      if (!groups[cat]) groups[cat] = []
      groups[cat].push(ctrl)
    })
    return groups
  }, [])

  // State for collapsible sections
  // experimental closed by default, others open
  const [openCategories, setOpenCategories] = useState(() => {
    const cats = {}
    controlDefinitions.forEach(ctrl => {
      const cat = ctrl.category || 'default'
      if (cat === 'experimental') cats[cat] = false
      else cats[cat] = true
    })
    return cats
  })

  const toggleCategory = (cat) => {
    setOpenCategories(prev => ({ ...prev, [cat]: !prev[cat] }))
  }

  // Memoize SVG generation
  const content = useMemo(() => {
    try {
      // For splines/tweens we might want to run even if text is empty (using default)
      // but 'text' state init has default anyway.

      if (activeTab === 'font') {
        if (!text) return ""
        return <div
          className="svg-container"
          dangerouslySetInnerHTML={{ __html: generateSvg(text, axes) }}
        />
      } else if (activeTab === 'splines') {
        // For splines, we likely want a shorter default text if it's empty or too long, 
        // but the user might want to debug specific chars.
        const splineText = text.length > 0 ? text : "a"
        // Definitions moved to input area
        return (
          <div className="splines-container">
            <div
              className="svg-container"
              dangerouslySetInnerHTML={{ __html: generateSplineDebugSvg(splineText, axes) }}
            />
          </div>
        )
      } else if (activeTab === 'tweens') {
        // Logic for tweens: iterate over controls, generate variations
        const steps = 9
        const char = text.length > 0 ? text[0] : 'a'

        return (
          <div className="tweens-grid">
            {controlDefinitions.filter(c => c.type_ !== 'checkbox').map(ctrl => {
              const variations = []
              const min = ctrl.min
              const max = ctrl.max
              const range = max - min

              for (let i = 0; i < steps; i++) {
                const val = min + (range * (i / (steps - 1)))
                // Create a temporary axes object with this value overridden
                const tempAxes = { ...axes, [ctrl.name]: val }
                // Apply zoom to individual boxes. use minWidth to override CSS
                const boxWidth = 150 * zoom
                variations.push(
                  <div key={`${ctrl.name}-${i}`} className="tween-item" style={{ minWidth: boxWidth + 'px', width: boxWidth + 'px' }}>
                    <div dangerouslySetInnerHTML={{ __html: generateTweenSvg(char, tempAxes) }} />
                    <div style={{ fontSize: '0.7em' }}>{val.toFixed(2)}</div>
                  </div>
                )
              }

              return (
                <div key={ctrl.name} className="tween-row" style={{ gridColumn: '1 / -1', marginBottom: '20px' }}>
                  <h4 style={{ textAlign: 'left', margin: '5px 0' }}>{ctrl.name}</h4>
                  <div className="tween-variations" style={{ display: 'flex', gap: '10px', overflowX: 'auto', paddingBottom: '10px' }}>
                    {variations}
                  </div>
                </div>
              )
            })}
          </div>
        )
      }
      return null

    } catch (e) {
      console.error("Error generating Content:", e)
      return <div style={{ color: 'red' }}>Error: {e.message}</div>
    }
  }, [text, axes, activeTab, zoom])

  const handleControlChange = (name, value) => {
    setAxes(prev => ({ ...prev, [name]: value }))
  }

  const handleReset = () => {
    setAxes({ ...defaultAxes })
  }

  const handleRandom = () => {
    const newAxes = { ...axes }
    controlDefinitions.forEach(ctrl => {
      if (ctrl.category === 'experimental') return

      if (ctrl.type_ === 'checkbox') {
        newAxes[ctrl.name] = Math.random() > 0.5
      } else {
        newAxes[ctrl.name] = ctrl.min + Math.random() * (ctrl.max - ctrl.min)
      }
    })
    setAxes(newAxes)
  }

  // Calculate definitions for Splines tab outside content useMemo
  const splineIdxText = activeTab === 'splines' ? (text.length > 0 ? text : "a") : ""
  const splineDefs = activeTab === 'splines' ? getGlyphDefs(splineIdxText) : null

  return (
    <div className="container">
      <div className="sidebar">
        <div className="sidebar-title" dangerouslySetInnerHTML={{ __html: generateTweenSvg("Dactyl", { ...defaultAxes, thickness: 35 }) }} />
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '10px' }}>
          <h2 style={{ margin: 0 }}>Controls</h2>
          <div className="toolbar" style={{ display: 'flex', gap: '5px' }}>
            <button className="icon-button" onClick={handleReset} title="Reset">
              <span className="material-symbols-outlined">restart_alt</span>
            </button>
            <button className="icon-button" onClick={handleRandom} title="Randomize">
              <span className="material-symbols-outlined">casino</span>
            </button>
            <a className="icon-button" href="https://terryspitz.github.io/dactyl-font/" target="_blank" title="Documentation">
              <span className="material-symbols-outlined">menu_book</span>
            </a>
          </div>
        </div>
        <div className="controls-list">
          {Object.entries(controlsByCategory).map(([category, controls]) => (
            <div key={category} className="category-group">
              <div
                className="category-header"
                onClick={() => toggleCategory(category)}
                style={{ cursor: 'pointer', fontWeight: 'bold', marginBottom: '10px', display: 'flex', alignItems: 'center' }}
              >
                <span style={{ marginRight: '5px' }}>{openCategories[category] ? '▼' : '▶'}</span>
                {category.charAt(0).toUpperCase() + category.slice(1)}
              </div>

              {openCategories[category] && (
                <div className="category-content" style={{ paddingLeft: '10px' }}>
                  {controls.map(ctrl => (
                    <div key={ctrl.name} className="control-group">
                      <label>
                        {ctrl.name}
                      </label>
                      <div className="control-input">
                        <div className="slider-container">
                          <div className="slider-center-line"></div>
                          {ctrl.type_ === 'range' ? (
                            <input
                              type="range"
                              min={ctrl.min}
                              max={ctrl.max}
                              step={ctrl.step}
                              value={axes[ctrl.name]}
                              onChange={e => handleControlChange(ctrl.name, parseFloat(e.target.value))}
                              className="slider-input neutral-slider"
                              style={{
                                transform: (() => {
                                  let defaultVal = 0.5
                                  if (defaultAxes && defaultAxes[ctrl.name] !== undefined) {
                                    defaultVal = defaultAxes[ctrl.name]
                                  } else {
                                    defaultVal = (ctrl.min + ctrl.max) / 2
                                  }

                                  const range = ctrl.max - ctrl.min
                                  const fraction = range === 0 ? 0.5 : (defaultVal - ctrl.min) / range
                                  // Account for 16px thumb width (8px radius)
                                  // Left = 100 - (8 + fraction * (100 - 16))
                                  // But here we use translateX from left:100
                                  // so translateX = - (8 + fraction * 84) px
                                  return `translateX(${24 - fraction * 84}px)`
                                })(),
                              }}
                            />
                          ) : (
                            <input
                              type="checkbox"
                              checked={axes[ctrl.name]}
                              onChange={e => handleControlChange(ctrl.name, e.target.checked)}
                              style={{ position: 'absolute', left: '50%', transform: 'translateX(-50%)', margin: 0, zIndex: 1 }}
                            />
                          )}
                        </div>
                        {ctrl.type_ === 'range' && <span className="value-display" style={{ marginLeft: '10px' }}>{Number(axes[ctrl.name]).toFixed(2)}</span>}
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </div>
          ))}
        </div>
      </div>
      <div className="main">
        <div className="top-bar">
          <div className="tabs">
            <button className={`tab-button ${activeTab === 'font' ? 'active' : ''}`} onClick={() => setActiveTab('font')}>Font</button>
            <button className={`tab-button ${activeTab === 'splines' ? 'active' : ''}`} onClick={() => setActiveTab('splines')}>Splines</button>
            <button className={`tab-button ${activeTab === 'tweens' ? 'active' : ''}`} onClick={() => setActiveTab('tweens')}>Tweens</button>
          </div>

        </div>

        <div className={`input-area ${activeTab === 'splines' ? 'with-defs' : ''}`}>
          <div className="input-wrapper">
            <textarea
              value={text}
              onChange={e => setText(e.target.value)}
              rows={3}
              placeholder="Type here..."
            />
            <button
              className="text-reset-button"
              onClick={() => {
                const defaults = { font: allChars, splines: 'font', tweens: 'a' }
                setText(defaults[activeTab])
              }}
              title="Reset Text to Default"
            >
              <span className="material-symbols-outlined" style={{ fontSize: '16px' }}>restart_alt</span>
            </button>
          </div>
          {activeTab === 'splines' && (
            <div className="glyph-defs-panel">
              <h3>Glyph Definitions</h3>
              {splineDefs}
            </div>
          )}
        </div>
        <div className="preview">
          <div className="zoom-controls">
            <button onClick={() => setZoom(z => Math.min(z + 0.1, 5.0))} title="Zoom In">
              <span className="material-symbols-outlined">add</span>
            </button>
            <button onClick={() => setZoom(1.0)} title="Reset Zoom">
              <span className="material-symbols-outlined">restart_alt</span>
            </button>
            <button onClick={() => setZoom(z => Math.max(z - 0.1, 0.1))} title="Zoom Out">
              <span className="material-symbols-outlined">remove</span>
            </button>
          </div>
          <div style={{ transform: activeTab === 'tweens' ? 'none' : `scale(${zoom})`, transformOrigin: 'top left', minHeight: '100%' }}>
            {content}
          </div>
        </div>

        {activeTab === 'splines' && (
          <div className="spline-legend">
            <div className="legend-item"><span className="swatch blue"></span> Spiro</div>
            <div className="legend-item"><span className="swatch green"></span> Spline2</div>
            <div className="legend-item"><span className="swatch orange"></span> DactylSpline</div>
            <div className="legend-item"><span className="swatch grey"></span> Guides</div>
            <div className="legend-item">
              <span className="swatch lightBlue circle"></span>
              <span className="swatch lightGreen circle"></span>
              Knots
            </div>
          </div>
        )}
      </div>
    </div>
  )
}

export default App
