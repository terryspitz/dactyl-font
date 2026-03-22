import { useState, useMemo, useEffect, useRef } from 'react'
import { generateSvg, defaultAxes, controlDefinitions, generateTweenSvg, getGlyphDefs, allChars, generateVisualTestsSvg } from './lib/fable/Api' // Adjust path if needed
import './App.css'


function App() {
  const [tabTexts, setTabTexts] = useState(() => {
    const savedSplines = localStorage.getItem('splineText')

    return {
      font: allChars,
      splines: savedSplines !== null ? savedSplines : 'font',
      tweens: 'a',
      visualTests: '',
      visualDiffs: allChars
    }
  })
  const [splinesDefsText, setSplinesDefsText] = useState(() => {
    const initialText = tabTexts['splines'] || 'a'
    return getGlyphDefs(initialText)
  })
  const [axes, setAxes] = useState({ ...defaultAxes })
  const [activeTab, setActiveTab] = useState('font')
  const [tabZooms, setTabZooms] = useState({
    font: 1.0,
    splines: 1.0,
    tweens: 1.0,
    visualTests: 1.0,
    visualDiffs: 1.0,
  })
  const [layerVisibility, setLayerVisibility] = useState({
    spiro: true,
    spline2: true,
    dspline: true,
    guides: true,
    knots: true,
    comb: true,
    tangents: true,
    labels: true,
  })

  // Check URL on mount
  useEffect(() => {
    const params = new URLSearchParams(window.location.search)
    const view = params.get('view')
    if (view && ['font', 'splines', 'tweens', 'visualTests', 'visualDiffs'].includes(view)) {
      setActiveTab(view)
    }
  }, [])

  // Update URL helper
  const setTabWithUrl = (tab) => {
    setActiveTab(tab)
    const url = new URL(window.location)
    url.searchParams.set('view', tab)
    window.history.pushState({}, '', url)
  }

  const setVisualTestsMode = (e) => {
    e.preventDefault()
    setTabWithUrl('visualTests')
  }

  const zoom = tabZooms[activeTab]
  const setZoom = (newValFunc) => {
    setTabZooms(prev => {
      const current = prev[activeTab]
      const next = typeof newValFunc === 'function' ? newValFunc(current) : newValFunc
      return { ...prev, [activeTab]: next }
    })
  }

  const text = tabTexts[activeTab]
  const setText = (newVal) => {
    setTabTexts(prev => ({ ...prev, [activeTab]: newVal }))
    if (activeTab === 'splines') {
      localStorage.setItem('splineText', newVal)
      setSplinesDefsText(getGlyphDefs(newVal || 'a'))
    }
  }

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

  // Worker state is now handled within the effect directly

  const renderIdRef = useRef(0)
  const loadingRef = useRef(false)
  const [loading, setLoading] = useState(false)
  const [showProgress, setShowProgress] = useState(false)
  const [progressValue, setProgressValue] = useState(0)
  const [workerResult, setWorkerResult] = useState(null)
  const [error, setError] = useState(null)

  // Trigger generation
  useEffect(() => {
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    
    worker.onmessage = (e) => {
      const { id, result, error, type, value } = e.data
      if (id !== renderIdRef.current) return

      if (type === 'progress') {
        setProgressValue(value)
        if (value > 0) setShowProgress(true)
        return
      }

      if (error) {
        setError(error)
      } else {
        setWorkerResult(result)
        setError(null)
      }
      setLoading(false)
      loadingRef.current = false
      setShowProgress(false)
    }

    const id = ++renderIdRef.current
    setLoading(true)
    loadingRef.current = true
    setProgressValue(0)
    setError(null)

    // Timer for progress bar
    const timer = setTimeout(() => {
      if (id === renderIdRef.current && loadingRef.current) {
        setShowProgress(true)
      }
    }, 1000)

    let typeReq, args
    if (activeTab === 'font') {
      if (!text) {
        setWorkerResult("")
        setLoading(false)
        clearTimeout(timer)
        worker.terminate()
        return
      }
      typeReq = 'font'
      args = [text, axes]
    } else if (activeTab === 'splines') {
      typeReq = 'splinesFromDefs'
      args = [splinesDefsText, axes]
    } else if (activeTab === 'tweens') {
      const char = text.length > 0 ? text[0] : 'a'
      typeReq = 'tweens'
      args = [char, axes]
    } else if (activeTab === 'visualTests') {
      typeReq = 'visualTests'
      args = []
    } else if (activeTab === 'visualDiffs') {
      typeReq = 'visualDiffs'
      args = [text || allChars, axes]
    }

    if (typeReq) {
      worker.postMessage({ id, type: typeReq, args })
    }

    return () => {
      clearTimeout(timer)
      worker.terminate()
    }
  }, [text, axes, activeTab, splinesDefsText])

  const renderContent = () => {
    if (error) return <div style={{ color: 'red' }}>Error: {error}</div>
    if (!workerResult && loading && activeTab !== 'tweens') {
      // Optional: return <div style={{padding: '20px'}}>Generating...</div> 
      // But user asked for progress bar at top, so maybe leave blank or keep old?
      // If we return null, it might flash.
    }

    // Safety check: ensure result matches expected type for tab
    const content = workerResult
    if (!content) return null

    try {
      if (activeTab === 'font') {
        if (typeof content !== 'string') return null
        return <div
          className="svg-container"
          dangerouslySetInnerHTML={{ __html: content }}
        />
      } else if (activeTab === 'splines') {
        if (typeof content !== 'string') return null
        const visibilityClasses = Object.entries(layerVisibility)
          .filter(([_, visible]) => !visible)
          .map(([key]) => `hide-${key}`)
          .join(' ')

        return (
          <div className={`splines-container ${visibilityClasses}`}>
            <div
              className="svg-container"
              dangerouslySetInnerHTML={{ __html: content }}
            />
          </div>
        )
      } else if (activeTab === 'tweens') {
        if (typeof content !== 'object') return null
        // content is { [ctrlName]: [ { val, svg } ] }

        return (
          <div className="tweens-grid">
            {(() => {
              const EXCLUDED_TWEEN_AXES = ['tracking', 'leading']
              return controlDefinitions
                .filter(c => c.type_ !== 'checkbox' && !EXCLUDED_TWEEN_AXES.includes(c.name))
                .map(ctrl => {
                  const variations = content[ctrl.name]
                  if (!variations) return null

                  const rowVariations = variations.map((v, i) => {
                    const boxWidth = 150 * zoom
                    return (
                      <div key={`${ctrl.name}-${i}`} className="tween-item" style={{ minWidth: boxWidth + 'px', width: boxWidth + 'px' }}>
                        <div dangerouslySetInnerHTML={{ __html: v.svg }} />
                        <div style={{ fontSize: '0.7em' }}>{v.val.toFixed(2)}</div>
                      </div>
                    )
                  })

                  return (
                    <div key={ctrl.name} className="tween-row" style={{ gridColumn: '1 / -1', marginBottom: '20px' }}>
                      <h4 style={{ textAlign: 'left', margin: '5px 0' }}>{ctrl.name}</h4>
                      <div className="tween-variations" style={{ display: 'flex', gap: '10px', overflowX: 'auto', paddingBottom: '10px' }}>
                        {rowVariations}
                      </div>
                    </div>
                  )
                })
            })()}
          </div>
        )
      } else if (activeTab === 'visualTests' || activeTab === 'visualDiffs') {
        if (typeof content !== 'string') return null
        return <div
          className="svg-container"
          dangerouslySetInnerHTML={{ __html: content }}
        />
      }
      return null
    } catch (e) {
      console.error("Error generating Content:", e)
      return <div style={{ color: 'red' }}>Error: {e.message}</div>
    }
  }

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
            <a className="icon-button" href="https://terryspitz.github.io/dactyl-font/README.html" target="_blank" title="Documentation">
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
            <button className={`tab-button ${activeTab === 'font' ? 'active' : ''}`} onClick={() => setTabWithUrl('font')}>Font</button>
            <button className={`tab-button ${activeTab === 'splines' ? 'active' : ''}`} onClick={() => setTabWithUrl('splines')}>Splines</button>
            <button className={`tab-button ${activeTab === 'tweens' ? 'active' : ''}`} onClick={() => setTabWithUrl('tweens')}>Tweens</button>
            <button className={`tab-button ${activeTab === 'visualDiffs' ? 'active' : ''}`} onClick={() => setTabWithUrl('visualDiffs')}>Visual Diffs</button>
          </div>

        </div>

        <div className={`input-area ${activeTab === 'splines' ? 'with-defs' : ''}`}>
          <div className="input-wrapper">
            <textarea
              value={text}
              onChange={e => setText(e.target.value)}
              rows={3}
              placeholder="Characters..."
            />
            <button
              className="text-reset-button"
              onClick={() => {
                const defaults = { font: allChars, splines: 'font', tweens: 'a', visualTests: '', visualDiffs: allChars }
                setText(defaults[activeTab])
              }}
              title="Reset Text to Default"
            >
              <span className="material-symbols-outlined" style={{ fontSize: '16px' }}>restart_alt</span>
            </button>
          </div>
          {activeTab === 'splines' && (
            <div className="glyph-defs-panel" style={{ display: 'flex', flexDirection: 'column', gap: '5px' }}>
              <h3 style={{ margin: 0 }}>Glyph Definitions</h3>
              <textarea
                value={splinesDefsText}
                onChange={e => setSplinesDefsText(e.target.value)}
                style={{ width: '100%', flex: '1', minHeight: '100px', fontFamily: 'monospace', resize: 'vertical' }}
                spellCheck="false"
              />
              <div className="helper-key" style={{ fontSize: '0.85em', color: '#666' }}>
                <strong>Key:</strong> y: (t)op, (x)-height, (h)alf, (b)ottom, (d)escender, (o)ffset, (e)xtended. <br/>
                x: (l)eft, (c)enter, (r)ight, (w)ide. <br/>
                Dirs: N,S,E,W. Lines: (-) straight, (~) curve, (.) corner. Brackets mean 'fit this coordinate instead'
              </div>
            </div>
          )}
        </div>
        <div className="preview">
          {showProgress && (
            <div className="progress-bar-container">
              {progressValue > 0 ? (
                <div
                  className="progress-bar-determinate"
                  style={{ width: `${progressValue * 100}%` }}
                />
              ) : (
                <div className="progress-bar-indeterminate"></div>
              )}
            </div>
          )}
          <div className="preview-content">
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
              {renderContent()}
            </div>
          </div>
        </div>

        {activeTab === 'splines' && (
          <div className="spline-legend">
            <div className="legend-item">
              <input
                type="checkbox"
                checked={layerVisibility.spiro}
                onChange={e => setLayerVisibility(prev => ({ ...prev, spiro: e.target.checked }))}
              />
              <span className="swatch blue"></span>
              <a href="https://www.levien.com/spiro/" target="_blank" rel="noopener noreferrer" style={{ color: 'inherit', textDecoration: 'underline' }}>Spiro</a>
            </div>
            <div className="legend-item">
              <input
                type="checkbox"
                checked={layerVisibility.spline2}
                onChange={e => setLayerVisibility(prev => ({ ...prev, spline2: e.target.checked }))}
              />
              <span className="swatch green"></span>
              <a href="https://raphlinus.github.io/curves/2018/12/21/new-spline.html" target="_blank" rel="noopener noreferrer" style={{ color: 'inherit', textDecoration: 'underline' }}>Spline2</a>
            </div>
            <div className="legend-item">
              <input
                type="checkbox"
                checked={layerVisibility.dspline}
                onChange={e => setLayerVisibility(prev => ({ ...prev, dspline: e.target.checked }))}
              />
              <span className="swatch orange"></span>
              <span onClick={setVisualTestsMode} style={{ cursor: 'pointer', textDecoration: 'dotted underline' }} title="Visual Tests">
                DactylSpline
              </span>
            </div>
            <div className="legend-item">
              <input
                type="checkbox"
                checked={layerVisibility.comb}
                onChange={e => setLayerVisibility(prev => ({ ...prev, comb: e.target.checked }))}
              />
              <span className="swatch" style={{ border: '1px solid black', backgroundColor: 'transparent' }}></span> Comb
            </div>
            <div className="legend-item">
              <input
                type="checkbox"
                checked={layerVisibility.tangents}
                onChange={e => setLayerVisibility(prev => ({ ...prev, tangents: e.target.checked }))}
              />
              <span className="swatch" style={{ backgroundColor: '#e00000' }}></span> Tangents
            </div>
            <div className="legend-item">
              <input
                type="checkbox"
                checked={layerVisibility.guides}
                onChange={e => setLayerVisibility(prev => ({ ...prev, guides: e.target.checked }))}
              />
              <span className="swatch grey"></span> Guides
            </div>
            <div className="legend-item">
              <input
                type="checkbox"
                checked={layerVisibility.labels}
                onChange={e => setLayerVisibility(prev => ({ ...prev, labels: e.target.checked }))}
              />
              <span style={{ color: 'red', fontSize: '0.8em', fontWeight: 'bold', width: '24px', textAlign: 'center' }}>abc</span>
              Labels
            </div>
            <div className="legend-item">
              <input
                type="checkbox"
                checked={layerVisibility.knots}
                onChange={e => setLayerVisibility(prev => ({ ...prev, knots: e.target.checked }))}
              />
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
