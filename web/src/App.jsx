import { useState, useMemo, useEffect, useRef, useCallback } from 'react'
import { generateSvg, defaultAxes, controlDefinitions, generateTweenSvg, getGlyphDefs, allChars } from './lib/fable/Api' // Adjust path if needed
import SplineEditor from './SplineEditor'
import SplineGrid from './SplineGrid'
import GrowCanvas from './GrowCanvas'
import { downloadBlob, svgBlob, svgToPngBlob, growFilenameBase } from './growthExport'
import { downloadFont, buildFontDataUrl } from './fontExport'
import { buildCompareOverlaySvg } from './fontCompare'
import FontCompareControls from './FontCompareControls'
import FontCompareTextOverlay from './FontCompareTextOverlay'
import { proofTexts, proofLabels, proofCases, classicBooks } from './proofs'
import './App.css'

// Special Visual Diffs option: compare the old spiro/spline2 engine vs the new dactyl spline
const SPLINE_ENGINE = 'spline_engine'

// Glyphs floating tools legend: non-spline layerVisibility keys grouped under "Debug"
const DEBUG_LAYER_KEYS = ['comb', 'tangents', 'guides', 'labels', 'knots']

// Build the two axes variants (and key labels) for the Visual Diffs tab
function getDiffAxes(axes, diffConfig) {
  if (diffConfig.axis === SPLINE_ENGINE) {
    return {
      axesA: { ...axes, dactyl_spline: false, spline2: true },
      axesB: { ...axes, dactyl_spline: true, spline2: false },
      labelA: 'Old Spline',
      labelB: 'New Spline',
    }
  }
  const ctrl = controlDefinitions.find(c => c.name === diffConfig.axis)
  if (ctrl.type_ === 'checkbox') {
    return {
      axesA: { ...axes, [diffConfig.axis]: Boolean(diffConfig.valueA) },
      axesB: { ...axes, [diffConfig.axis]: Boolean(diffConfig.valueB) },
      labelA: `${diffConfig.axis}=${diffConfig.valueA ? 'on' : 'off'}`,
      labelB: `${diffConfig.axis}=${diffConfig.valueB ? 'on' : 'off'}`,
    }
  }
  return {
    axesA: { ...axes, [diffConfig.axis]: diffConfig.valueA },
    axesB: { ...axes, [diffConfig.axis]: diffConfig.valueB },
    labelA: `${diffConfig.axis}=${Number(diffConfig.valueA.toFixed(2))}`,
    labelB: `${diffConfig.axis}=${Number(diffConfig.valueB.toFixed(2))}`,
  }
}

function App() {
  const [tabTexts, setTabTexts] = useState(() => {
    const savedGlyphs = localStorage.getItem('glyphText') || localStorage.getItem('splineText')

    return {
      font: allChars,
      glyphs: savedGlyphs !== null ? savedGlyphs : 'font',
      tweens: 'a',
      visualDiffs: allChars,
      splines: '',
      splineGrid: '',
      proofs: proofTexts.lowercase,
      grow: 'dactyl'
    }
  })
  const [glyphsDefsText, setGlyphsDefsText] = useState(() => {
    const initialText = tabTexts['glyphs'] || 'a'
    return getGlyphDefs(initialText, defaultAxes.alt_a_g)
  })
  const [axes, setAxes] = useState({ ...defaultAxes })
  const [activeTab, setActiveTab] = useState('font')
  // Visual Diffs config: which axis to diff and the two values to compare.
  // SPLINE_ENGINE is a special compound option (old spiro/spline2 vs new dactyl spline).
  const [diffConfig, setDiffConfig] = useState(() => {
    const params = new URLSearchParams(window.location.search)
    const axis = params.get('diffAxis')
    const ctrl = controlDefinitions.find(c => c.name === axis)
    if (!ctrl) return { axis: SPLINE_ENGINE, valueA: 0, valueB: 1 }
    const a = parseFloat(params.get('diffA'))
    const b = parseFloat(params.get('diffB'))
    if (ctrl.type_ === 'checkbox') {
      return { axis, valueA: isNaN(a) ? 0 : (a ? 1 : 0), valueB: isNaN(b) ? 1 : (b ? 1 : 0) }
    }
    return {
      axis,
      valueA: isNaN(a) ? Number(defaultAxes[axis]) : a,
      valueB: isNaN(b) ? ctrl.max : b,
    }
  })
  // Visual Diffs "compare font" mode: diff Dactyl against an external font
  // (Google Fonts by default, or upload / system). 'axis' keeps the original
  // Dactyl-vs-Dactyl diff.
  // `compare` and `size` are URL-addressable so a comparison view is shareable
  // (and deep-linkable from the visual tests). The chosen font itself can't be
  // encoded in a URL, so it must still be (re)selected after navigation.
  const [compareMode, setCompareMode] = useState(() => {
    const c = new URLSearchParams(window.location.search).get('compare')
    return c === 'font' ? 'font' : 'axis'
  })
  // Size of the comparison font relative to a cap-height match (1.0 = exact).
  const [compareSize, setCompareSize] = useState(() => {
    const s = parseFloat(new URLSearchParams(window.location.search).get('size'))
    return !isNaN(s) && s >= 0.6 && s <= 1.5 ? s : 1.0
  })
  const [compareFont, setCompareFont] = useState(null)
  const [compareError, setCompareError] = useState(null)
  const [dactylGlyphData, setDactylGlyphData] = useState(null)
  const [proofCase, setProofCase] = useState(() => {
    const params = new URLSearchParams(window.location.search)
    const p = params.get('proof')
    if (p === 'classic') return 'classic'
    return proofCases.includes(p) ? p : 'lowercase'
  })
  const [tabZooms, setTabZooms] = useState(() => {
    const urlZoom = parseFloat(new URLSearchParams(window.location.search).get('zoom'))
    const zoom = isNaN(urlZoom) ? 1.0 : urlZoom
    return { font: zoom, glyphs: zoom, tweens: zoom, visualDiffs: zoom, splines: zoom, splineGrid: zoom, proofs: zoom, grow: zoom }
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
  const [glyphsFilled, setGlyphsFilled] = useState(true)
  const [legendPos, setLegendPos] = useState({ x: 0, y: 0 })
  const isDraggingRef = useRef(false)
  const dragStartRef = useRef({ x: 0, y: 0 })
  const [tweenFilter, setTweenFilter] = useState(
    () => new URLSearchParams(window.location.search).get('tween') || ''
  )
  // Grow tab: constant-gap growth parameters (see growth.js)
  const [growParams, setGrowParams] = useState({ grow: 0.7, gap: 30, layers: true, animate: false })
  // Grow tab GPU path: the worker builds the (d1, dOpp) field once per
  // text/axes change; sliders only move shader uniforms (see GrowCanvas.jsx).
  // Without WebGL2 the tab falls back to the worker-side SVG render.
  const [growField, setGrowField] = useState(null)
  const [savingGrow, setSavingGrow] = useState(false)
  const [growCopied, setGrowCopied] = useState(false)
  const [growMenuOpen, setGrowMenuOpen] = useState(false)
  const growMenuRef = useRef(null)
  const supportsWebGL2 = useMemo(() => {
    try { return !!document.createElement('canvas').getContext('webgl2') } catch { return false }
  }, [])

  // Check URL on mount
  useEffect(() => {
    const params = new URLSearchParams(window.location.search)
    let view = params.get('view')
    if (view && ['font', 'glyphs', 'tweens', 'visualDiffs', 'splines', 'splineGrid', 'proofs', 'grow'].includes(view)) {
      setActiveTab(view)
    }
    const p = params.get('proof')
    if (proofCases.includes(p)) {
      setTabTexts(prev => ({ ...prev, proofs: proofTexts[p] }))
    } else if (p === 'classic') {
      const idx = parseInt(params.get('book'))
      const book = (!isNaN(idx) && idx >= 0 && idx < classicBooks.length) ? classicBooks[idx] : null
      if (book) setTabTexts(prev => ({ ...prev, proofs: book.text }))
    }
  }, [])

  // Keep tweenFilter in sync with URL when the test changes it via pushState+popstate
  useEffect(() => {
    const onPopState = () =>
      setTweenFilter(new URLSearchParams(window.location.search).get('tween') || '')
    window.addEventListener('popstate', onPopState)
    return () => window.removeEventListener('popstate', onPopState)
  }, [])

  // Update URL helper
  const setTabWithUrl = (tab) => {
    setActiveTab(tab)
    const url = new URL(window.location)
    url.searchParams.set('view', tab)
    window.history.pushState({}, '', url)
  }

  const setProofCaseWithUrl = (pcase) => {
    setProofCase(pcase)
    setClassicBook(null)
    setTabTexts(prev => ({ ...prev, proofs: proofTexts[pcase] }))
    const url = new URL(window.location)
    url.searchParams.set('proof', pcase)
    window.history.pushState({}, '', url)
  }

  const setCompareSpacingWithUrl = (on) => {
    setCompareSpacing(on)
    const url = new URL(window.location)
    if (on) url.searchParams.set('compareSpacing', '1')
    else url.searchParams.delete('compareSpacing')
    window.history.replaceState({}, '', url)
  }

  const setDiffConfigWithUrl = (cfg) => {
    setDiffConfig(cfg)
    const url = new URL(window.location)
    if (cfg.axis === SPLINE_ENGINE) {
      url.searchParams.delete('diffAxis')
      url.searchParams.delete('diffA')
      url.searchParams.delete('diffB')
    } else {
      url.searchParams.set('diffAxis', cfg.axis)
      url.searchParams.set('diffA', cfg.valueA)
      url.searchParams.set('diffB', cfg.valueB)
    }
    // replaceState: value edits shouldn't spam browser history
    window.history.replaceState({}, '', url)
  }

  const setCompareModeWithUrl = (m) => {
    setCompareMode(m)
    const url = new URL(window.location)
    if (m === 'font') url.searchParams.set('compare', 'font')
    else url.searchParams.delete('compare')
    window.history.replaceState({}, '', url)
  }

  const setCompareSizeWithUrl = (s) => {
    setCompareSize(s)
    const url = new URL(window.location)
    if (s === 1.0) url.searchParams.delete('size')
    else url.searchParams.set('size', String(s))
    window.history.replaceState({}, '', url)
  }

  const handleDiffAxisChange = (axisName) => {
    if (axisName === SPLINE_ENGINE) {
      setDiffConfigWithUrl({ axis: SPLINE_ENGINE, valueA: 0, valueB: 1 })
      return
    }
    const ctrl = controlDefinitions.find(c => c.name === axisName)
    if (ctrl.type_ === 'checkbox') {
      setDiffConfigWithUrl({ axis: axisName, valueA: 0, valueB: 1 })
    } else {
      // A starts at the current sidebar value, B at the axis max
      setDiffConfigWithUrl({ axis: axisName, valueA: Number(axes[axisName]), valueB: ctrl.max })
    }
  }

  const handlePickClassic = () => {
    const idx = Math.floor(Math.random() * classicBooks.length)
    const book = classicBooks[idx]
    setClassicBook(book)
    setProofCase('classic')
    setTabTexts(prev => ({ ...prev, proofs: book.text }))
    const url = new URL(window.location)
    url.searchParams.set('proof', 'classic')
    url.searchParams.set('book', idx)
    window.history.pushState({}, '', url)
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
    if (activeTab === 'glyphs') {
      localStorage.setItem('glyphText', newVal)
      setGlyphsDefsText(getGlyphDefs(newVal || 'a', axes.alt_a_g))
    }
  }

  // The Glyphs tab's def textarea holds resolved definition strings (picked from
  // altGlyphMap vs glyphMap), not a live axes lookup, so — unlike every other axis,
  // which the renderer applies to the existing defs on the fly — toggling alt_a_g
  // needs to re-derive the text to pick up the alternate 'a'/'g' shapes.
  useEffect(() => {
    if (activeTab === 'glyphs') {
      setGlyphsDefsText(getGlyphDefs(tabTexts.glyphs || 'a', axes.alt_a_g))
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [axes.alt_a_g])

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

  const categoryIcons = {
    backbone: 'straighten',
    outline: 'brush',
    artistic: 'palette',
    experimental: 'science',
    debug: 'pest_control'
  }


  // State for collapsible sections
  // experimental closed by default, others open
  const [openCategories, setOpenCategories] = useState(() => {
    const cats = {}
    controlDefinitions.forEach(ctrl => {
      const cat = ctrl.category || 'default'
      if (cat === 'experimental' || cat === 'debug') cats[cat] = false
      else cats[cat] = true
    })
    return cats
  })

  const toggleCategory = (cat) => {
    setOpenCategories(prev => ({ ...prev, [cat]: !prev[cat] }))
  }

  const handleLegendMouseDown = (e) => {
    // Only drag on left click and not on interactive elements inside
    if (e.button !== 0 || e.target.tagName === 'INPUT' || e.target.tagName === 'A') return

    isDraggingRef.current = true
    dragStartRef.current = { x: e.clientX - legendPos.x, y: e.clientY - legendPos.y }
    e.preventDefault()
  }

  const handleLegendTouchStart = (e) => {
    if (e.target.tagName === 'INPUT' || e.target.tagName === 'A') return
    const touch = e.touches[0]
    isDraggingRef.current = true
    dragStartRef.current = { x: touch.clientX - legendPos.x, y: touch.clientY - legendPos.y }
    // Don't preventDefault here so checkboxes still work
  }

  useEffect(() => {
    const handleMouseMove = (e) => {
      if (!isDraggingRef.current) return
      setLegendPos(prev => ({
        x: e.clientX - dragStartRef.current.x,
        y: e.clientY - dragStartRef.current.y
      }))
    }

    const handleMouseUp = () => {
      isDraggingRef.current = false
    }

    const handleTouchMove = (e) => {
      if (!isDraggingRef.current) return
      const touch = e.touches[0]
      setLegendPos(prev => ({
        x: touch.clientX - dragStartRef.current.x,
        y: touch.clientY - dragStartRef.current.y
      }))
      if (e.cancelable) e.preventDefault() // prevent page scroll while dragging legend
    }

    const handleTouchEnd = () => {
      isDraggingRef.current = false
    }

    if (activeTab === 'glyphs') {
      document.addEventListener('mousemove', handleMouseMove)
      document.addEventListener('mouseup', handleMouseUp)
      document.addEventListener('touchmove', handleTouchMove, { passive: false })
      document.addEventListener('touchend', handleTouchEnd)
    }

    return () => {
      document.removeEventListener('mousemove', handleMouseMove)
      document.removeEventListener('mouseup', handleMouseUp)
      document.removeEventListener('touchmove', handleTouchMove)
      document.removeEventListener('touchend', handleTouchEnd)
    }
  }, [activeTab, legendPos.x, legendPos.y])

  // Worker state is now handled within the effect directly

  const [downloadingFont, setDownloadingFont] = useState(false)
  const [proofFontUrl, setProofFontUrl] = useState(null)
  // Proofs tab "Compare spacing" mode: stacks the same proof text rendered
  // with kerning axes forced off (baseline) above the current axes, so
  // manual + optical kerning / sidebearingScale changes are easy to spot.
  const [compareSpacing, setCompareSpacing] = useState(
    () => new URLSearchParams(window.location.search).get('compareSpacing') === '1'
  )
  const [baselineGlyphData, setBaselineGlyphData] = useState(null)
  const [classicBook, setClassicBook] = useState(() => {
    const params = new URLSearchParams(window.location.search)
    if (params.get('proof') !== 'classic') return null
    const idx = parseInt(params.get('book'))
    return (!isNaN(idx) && idx >= 0 && idx < classicBooks.length) ? classicBooks[idx] : null
  })

  const handleDownloadFont = () => {
    setDownloadingFont(true)
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    worker.onmessage = (e) => {
      const { result, error } = e.data
      worker.terminate()
      setDownloadingFont(false)
      if (error) {
        console.error('Font generation error:', error)
      } else {
        downloadFont(result, axes, defaultAxes)
      }
    }
    worker.postMessage({ id: 1, type: 'fontData', args: [axes] })
  }

  const renderIdRef = useRef(0)
  // Separate counter for the "Compare spacing" baseline fetch — it runs
  // concurrently with the main proofs fontPreview effect, which uses
  // renderIdRef to discard stale responses. Sharing the counter would bump
  // it out from under that effect's in-flight request and drop its result.
  const baselineRenderIdRef = useRef(0)
  const loadingRef = useRef(false)
  const previewRef = useRef(null)
  const activeTabRef = useRef(activeTab)
  const prevEffectTabRef = useRef(null)
  useEffect(() => { activeTabRef.current = activeTab }, [activeTab])

  const handleWheelZoom = useCallback((e) => {
    if (!e.ctrlKey) return
    e.preventDefault()
    const tab = activeTabRef.current
    // negate delta: on Mac+Chrome, Ctrl+scroll-down gives negative deltaY; we want down = zoom in
    const clampedDelta = Math.max(-200, Math.min(200, e.deltaY))
    const scaleFactor = 1 - clampedDelta * 0.001
    setTabZooms(prev => ({
      ...prev,
      [tab]: Math.max(0.1, Math.min(5.0, prev[tab] * scaleFactor))
    }))
  }, [])

  useEffect(() => {
    const el = previewRef.current
    if (!el) return
    el.addEventListener('wheel', handleWheelZoom, { passive: false })
    return () => el.removeEventListener('wheel', handleWheelZoom)
  }, [handleWheelZoom])

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
    if (activeTab !== prevEffectTabRef.current) {
      setWorkerResult(null)
      prevEffectTabRef.current = activeTab
    }
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
      args = [text, axes, false]
    } else if (activeTab === 'glyphs') {
      typeReq = 'glyphsFromDefs'
      args = [glyphsDefsText, { ...axes, filled: glyphsFilled }]
    } else if (activeTab === 'tweens') {
      const char = text.length > 0 ? text[0] : 'a'
      typeReq = 'tweens'
      const boxWidth = 150 * zoom
      const availableWidth = previewRef.current?.clientWidth ?? window.innerWidth
      const steps = Math.max(2, Math.floor((availableWidth + 10) / (boxWidth + 10)))
      args = [char, axes, steps]
    } else if (activeTab === 'visualDiffs') {
      if (compareMode === 'font') {
        // Compare-font mode is rendered on the main thread from dactylGlyphData
        // (see the dedicated effect / useMemo below) — skip the F# diff worker.
        setLoading(false)
        clearTimeout(timer)
        worker.terminate()
        return
      }
      typeReq = 'visualDiffs'
      const { axesA, axesB, labelA, labelB } = getDiffAxes(axes, diffConfig)
      args = [text || allChars, axesA, axesB, labelA, labelB]
    } else if (activeTab === 'grow') {
      if (supportsWebGL2) {
        // GPU path has its own dedicated effect — skip
        setLoading(false)
        clearTimeout(timer)
        worker.terminate()
        return
      }
      if (!text) {
        setWorkerResult("")
        setLoading(false)
        clearTimeout(timer)
        worker.terminate()
        return
      }
      typeReq = 'growth'
      args = [text, axes, growParams]
    } else if (activeTab === 'proofs') {
      // Proofs has its own dedicated effect — skip
      setLoading(false)
      clearTimeout(timer)
      worker.terminate()
      return
    } else if (activeTab === 'splines' || activeTab === 'splineGrid') {
      // SplineEditor and SplineGrid have their own workers — skip
      setLoading(false)
      clearTimeout(timer)
      worker.terminate()
      return
    }

    if (typeReq) {
      worker.postMessage({ id, type: typeReq, args })
    }

    return () => {
      clearTimeout(timer)
      worker.terminate()
    }
  }, [text, axes, activeTab, glyphsDefsText, glyphsFilled, diffConfig, compareMode, growParams])

  // Close the Grow download-format menu on outside click / Escape.
  useEffect(() => {
    if (!growMenuOpen) return
    const onDown = (e) => {
      if (growMenuRef.current && !growMenuRef.current.contains(e.target)) setGrowMenuOpen(false)
    }
    const onKey = (e) => { if (e.key === 'Escape') setGrowMenuOpen(false) }
    document.addEventListener('mousedown', onDown)
    document.addEventListener('keydown', onKey)
    return () => {
      document.removeEventListener('mousedown', onDown)
      document.removeEventListener('keydown', onKey)
    }
  }, [growMenuOpen])

  // Dedicated effect for proofs tab: generates full font and builds a data URL.
  // Deps are [axes, activeTab] only — switching proof text doesn't re-trigger.
  // Old font stays visible until the new one arrives (proofFontUrl is not cleared).
  useEffect(() => {
    if (activeTab !== 'proofs') return

    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    const id = ++renderIdRef.current
    setLoading(true)
    loadingRef.current = true
    setError(null)

    const timer = setTimeout(() => {
      if (id === renderIdRef.current && loadingRef.current) setShowProgress(true)
    }, 300)

    worker.onmessage = (e) => {
      const { id: msgId, result, error } = e.data
      if (msgId !== renderIdRef.current) return
      clearTimeout(timer)
      if (error) { setError(error) }
      else { setProofFontUrl(result); setError(null) }
      setLoading(false)
      loadingRef.current = false
      setShowProgress(false)
    }

    worker.postMessage({ id, type: 'fontPreview', args: [axes] })

    return () => {
      clearTimeout(timer)
      worker.terminate()
    }
  }, [axes, activeTab])

  // Compare-spacing baseline: fetch glyph data with kerning axes forced off,
  // so "Compare spacing" can stack a no-kerning render above the live one.
  // Only runs while the toggle is on to avoid the extra font build otherwise.
  useEffect(() => {
    if (activeTab !== 'proofs' || !compareSpacing) return

    const baselineAxes = { ...axes, manualKerning: false, opticalKerning: false, sidebearingScale: 1.0 }
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    worker.onmessage = (e) => {
      const { result, error } = e.data
      worker.terminate()
      if (!error) setBaselineGlyphData(result)
    }
    worker.postMessage({ id: ++baselineRenderIdRef.current, type: 'fontData', args: [baselineAxes] })
    return () => worker.terminate()
  }, [axes, activeTab, compareSpacing])

  // Dedicated effect for the Grow tab GPU path: rebuild the growth field only
  // when text/axes change.  growParams are shader uniforms and don't re-trigger.
  useEffect(() => {
    if (activeTab !== 'grow' || !supportsWebGL2) return
    if (!text) { setGrowField(null); return }

    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    const id = ++renderIdRef.current
    setLoading(true)
    loadingRef.current = true
    setError(null)
    setProgressValue(0)

    const timer = setTimeout(() => {
      if (id === renderIdRef.current && loadingRef.current) setShowProgress(true)
    }, 400)

    worker.onmessage = (e) => {
      const { id: msgId, result, error, type, value } = e.data
      if (msgId !== renderIdRef.current) return
      if (type === 'progress') {
        setProgressValue(value)
        if (value > 0) setShowProgress(true)
        return
      }
      clearTimeout(timer)
      if (error) { setError(error) }
      else { setGrowField(result); setError(null) }
      setLoading(false)
      loadingRef.current = false
      setShowProgress(false)
    }

    worker.postMessage({ id, type: 'growthField', args: [text, axes] })

    return () => {
      clearTimeout(timer)
      worker.terminate()
    }
  }, [text, axes, activeTab, supportsWebGL2])

  // Inject/update the @font-face rule whenever a new proof font data URL arrives.
  useEffect(() => {
    if (!proofFontUrl) return
    let el = document.getElementById('dactyl-proof-font')
    if (!el) {
      el = document.createElement('style')
      el.id = 'dactyl-proof-font'
      document.head.appendChild(el)
    }
    el.textContent = `@font-face { font-family: 'DactylPreview'; src: url('${proofFontUrl}') format('opentype'); }`
  }, [proofFontUrl])

  // Build the no-kerning baseline @font-face from the fetched glyph data.
  const baselineFontUrl = useMemo(() => {
    if (!compareSpacing || !baselineGlyphData) return null
    try { return buildFontDataUrl(baselineGlyphData, 'DactylBaseline') } catch { return null }
  }, [compareSpacing, baselineGlyphData])

  useEffect(() => {
    if (!baselineFontUrl) return
    let el = document.getElementById('dactyl-baseline-font')
    if (!el) {
      el = document.createElement('style')
      el.id = 'dactyl-baseline-font'
      document.head.appendChild(el)
    }
    el.textContent = `@font-face { font-family: 'DactylBaseline'; src: url('${baselineFontUrl}') format('opentype'); }`
  }, [baselineFontUrl])

  // Compare-font mode: fetch Dactyl's outlines for the current axes once per
  // axes change. Used to build the vector overlay and (for text-mode sources)
  // the DactylCompare @font-face.
  useEffect(() => {
    if (activeTab !== 'visualDiffs' || compareMode !== 'font') return
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    worker.onmessage = (e) => {
      const { result, error } = e.data
      worker.terminate()
      if (error) setCompareError(error)
      else setDactylGlyphData(result)
    }
    worker.postMessage({ id: ++renderIdRef.current, type: 'fontData', args: [axes] })
    return () => worker.terminate()
  }, [axes, activeTab, compareMode])

  // Vector overlay SVG (outline sources). Rebuilt when the font, alignment,
  // text or Dactyl outlines change.
  const compareSvg = useMemo(() => {
    if (compareMode !== 'font' || !dactylGlyphData) return null
    if (!compareFont || compareFont.kind !== 'outline') return null
    try {
      return buildCompareOverlaySvg(dactylGlyphData, compareFont.font, text || allChars, 'cap', compareFont.displayName, compareSize)
    } catch (e) {
      console.error('compare overlay failed', e)
      return null
    }
  }, [compareMode, compareFont, dactylGlyphData, compareSize, text])

  // DactylCompare @font-face for text-mode comparison (Dactyl side rendered via CSS).
  const dactylCompareUrl = useMemo(() => {
    if (compareMode !== 'font' || compareFont?.kind !== 'text' || !dactylGlyphData) return null
    try { return buildFontDataUrl(dactylGlyphData, 'DactylCompare') } catch { return null }
  }, [compareMode, compareFont, dactylGlyphData])

  useEffect(() => {
    if (!dactylCompareUrl) return
    let el = document.getElementById('dactyl-compare-font')
    if (!el) {
      el = document.createElement('style')
      el.id = 'dactyl-compare-font'
      document.head.appendChild(el)
    }
    el.textContent = `@font-face { font-family: 'DactylCompare'; src: url('${dactylCompareUrl}') format('opentype'); }`
  }, [dactylCompareUrl])

  const renderContent = () => {
    if (error) return <div style={{ color: 'red' }}>Error: {error}</div>
    if (!workerResult && loading && activeTab !== 'tweens') {
      // Optional: return <div style={{padding: '20px'}}>Generating...</div> 
      // But user asked for progress bar at top, so maybe leave blank or keep old?
      // If we return null, it might flash.
    }

    // SplineEditor manages its own state/worker — render immediately
    if (activeTab === 'splines') {
      return <SplineEditor axes={axes} zoom={zoom} />
    }

    if (activeTab === 'splineGrid') {
      return <SplineGrid />
    }

    // Proofs tab uses CSS font rendering — bypass SVG result check
    if (activeTab === 'proofs') {
      const proofStyle = (fontFamily) => ({
        fontFamily,
        fontSize: `${18 * zoom}pt`,
        lineHeight: 1.4,
        whiteSpace: 'pre-wrap',
        textAlign: 'left',
        padding: '20px',
        color: '#000',
      })

      if (compareSpacing) {
        return (
          <div className="proof-compare">
            <div className="proof-compare-panel">
              <div className="proof-compare-label">Before &mdash; no kerning</div>
              <div
                className="proof-text"
                style={proofStyle(baselineFontUrl ? "'DactylBaseline', monospace" : 'monospace')}
              >
                {text}
              </div>
            </div>
            <div className="proof-compare-panel">
              <div className="proof-compare-label">After &mdash; current axes</div>
              <div
                className="proof-text"
                style={proofStyle(proofFontUrl ? "'DactylPreview', monospace" : 'monospace')}
              >
                {text}
              </div>
            </div>
          </div>
        )
      }

      return (
        <div className="proof-text" style={proofStyle(proofFontUrl ? "'DactylPreview', monospace" : 'monospace')}>
          {text}
        </div>
      )
    }

    // Visual Diffs has its own renderer (axis worker SVG or compare-font mode).
    if (activeTab === 'visualDiffs') return renderVisualDiffs()

    // Grow tab GPU path: render the field via the WebGL canvas (sliders are
    // shader uniforms).  Falls through to the worker SVG result without WebGL2.
    if (activeTab === 'grow' && supportsWebGL2) {
      if (!growField) return null
      return <GrowCanvas field={growField} params={growParams} zoom={zoom} />
    }

    // Safety check: ensure result matches expected type for tab
    const content = workerResult
    if (!content) return null

    try {
      if (activeTab === 'font' || activeTab === 'grow') {
        if (typeof content !== 'string') return null
        return <div
          className="svg-container"
          dangerouslySetInnerHTML={{ __html: content }}
        />
      } else if (activeTab === 'glyphs') {
        if (typeof content !== 'string') return null
        const visibilityClasses = Object.entries(layerVisibility)
          .filter(([_, visible]) => !visible)
          .map(([key]) => `hide-${key}`)
          .join(' ')

        return (
          <div className={`glyphs-container ${visibilityClasses}`}>
            <div
              className="svg-container"
              dangerouslySetInnerHTML={{ __html: content }}
            />
          </div>
        )
      } else if (activeTab === 'tweens') {
        if (typeof content !== 'object') return null
        // content is { [ctrlName]: [ { val, svg } ] }
        // tweenFilter is kept in sync with ?tween= URL param via popstate listener

        return (
          <div className="tweens-grid">
            {(() => {
              const EXCLUDED_TWEEN_AXES = ['tracking', 'leading', 'sidebearingScale']
              return controlDefinitions
                .filter(c => !EXCLUDED_TWEEN_AXES.includes(c.name) && c.category !== 'debug')
                .filter(c => !tweenFilter || c.name === tweenFilter)
                .map(ctrl => {
                  const variations = content[ctrl.name]
                  if (!variations) return null

                  const rowVariations = variations.map((v, i) => {
                    const boxWidth = 150 * zoom
                    return (
                      <div key={`${ctrl.name}-${i}`} className="tween-item" style={{ minWidth: boxWidth + 'px', width: boxWidth + 'px' }}>
                        <div dangerouslySetInnerHTML={{ __html: v.svg }} />
                        <div style={{ fontSize: '0.7em' }}>{ctrl.type_ === 'checkbox' ? (v.val === 'diff' ? 'diff' : v.val ? 'true' : 'false') : v.val.toFixed(2)}</div>
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
      }
      return null
    } catch (e) {
      console.error("Error generating Content:", e)
      return <div style={{ color: 'red' }}>Error: {e.message}</div>
    }
  }

  // Visual Diffs preview: compare-font mode renders on the main thread, axis
  // mode uses the worker SVG result handled by renderContent above.
  const renderVisualDiffs = () => {
    if (compareMode !== 'font') {
      const content = workerResult
      if (typeof content !== 'string') return null
      return <div className="svg-container" dangerouslySetInnerHTML={{ __html: content }} />
    }
    if (compareError) return <div style={{ color: 'red', padding: 20 }}>Error: {compareError}</div>
    if (!compareFont) {
      return <div style={{ padding: 20, color: '#666' }}>
        Pick a font to compare with Dactyl — upload a .ttf/.otf/.woff, choose a Google Font, or list your system fonts.
      </div>
    }
    if (compareFont.kind === 'text') {
      return <FontCompareTextOverlay
        text={text || allChars}
        fontFamily={compareFont.fontFamily}
        dactylFamily="DactylCompare"
        labelB={compareFont.displayName}
        sizeScale={compareSize}
      />
    }
    if (!compareSvg) return <div style={{ padding: 20, color: '#666' }}>Generating…</div>
    return <div className="svg-container" dangerouslySetInnerHTML={{ __html: compareSvg }} />
  }

  // Render the current Grow view to a vector SVG string via a one-off worker.
  // Used for both SVG and PNG downloads so the saved output matches the rule
  // exactly, independent of which preview path (GPU / fallback) is on screen.
  const requestGrowthSvg = () => new Promise((resolve, reject) => {
    if (!text) { resolve(''); return }
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    worker.onmessage = (e) => {
      worker.terminate()
      if (e.data.error) reject(new Error(e.data.error))
      else resolve(e.data.result)
    }
    worker.onerror = (err) => { worker.terminate(); reject(err) }
    worker.postMessage({ id: 0, type: 'growth', args: [text, axes, growParams] })
  })

  const handleDownloadGrow = async (format) => {
    setGrowMenuOpen(false)
    setSavingGrow(true)
    setError(null)
    try {
      const svg = await requestGrowthSvg()
      if (!svg) return
      const base = growFilenameBase(text)
      if (format === 'svg') {
        downloadBlob(svgBlob(svg), `${base}.svg`)
      } else {
        // Transparent background: the layered look drops onto any surface.
        const png = await svgToPngBlob(svg, { scale: 3, background: null })
        downloadBlob(png, `${base}.png`)
      }
    } catch (e) {
      setError(`Grow ${format.toUpperCase()} export failed: ${e.message}`)
    } finally {
      setSavingGrow(false)
    }
  }

  // Copy the grown logotype as a PNG to the clipboard.  ClipboardItem is fed a
  // Promise<Blob> so Safari can defer the async rasterise inside the user
  // gesture; Chrome/Firefox accept it too.
  const handleCopyGrow = async () => {
    if (!navigator.clipboard || typeof ClipboardItem === 'undefined') {
      setError('Clipboard image copy is not supported in this browser')
      return
    }
    setSavingGrow(true)
    setError(null)
    try {
      const svg = await requestGrowthSvg()
      if (!svg) throw new Error('nothing to copy')
      const png = await svgToPngBlob(svg, { scale: 3, background: null })
      await navigator.clipboard.write([new ClipboardItem({ 'image/png': png })])
      setGrowCopied(true)
      setTimeout(() => setGrowCopied(false), 1500)
    } catch (e) {
      setError(`Grow copy failed: ${e.message}`)
    } finally {
      setSavingGrow(false)
    }
  }

  const handleControlChange = (name, value) => {
    setAxes(prev => ({ ...prev, [name]: value }))
  }

  const handleReset = () => {
    setAxes({ ...defaultAxes })
  }

  // "Debug" master checkbox in the glyphs floating tools: reflects/controls all
  // non-spline layer toggles + Filled at once. Individual checkboxes below it
  // can still be changed independently afterwards, overriding the parent.
  const debugValues = [...DEBUG_LAYER_KEYS.map(k => layerVisibility[k]), glyphsFilled]
  const allDebugOn = debugValues.every(Boolean)
  const noDebugOn = debugValues.every(v => !v)
  const setDebugMasterRef = el => {
    if (el) el.indeterminate = !allDebugOn && !noDebugOn
  }

  const handleDebugMasterChange = (checked) => {
    setLayerVisibility(prev => {
      const next = { ...prev }
      DEBUG_LAYER_KEYS.forEach(k => { next[k] = checked })
      return next
    })
    setGlyphsFilled(checked)
  }

  // Only touch a fraction of axes per click, and bias sampled values toward
  // the default (nudge, don't reroll) so extreme/rare effects don't stack up.
  const RANDOMIZE_PROBABILITY = 0.35
  const RANDOMIZE_SPREAD = 0.3

  const handleRandom = () => {
    const newAxes = { ...axes }
    controlDefinitions.forEach(ctrl => {
      if (ctrl.category === 'experimental' || ctrl.category === 'debug') return
      // reset to default before re-randomizing, so clicks don't compound
      newAxes[ctrl.name] = defaultAxes[ctrl.name]
      if (Math.random() > RANDOMIZE_PROBABILITY) return

      if (ctrl.type_ === 'checkbox') {
        newAxes[ctrl.name] = Math.random() > 0.5
      } else {
        const center = defaultAxes[ctrl.name] ?? (ctrl.min + ctrl.max) / 2
        const range = ctrl.max - ctrl.min
        // triangular distribution centered on 0: most draws land near `center`
        const offset = (Math.random() - Math.random()) * range * RANDOMIZE_SPREAD
        newAxes[ctrl.name] = Math.min(ctrl.max, Math.max(ctrl.min, center + offset))
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
                style={{ cursor: 'pointer', fontWeight: 'bold', marginBottom: '10px', display: 'flex', alignItems: 'center', gap: '8px' }}
              >
                <span className="material-symbols-outlined" style={{ opacity: 0.7 }}>
                  {categoryIcons[category] || 'settings'}
                </span>
                <span className="category-title" style={{ flex: 1 }}>
                  {category.charAt(0).toUpperCase() + category.slice(1)}
                </span>
                <span className="material-symbols-outlined" style={{ fontSize: '18px', opacity: 0.5 }}>
                  {openCategories[category] ? 'expand_more' : 'chevron_right'}
                </span>
              </div>

              {openCategories[category] && (
                <div className="category-content" style={{ paddingLeft: '10px' }}>
                  {controls.map(ctrl => (
                    <div key={ctrl.name} className="control-group" title={ctrl.description}>
                      <label>
                        {ctrl.name}
                      </label>
                      <div className="control-input">
                        <div className="slider-container">
                          {ctrl.type_ === 'range' ? (
                            <div className="range-wrapper">
                              <input
                                type="range"
                                min={ctrl.min}
                                max={ctrl.max}
                                step={ctrl.step}
                                value={axes[ctrl.name]}
                                onChange={e => handleControlChange(ctrl.name, parseFloat(e.target.value))}
                                className="modern-slider"
                              />
                              <div className="slider-track-fill" style={{ width: `${((axes[ctrl.name] || 0) - ctrl.min) / (ctrl.max - ctrl.min) * 100}%` }}></div>
                            </div>
                          ) : (
                            <label className="toggle-switch">
                              <input
                                type="checkbox"
                                checked={axes[ctrl.name]}
                                onChange={e => handleControlChange(ctrl.name, e.target.checked)}
                              />
                            </label>
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
            <button className={`tab-button ${activeTab === 'glyphs' ? 'active' : ''}`} onClick={() => setTabWithUrl('glyphs')}>Glyphs</button>
            <button className={`tab-button ${activeTab === 'tweens' ? 'active' : ''}`} onClick={() => setTabWithUrl('tweens')}>Tweens</button>
            <button className={`tab-button ${activeTab === 'visualDiffs' ? 'active' : ''}`} onClick={() => setTabWithUrl('visualDiffs')}>Visual Diffs</button>
            <button className={`tab-button ${activeTab === 'splines' ? 'active' : ''}`} onClick={() => setTabWithUrl('splines')}>Splines</button>
            <button className={`tab-button ${activeTab === 'splineGrid' ? 'active' : ''}`} onClick={() => setTabWithUrl('splineGrid')}>Spline Grid</button>
            <button className={`tab-button ${activeTab === 'proofs' ? 'active' : ''}`} onClick={() => setTabWithUrl('proofs')}>Proofs</button>
            <button className={`tab-button ${activeTab === 'grow' ? 'active' : ''}`} onClick={() => setTabWithUrl('grow')}>Grow</button>
          </div>
          {activeTab === 'grow' && (
            <div className="grow-controls" style={{ display: 'flex', alignItems: 'center', gap: '12px', flexWrap: 'wrap' }}>
              <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                grow
                <input
                  type="range" min="0" max="1" step="0.05"
                  value={growParams.grow}
                  onChange={e => setGrowParams(p => ({ ...p, grow: parseFloat(e.target.value) }))}
                />
                <span style={{ minWidth: '2.5em' }}>{growParams.grow.toFixed(2)}</span>
              </label>
              <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                gap
                <input
                  type="range" min="5" max="100" step="5"
                  value={growParams.gap}
                  onChange={e => setGrowParams(p => ({ ...p, gap: parseFloat(e.target.value) }))}
                />
                <span style={{ minWidth: '2em' }}>{growParams.gap}</span>
              </label>
              <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                layers
                <input
                  type="checkbox"
                  checked={growParams.layers}
                  onChange={e => setGrowParams(p => ({ ...p, layers: e.target.checked }))}
                />
              </label>
              {supportsWebGL2 && (
                <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                  animate
                  <input
                    type="checkbox"
                    checked={growParams.animate}
                    onChange={e => setGrowParams(p => ({ ...p, animate: e.target.checked }))}
                  />
                </label>
              )}
              <span style={{ display: 'flex', alignItems: 'center', gap: '4px', marginLeft: '4px' }}>
                <button
                  className="icon-button"
                  onClick={handleCopyGrow}
                  disabled={savingGrow || !text}
                  title="Copy PNG to clipboard"
                >
                  <span className="material-symbols-outlined" style={{ fontSize: '20px' }}>
                    {growCopied ? 'check' : 'content_copy'}
                  </span>
                </button>
                {/* Download defaults to PNG; the caret opens a PNG/SVG menu. */}
                <span ref={growMenuRef} className="grow-download-split" style={{ display: 'flex', alignItems: 'center', gap: '4px', position: 'relative' }}>
                  <button
                    className="icon-button"
                    onClick={() => handleDownloadGrow('png')}
                    disabled={savingGrow || !text}
                    title="Download PNG (transparent, high-res)"
                  >
                    <span className="material-symbols-outlined" style={{ fontSize: '20px' }}>
                      {savingGrow ? 'hourglass_empty' : 'download'}
                    </span>
                  </button>
                  <button
                    className="icon-button"
                    onClick={() => setGrowMenuOpen(o => !o)}
                    disabled={savingGrow || !text}
                    title="Choose download format"
                    aria-haspopup="menu"
                    aria-expanded={growMenuOpen}
                    style={{ width: '24px', minWidth: '24px', padding: '6px 0' }}
                  >
                    <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>arrow_drop_down</span>
                  </button>
                  {growMenuOpen && (
                    <div
                      role="menu"
                      style={{
                        position: 'absolute', top: '100%', right: 0, marginTop: '4px', zIndex: 20,
                        background: 'var(--panel-bg)', border: '1px solid var(--border-color)',
                        borderRadius: 'var(--radius-md)', boxShadow: '0 4px 12px rgba(0,0,0,0.4)', overflow: 'hidden', minWidth: '160px',
                      }}
                    >
                      <button className="grow-menu-item" role="menuitem" onClick={() => handleDownloadGrow('png')}>
                        <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>image</span>
                        PNG <span style={{ opacity: 0.55, marginLeft: 'auto', fontSize: '0.8em' }}>transparent</span>
                      </button>
                      <button className="grow-menu-item" role="menuitem" onClick={() => handleDownloadGrow('svg')}>
                        <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>polyline</span>
                        SVG <span style={{ opacity: 0.55, marginLeft: 'auto', fontSize: '0.8em' }}>vector</span>
                      </button>
                    </div>
                  )}
                </span>
              </span>
            </div>
          )}
          {activeTab === 'proofs' && (
            <div className="proof-chips">
              {proofCases.map(k => (
                <button
                  key={k}
                  className={`proof-chip ${proofCase === k ? 'selected' : ''}`}
                  onClick={() => setProofCaseWithUrl(k)}
                >
                  {proofLabels[k]}
                </button>
              ))}
              <button
                className={`proof-chip ${proofCase === 'classic' ? 'selected' : ''}`}
                onClick={handlePickClassic}
                title="Pick a random classic"
              >
                Classic &#x21BA;
              </button>
              {proofCase === 'classic' && classicBook && (
                <span className="proof-book-title">
                  {classicBook.title} &mdash; {classicBook.author}
                </span>
              )}
              <label className="proof-compare-toggle">
                <input
                  type="checkbox"
                  checked={compareSpacing}
                  onChange={e => setCompareSpacingWithUrl(e.target.checked)}
                />
                Compare spacing
              </label>
            </div>
          )}
          {activeTab === 'visualDiffs' && (() => {
            const ctrl = controlDefinitions.find(c => c.name === diffConfig.axis)
            const axisControls = (
              <>
                <label htmlFor="diff-axis-select">Diff axis:</label>
                <select
                  id="diff-axis-select"
                  value={diffConfig.axis}
                  onChange={e => handleDiffAxisChange(e.target.value)}
                >
                  <option value={SPLINE_ENGINE}>Spline engine (old vs new)</option>
                  {Object.entries(controlsByCategory).map(([category, controls]) => (
                    <optgroup key={category} label={category}>
                      {controls.map(c => <option key={c.name} value={c.name}>{c.name}</option>)}
                    </optgroup>
                  ))}
                </select>
                {ctrl && ctrl.type_ === 'checkbox' && (
                  <span className="diff-onoff">
                    <span className="diff-label-a">{diffConfig.valueA ? 'on' : 'off'}</span>
                    {' vs '}
                    <span className="diff-label-b">{diffConfig.valueB ? 'on' : 'off'}</span>
                  </span>
                )}
                {ctrl && ctrl.type_ === 'range' && (
                  <>
                    <label className="diff-label-a" htmlFor="diff-value-a">A:</label>
                    <input
                      id="diff-value-a"
                      type="number"
                      min={ctrl.min}
                      max={ctrl.max}
                      step={ctrl.step}
                      value={diffConfig.valueA}
                      onChange={e => {
                        const v = parseFloat(e.target.value)
                        if (!isNaN(v)) setDiffConfigWithUrl({ ...diffConfig, valueA: v })
                      }}
                    />
                    <label className="diff-label-b" htmlFor="diff-value-b">B:</label>
                    <input
                      id="diff-value-b"
                      type="number"
                      min={ctrl.min}
                      max={ctrl.max}
                      step={ctrl.step}
                      value={diffConfig.valueB}
                      onChange={e => {
                        const v = parseFloat(e.target.value)
                        if (!isNaN(v)) setDiffConfigWithUrl({ ...diffConfig, valueB: v })
                      }}
                    />
                  </>
                )}
                {ctrl && (
                  <button
                    className="icon-button"
                    title="Swap A and B"
                    onClick={() => setDiffConfigWithUrl({ ...diffConfig, valueA: diffConfig.valueB, valueB: diffConfig.valueA })}
                  >
                    <span className="material-symbols-outlined">swap_horiz</span>
                  </button>
                )}
              </>
            )
            return (
              <FontCompareControls
                mode={compareMode}
                onModeChange={setCompareModeWithUrl}
                size={compareSize}
                onSizeChange={setCompareSizeWithUrl}
                font={compareFont}
                onFontChange={(f) => { setCompareFont(f); setCompareError(null) }}
                onError={setCompareError}
                axisControls={axisControls}
              />
            )
          })()}
          {activeTab === 'font' && (
            <button
              className="icon-button"
              onClick={handleDownloadFont}
              disabled={downloadingFont}
              title="Download Font (OTF)"
            >
              <span className="material-symbols-outlined">
                {downloadingFont ? 'hourglass_empty' : 'download'}
              </span>
            </button>
          )}
        </div>

        <div className={`input-area ${activeTab === 'glyphs' ? 'with-defs' : ''}`} style={activeTab === 'splines' || activeTab === 'splineGrid' ? { display: 'none' } : undefined}>
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
                const defaults = { font: allChars, glyphs: 'font', tweens: 'a', visualDiffs: allChars, splines: '', splineGrid: '', proofs: proofTexts[proofCase], grow: 'dactyl' }
                setText(defaults[activeTab])
              }}
              title="Reset Text to Default"
            >
              <span className="material-symbols-outlined" style={{ fontSize: '16px' }}>restart_alt</span>
            </button>
          </div>
          {activeTab === 'glyphs' && (
            <div className="glyph-defs-panel" style={{ display: 'flex', flexDirection: 'column', gap: '5px' }}>
              <h3 style={{ margin: 0 }}>Glyph Definitions{' '}
                <a
                  href="https://github.com/terryspitz/dactyl-font/blob/master/docs/DactylGlyphs.md"
                  target="_blank"
                  rel="noopener noreferrer"
                  style={{ fontWeight: 'normal', textDecoration: 'underline' }}
                >
                  (docs)
                </a>
              </h3>
              <textarea
                value={glyphsDefsText}
                onChange={e => setGlyphsDefsText(e.target.value)}
                style={{ width: '100%', flex: '1', minHeight: '100px', fontFamily: 'monospace', resize: 'vertical' }}
                spellCheck="false"
              />
              <div className="helper-key" style={{ fontSize: '0.85em', color: '#666' }}>
                <strong>Key:</strong> y: (t)op, (x)-height, (h)alf, (b)ottom, (d)escender, (o)ffset in, (e)xtended out. <br />
                x: (l)eft, (c)enter, (r)ight, (w)ide. Solo point → dot. <br />
                Dirs: N,S,E,W. Lines: (-) straight, (~) curve. Brackets mean 'fit this coordinate instead'. <br />
                Repeats average coordinates (e.g. "bt"="h"); a digit repeats the letter before it, so "b2t"="bbt" and "r4c"="rrrrc".
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
          <div ref={previewRef} className={`preview-content ${activeTab === 'splines' ? 'spline-mode' : ''}`} style={activeTab === 'splineGrid' ? { padding: 0 } : undefined}>
            <div style={activeTab === 'splines' ? { display: 'contents' } : { transform: (activeTab === 'tweens' || activeTab === 'proofs' || (activeTab === 'grow' && supportsWebGL2)) ? 'none' : `scale(${zoom})`, transformOrigin: 'top left', minHeight: '100%' }}>
              {renderContent()}
            </div>
          </div>
        </div>

        {activeTab === 'glyphs' && (
          <div
            className="glyph-legend"
            onMouseDown={handleLegendMouseDown}
            onTouchStart={handleLegendTouchStart}
            style={{
              transform: `translate(${legendPos.x}px, ${legendPos.y}px)`,
              cursor: 'move',
              userSelect: 'none'
            }}
          >
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
              <span>
                <a href="#" onClick={(e) => { e.preventDefault(); setTabWithUrl('splines'); }} style={{ color: 'inherit', textDecoration: 'underline' }}>DactylSpline</a>
              </span>
            </div>
            <div className="legend-item legend-heading">
              <input
                ref={setDebugMasterRef}
                type="checkbox"
                checked={allDebugOn}
                onChange={e => handleDebugMasterChange(e.target.checked)}
              />
              <strong>Debug</strong>
            </div>
            <div className="legend-debug-group">
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
              <div className="legend-item">
                <input
                  type="checkbox"
                  checked={glyphsFilled}
                  onChange={e => setGlyphsFilled(e.target.checked)}
                />
                Filled
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  )
}

export default App
