import { useState, useMemo, useEffect, useRef, useCallback } from 'react'
import { generateSvg, defaultAxes, controlDefinitions, generateTweenSvg, getGlyphDefs, allChars, alphabetChars } from './lib/fable/Api' // Adjust path if needed
import SplineEditor from './SplineEditor'
import SplineGrid from './SplineGrid'
import GrowCanvas from './GrowCanvas'
import { downloadBlob, svgBlob, svgToPngBlob, growFilenameBase, filenameBase } from './growthExport'
import { LAYER_COLORS } from './growth'
import { DEFAULT_BRANCH_COLOR } from './branching'
import { downloadFont, buildFontDataUrl } from './fontExport'
import { buildCompareOverlaySvg } from './fontCompare'
import FontCompareControls from './FontCompareControls'
import FontCompareTextOverlay from './FontCompareTextOverlay'
import { proofTexts, proofLabels, proofCases, classicBooks } from './proofs'
import { randomizeAxes, buildGlyphAxes, newGlyphSeed } from './glyphRandom'
import './App.css'

// Special Visual Diffs option: compare the old spiro/spline2 engine vs the new dactyl spline
const SPLINE_ENGINE = 'spline_engine'

// Glyphs floating tools legend: non-spline layerVisibility keys grouped under "Debug"
const DEBUG_LAYER_KEYS = ['comb', 'tangents', 'guides', 'labels', 'knots']

// Generate tab defaults, factored out so the per-mode "reset" button can
// restore them without touching which mode is selected. Functions (not plain
// objects) so layerColors gets a fresh array each time, never a shared one.
const defaultGrowParams = () => ({
  grow: 0.7, gap: 30, growScale: 120, fuse: 0, warp: 0, layers: true, animate: false,
  color: '#000000', layerColors: [...LAYER_COLORS],
})
const defaultBranchParams = () => ({
  density: 52, influence: 40, killDistance: 8, stepSize: 6, iterations: 90, seed: 1,
  backbone: true, color: DEFAULT_BRANCH_COLOR, backboneColor: '#000000',
  maxReach: 140, baseRadius: 10, minRadius: 1.2, maxDepthForTaper: 14,
})

// Field grid spacing used for the "fast preview" toggle — coarser than the
// normal 3–6 cellFor(text) range, so slider drags stay responsive on slow
// modes (Grow especially) instead of recomputing at full resolution.
const PREVIEW_CELL = 10

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
      font: alphabetChars,
      glyphs: savedGlyphs !== null ? savedGlyphs : 'font',
      tweens: 'a',
      visualDiffs: allChars,
      splines: '',
      splineGrid: '',
      proofs: proofTexts.lowercase,
      generate: 'dactyl'
    }
  })
  const [glyphsDefsText, setGlyphsDefsText] = useState(() => {
    const initialText = tabTexts['glyphs'] || 'a'
    return getGlyphDefs(initialText, defaultAxes.alt_a_g)
  })
  const [axes, setAxes] = useState({ ...defaultAxes })
  // "Randomise every glyph": null = off, otherwise the seed that every
  // character's axes are derived from.  Holding a seed (rather than a big map of
  // per-glyph axes) is what makes the variant font stable — it only changes when
  // the button is clicked again or Reset clears it.
  const [glyphSeed, setGlyphSeed] = useState(null)
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
    return { font: zoom, glyphs: zoom, tweens: zoom, visualDiffs: zoom, splines: zoom, splineGrid: zoom, proofs: zoom, generate: zoom }
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
  // Generate tab: which generative mode is active. Two UI-facing values:
  // 'bubble' (constant-gap SDF inflation, implemented in growth.js/growParams
  // — code keeps the "grow" name since that's the algorithm's name) and
  // 'grow' (space-colonisation twigs, implemented in branching.js/branchParams
  // — code keeps the "branch" name). URL-addressable so a chosen mode is
  // shareable/deep-linkable, same as compareMode.
  const [generateMode, setGenerateMode] = useState(() => {
    const m = new URLSearchParams(window.location.search).get('mode')
    return m === 'grow' ? 'grow' : 'bubble'
  })
  // Bubble mode ('grow' internally): constant-gap growth parameters (see growth.js)
  const [growParams, setGrowParams] = useState(defaultGrowParams)
  // Grow mode ('branch' internally): space-colonisation branching parameters
  // (see branching.js). Dense/tight enough that twig coverage alone reads
  // legibly with the backbone off, not just with it on.
  const [branchParams, setBranchParams] = useState(defaultBranchParams)
  // Fast preview: render just the first character at PREVIEW_CELL resolution
  // instead of the full text, for responsive slider dragging on slow modes.
  const [fastPreview, setFastPreview] = useState(false)
  // Bubble mode GPU path: the worker builds the (d1, dOpp) field once per
  // text/axes/growScale change; other sliders only move shader uniforms (see
  // GrowCanvas.jsx). Without WebGL2 the tab falls back to the worker-side SVG render.
  const [growField, setGrowField] = useState(null)
  const [savingGrow, setSavingGrow] = useState(false)
  const [growCopied, setGrowCopied] = useState(false)
  const [growMenuOpen, setGrowMenuOpen] = useState(false)
  const growMenuRef = useRef(null)
  // Grow mode (branch-mode) export state — mirrors the Bubble mode ones above.
  const [savingBranch, setSavingBranch] = useState(false)
  const [branchCopied, setBranchCopied] = useState(false)
  const [branchMenuOpen, setBranchMenuOpen] = useState(false)
  const branchMenuRef = useRef(null)
  // Font tab image export (same PNG/SVG copy + download as Grow, on the canvas)
  const [savingFontImage, setSavingFontImage] = useState(false)
  const [fontCopied, setFontCopied] = useState(false)
  const [fontMenuOpen, setFontMenuOpen] = useState(false)
  const fontMenuRef = useRef(null)
  const supportsWebGL2 = useMemo(() => {
    try { return !!document.createElement('canvas').getContext('webgl2') } catch { return false }
  }, [])

  // Check URL on mount
  useEffect(() => {
    const params = new URLSearchParams(window.location.search)
    let view = params.get('view')
    if (view && ['font', 'glyphs', 'tweens', 'visualDiffs', 'splines', 'splineGrid', 'proofs', 'generate'].includes(view)) {
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

  const setGenerateModeWithUrl = (m) => {
    setGenerateMode(m)
    const url = new URL(window.location)
    if (m === 'grow') url.searchParams.set('mode', 'grow')
    else url.searchParams.delete('mode')
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
  const [classicBook, setClassicBook] = useState(() => {
    const params = new URLSearchParams(window.location.search)
    if (params.get('proof') !== 'classic') return null
    const idx = parseInt(params.get('book'))
    return (!isNaN(idx) && idx >= 0 && idx < classicBooks.length) ? classicBooks[idx] : null
  })

  // Per-glyph random axes, as the parallel (chars, axesList) arrays the F# API
  // takes.  Two variants so the whole-font one (export / proofs / comparisons)
  // doesn't churn every time the typed text changes.
  const perGlyphFontAxes = useMemo(
    () => glyphSeed === null ? null : buildGlyphAxes(allChars.replace(/\n/g, '') + ' ', glyphSeed, axes, controlDefinitions),
    [glyphSeed, axes]
  )
  const perGlyphTextAxes = useMemo(
    () => glyphSeed === null ? null : buildGlyphAxes(text || '', glyphSeed, axes, controlDefinitions),
    [glyphSeed, axes, text]
  )
  const perGlyphFontArgs = perGlyphFontAxes ? [perGlyphFontAxes.chars, perGlyphFontAxes.axesList] : []

  const handleDownloadFont = () => {
    setDownloadingFont(true)
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    worker.onmessage = (e) => {
      const { result, error, type } = e.data
      if (type === 'progress') return
      worker.terminate()
      setDownloadingFont(false)
      if (error) {
        console.error('Font generation error:', error)
      } else {
        downloadFont(result, axes, defaultAxes, glyphSeed)
      }
    }
    worker.postMessage({ id: 1, type: 'fontData', args: [axes, ...perGlyphFontArgs] })
  }

  // Render the Font tab's text to a tightly-cropped vector SVG via a one-off
  // worker.  The on-screen preview uses a fixed 6000x6000 viewBox (autoscale
  // off), which would export mostly empty space, so this re-renders with
  // autoscale on.  Per-glyph randomisation is carried through, so what you save
  // is what you see.
  const requestFontSvg = () => new Promise((resolve, reject) => {
    if (!text) { resolve(''); return }
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    worker.onmessage = (e) => {
      if (e.data.type === 'progress') return
      worker.terminate()
      if (e.data.error) reject(new Error(e.data.error))
      else resolve(e.data.result)
    }
    worker.onerror = (err) => { worker.terminate(); reject(err) }
    worker.postMessage(perGlyphTextAxes
      ? { id: 0, type: 'fontPerGlyph', args: [text, axes, perGlyphTextAxes.chars, perGlyphTextAxes.axesList, true] }
      : { id: 0, type: 'font', args: [text, axes, true] })
  })

  const fontImageBase = () =>
    filenameBase('dactyl', text, glyphSeed === null ? '' : `random${glyphSeed}`)

  const handleDownloadFontImage = async (format) => {
    setFontMenuOpen(false)
    setSavingFontImage(true)
    setError(null)
    try {
      const svg = await requestFontSvg()
      if (!svg) return
      const base = fontImageBase()
      if (format === 'svg') {
        downloadBlob(svgBlob(svg), `${base}.svg`)
      } else {
        // Transparent background, matching the Grow tab's export.
        downloadBlob(await svgToPngBlob(svg, { scale: 3, background: null }), `${base}.png`)
      }
    } catch (e) {
      setError(`Font ${format.toUpperCase()} export failed: ${e.message}`)
    } finally {
      setSavingFontImage(false)
    }
  }

  const handleCopyFontImage = async () => {
    if (!navigator.clipboard || typeof ClipboardItem === 'undefined') {
      setError('Clipboard image copy is not supported in this browser')
      return
    }
    setSavingFontImage(true)
    setError(null)
    try {
      const svg = await requestFontSvg()
      if (!svg) throw new Error('nothing to copy')
      const png = await svgToPngBlob(svg, { scale: 3, background: null })
      await navigator.clipboard.write([new ClipboardItem({ 'image/png': png })])
      setFontCopied(true)
      setTimeout(() => setFontCopied(false), 1500)
    } catch (e) {
      setError(`Font copy failed: ${e.message}`)
    } finally {
      setSavingFontImage(false)
    }
  }

  const renderIdRef = useRef(0)
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
      if (perGlyphTextAxes) {
        typeReq = 'fontPerGlyph'
        args = [text, axes, perGlyphTextAxes.chars, perGlyphTextAxes.axesList, false]
      } else {
        typeReq = 'font'
        args = [text, axes, false]
      }
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
    } else if (activeTab === 'generate') {
      if (generateMode === 'bubble' && supportsWebGL2) {
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
      // Fast preview: render just the first character at a coarser field
      // resolution, so dragging a slider stays responsive on slow modes
      // (Grow especially) instead of recomputing the full text every tick.
      const previewText = fastPreview ? (text.trim().charAt(0) || text) : text
      if (generateMode === 'grow') {
        typeReq = 'branch'
        args = [previewText, axes, fastPreview ? { ...branchParams, cell: PREVIEW_CELL } : branchParams]
      } else {
        typeReq = 'growth'
        args = [previewText, axes, fastPreview ? { ...growParams, cell: PREVIEW_CELL } : growParams]
      }
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
  }, [text, axes, activeTab, glyphsDefsText, glyphsFilled, diffConfig, compareMode, generateMode, growParams, branchParams, fastPreview, perGlyphTextAxes])

  // Close the Bubble download-format menu on outside click / Escape.
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

  // Close the Grow download-format menu on outside click / Escape.
  useEffect(() => {
    if (!branchMenuOpen) return
    const onDown = (e) => {
      if (branchMenuRef.current && !branchMenuRef.current.contains(e.target)) setBranchMenuOpen(false)
    }
    const onKey = (e) => { if (e.key === 'Escape') setBranchMenuOpen(false) }
    document.addEventListener('mousedown', onDown)
    document.addEventListener('keydown', onKey)
    return () => {
      document.removeEventListener('mousedown', onDown)
      document.removeEventListener('keydown', onKey)
    }
  }, [branchMenuOpen])

  // Same for the Font tab's download-format menu.
  useEffect(() => {
    if (!fontMenuOpen) return
    const onDown = (e) => {
      if (fontMenuRef.current && !fontMenuRef.current.contains(e.target)) setFontMenuOpen(false)
    }
    const onKey = (e) => { if (e.key === 'Escape') setFontMenuOpen(false) }
    document.addEventListener('mousedown', onDown)
    document.addEventListener('keydown', onKey)
    return () => {
      document.removeEventListener('mousedown', onDown)
      document.removeEventListener('keydown', onKey)
    }
  }, [fontMenuOpen])

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

    worker.postMessage({ id, type: 'fontPreview', args: [axes, ...perGlyphFontArgs] })

    return () => {
      clearTimeout(timer)
      worker.terminate()
    }
  }, [axes, activeTab, perGlyphFontAxes])

  // Dedicated effect for the Bubble mode GPU path: rebuild the growth field
  // when text/axes/growScale change (growScale sizes the field's padding, so
  // it needs a rebuild); the rest of growParams are shader uniforms and don't
  // re-trigger.
  useEffect(() => {
    if (activeTab !== 'generate' || generateMode !== 'bubble' || !supportsWebGL2) return
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

    const previewText = fastPreview ? (text.trim().charAt(0) || text) : text
    worker.postMessage({
      id, type: 'growthField',
      args: [previewText, axes, { growScale: growParams.growScale, cell: fastPreview ? PREVIEW_CELL : undefined }],
    })

    return () => {
      clearTimeout(timer)
      worker.terminate()
    }
  }, [text, axes, activeTab, generateMode, supportsWebGL2, growParams.growScale, fastPreview])

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

  // Compare-font mode: fetch Dactyl's outlines for the current axes once per
  // axes change. Used to build the vector overlay and (for text-mode sources)
  // the DactylCompare @font-face.
  useEffect(() => {
    if (activeTab !== 'visualDiffs' || compareMode !== 'font') return
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    setLoading(true)
    loadingRef.current = true
    setShowProgress(false)
    setProgressValue(0)

    // Debounced so a fast regeneration doesn't flash the bar. Staleness here
    // is only about *this* effect's own axes/mode changes, so a locally-scoped
    // worker (cancelled via terminate() in cleanup) is the right guard —
    // renderIdRef is shared with unrelated effects (e.g. the main render
    // effect bumps it on every text change too) and would drop this effect's
    // still-valid, still-in-flight response.
    const timer = setTimeout(() => {
      if (loadingRef.current) setShowProgress(true)
    }, 400)

    worker.onmessage = (e) => {
      const { result, error, type, value } = e.data
      if (type === 'progress') {
        setProgressValue(value)
        if (value > 0) setShowProgress(true)
        return
      }
      clearTimeout(timer)
      worker.terminate()
      if (error) setCompareError(error)
      else { setDactylGlyphData(result); setCompareError(null) }
      setLoading(false)
      loadingRef.current = false
      setShowProgress(false)
    }
    worker.postMessage({ id: ++renderIdRef.current, type: 'fontData', args: [axes, ...perGlyphFontArgs] })
    return () => {
      clearTimeout(timer)
      worker.terminate()
    }
  }, [axes, activeTab, compareMode, perGlyphFontAxes])

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
      return (
        <div
          className="proof-text"
          style={{
            fontFamily: proofFontUrl ? "'DactylPreview', monospace" : 'monospace',
            fontSize: `${18 * zoom}pt`,
            lineHeight: 1.4,
            whiteSpace: 'pre-wrap',
            textAlign: 'left',
            padding: '20px',
            color: '#000',
          }}
        >
          {text}
        </div>
      )
    }

    // Visual Diffs has its own renderer (axis worker SVG or compare-font mode).
    if (activeTab === 'visualDiffs') return renderVisualDiffs()

    // Generate tab, Bubble mode GPU path: render the field via the WebGL
    // canvas (sliders are shader uniforms).  Falls through to the worker SVG
    // result without WebGL2, and Grow mode always uses the worker SVG result.
    if (activeTab === 'generate' && generateMode === 'bubble' && supportsWebGL2) {
      if (!growField) return null
      return <GrowCanvas field={growField} params={growParams} zoom={zoom} />
    }

    // Safety check: ensure result matches expected type for tab
    const content = workerResult
    if (!content) return null

    try {
      if (activeTab === 'font' || activeTab === 'generate') {
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
              const EXCLUDED_TWEEN_AXES = ['tracking', 'leading']
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

  // Render the current Bubble view to a vector SVG string via a one-off worker.
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
      const base = filenameBase('bubble', text)
      if (format === 'svg') {
        downloadBlob(svgBlob(svg), `${base}.svg`)
      } else {
        // Transparent background: the layered look drops onto any surface.
        const png = await svgToPngBlob(svg, { scale: 3, background: null })
        downloadBlob(png, `${base}.png`)
      }
    } catch (e) {
      setError(`Bubble ${format.toUpperCase()} export failed: ${e.message}`)
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
      setError(`Bubble copy failed: ${e.message}`)
    } finally {
      setSavingGrow(false)
    }
  }

  // Render the current Grow (branch-mode) view to a vector SVG string via a
  // one-off worker, for both SVG and PNG downloads — mirrors requestGrowthSvg.
  const requestBranchSvg = () => new Promise((resolve, reject) => {
    if (!text) { resolve(''); return }
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    worker.onmessage = (e) => {
      worker.terminate()
      if (e.data.error) reject(new Error(e.data.error))
      else resolve(e.data.result)
    }
    worker.onerror = (err) => { worker.terminate(); reject(err) }
    worker.postMessage({ id: 0, type: 'branch', args: [text, axes, branchParams] })
  })

  const handleDownloadBranch = async (format) => {
    setBranchMenuOpen(false)
    setSavingBranch(true)
    setError(null)
    try {
      const svg = await requestBranchSvg()
      if (!svg) return
      const base = filenameBase('grow', text)
      if (format === 'svg') {
        downloadBlob(svgBlob(svg), `${base}.svg`)
      } else {
        const png = await svgToPngBlob(svg, { scale: 3, background: null })
        downloadBlob(png, `${base}.png`)
      }
    } catch (e) {
      setError(`Grow ${format.toUpperCase()} export failed: ${e.message}`)
    } finally {
      setSavingBranch(false)
    }
  }

  const handleCopyBranch = async () => {
    if (!navigator.clipboard || typeof ClipboardItem === 'undefined') {
      setError('Clipboard image copy is not supported in this browser')
      return
    }
    setSavingBranch(true)
    setError(null)
    try {
      const svg = await requestBranchSvg()
      if (!svg) throw new Error('nothing to copy')
      const png = await svgToPngBlob(svg, { scale: 3, background: null })
      await navigator.clipboard.write([new ClipboardItem({ 'image/png': png })])
      setBranchCopied(true)
      setTimeout(() => setBranchCopied(false), 1500)
    } catch (e) {
      setError(`Grow copy failed: ${e.message}`)
    } finally {
      setSavingBranch(false)
    }
  }

  const handleControlChange = (name, value) => {
    setAxes(prev => ({ ...prev, [name]: value }))
  }

  const handleReset = () => {
    setAxes({ ...defaultAxes })
    setGlyphSeed(null)
  }

  // Reset the active Generate mode's own settings to their defaults, leaving
  // the mode selection (and the other mode's settings) untouched.
  const handleResetGenerateParams = () => {
    if (generateMode === 'bubble') setGrowParams(defaultGrowParams())
    else setBranchParams(defaultBranchParams())
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

  // Randomize around the *defaults* so repeated clicks don't compound.
  const handleRandom = () => {
    setAxes(randomizeAxes(axes, defaultAxes, controlDefinitions, Math.random))
  }

  // "Randomise every glyph": roll a new seed. Everything downstream (font tab
  // render, proofs, OTF export, font comparison) derives its per-glyph axes from
  // it, so the variant font is stable until this is clicked again or Reset.
  const handleRandomEveryGlyph = () => {
    setGlyphSeed(newGlyphSeed())
  }


  return (
    <div className="container">
      <div className="sidebar">
        <div className="sidebar-title" dangerouslySetInnerHTML={{ __html: generateTweenSvg("Dactyl", { ...defaultAxes, thickness: 35 }) }} />
        <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '10px', flex: '0 0 auto' }}>
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
            <button className={`tab-button ${activeTab === 'generate' ? 'active' : ''}`} onClick={() => setTabWithUrl('generate')}>Generate</button>
          </div>
          {activeTab === 'font' && (
            <div className="toolbar">
              <button
                className={`icon-button ${glyphSeed !== null ? 'active' : ''}`}
                onClick={handleRandomEveryGlyph}
                title={glyphSeed === null
                  ? 'Randomise every glyph — give each character its own settings'
                  : 'Randomise every glyph again (new set)'}
              >
                <span className="material-symbols-outlined">shuffle</span>
              </button>
              {glyphSeed !== null && (
                <button
                  className="icon-button"
                  onClick={() => setGlyphSeed(null)}
                  title="Turn off per-glyph randomisation"
                >
                  <span className="material-symbols-outlined">close</span>
                </button>
              )}
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
            </div>
          )}
        </div>

        <div className={`input-area ${['glyphs', 'generate', 'visualDiffs'].includes(activeTab) ? 'with-side-panel' : ''}`} style={activeTab === 'splines' || activeTab === 'splineGrid' ? { display: 'none' } : undefined}>
          <div className="input-wrapper">
            <textarea
              value={text}
              onChange={e => setText(e.target.value)}
              rows={3}
              placeholder="Characters..."
              style={activeTab === 'proofs' ? { paddingTop: '54px' } : undefined}
            />
            {activeTab === 'proofs' && (
              <div className="proof-chips-bar">
                <div className="proof-chips" style={{ marginLeft: 0 }}>
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
                </div>
              </div>
            )}
            <button
              className="text-reset-button"
              onClick={() => {
                const defaults = { font: alphabetChars, glyphs: 'font', tweens: 'a', visualDiffs: allChars, splines: '', splineGrid: '', proofs: proofTexts[proofCase], generate: 'dactyl' }
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
          {activeTab === 'generate' && (
            <div className="controls-panel">
              <div className="grow-controls" style={{ display: 'flex', alignItems: 'center', gap: '12px', flexWrap: 'wrap' }}>
                <div className="proof-chips" style={{ marginLeft: 0 }}>
                  <button
                    className={`proof-chip ${generateMode === 'bubble' ? 'selected' : ''}`}
                    onClick={() => setGenerateModeWithUrl('bubble')}
                  >
                    Bubble
                  </button>
                  <button
                    className={`proof-chip ${generateMode === 'grow' ? 'selected' : ''}`}
                    onClick={() => setGenerateModeWithUrl('grow')}
                  >
                    Grow
                  </button>
                </div>
                <button
                  className="icon-button"
                  onClick={handleResetGenerateParams}
                  title="Reset settings to default"
                >
                  <span className="material-symbols-outlined" style={{ fontSize: '20px' }}>restart_alt</span>
                </button>
                <label
                  style={{ display: 'flex', alignItems: 'center', gap: '6px' }}
                  title="Render just the first character at lower resolution, for responsive slider dragging (Grow mode especially is slow at full text)"
                >
                  fast preview
                  <input
                    type="checkbox"
                    checked={fastPreview}
                    onChange={e => setFastPreview(e.target.checked)}
                  />
                </label>
                <div className="controls-break" />
                {generateMode === 'bubble' && (<>
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
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }} title="Melt neighbouring letters into a logotype">
                    fuse
                    <input
                      type="range" min="0" max="1" step="0.05"
                      value={growParams.fuse}
                      onChange={e => setGrowParams(p => ({ ...p, fuse: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2.5em' }}>{growParams.fuse.toFixed(2)}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }} title="Wobble the grown edges with value noise">
                    warp
                    <input
                      type="range" min="0" max="1" step="0.05"
                      value={growParams.warp}
                      onChange={e => setGrowParams(p => ({ ...p, warp: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2.5em' }}>{growParams.warp.toFixed(2)}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    scale
                    <input
                      type="range" min="40" max="220" step="10"
                      value={growParams.growScale}
                      onChange={e => setGrowParams(p => ({ ...p, growScale: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2.5em' }}>{growParams.growScale}</span>
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
                  <div className="controls-break" />
                  {!growParams.layers && (
                    <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                      colour
                      <input
                        type="color"
                        value={growParams.color}
                        onChange={e => setGrowParams(p => ({ ...p, color: e.target.value }))}
                      />
                    </label>
                  )}
                  {growParams.layers && ['core', 'band 2', 'band 3', 'outline'].map((label, i) => (
                    <label key={label} style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                      {label}
                      <input
                        type="color"
                        value={growParams.layerColors[i]}
                        onChange={e => setGrowParams(p => {
                          const layerColors = [...p.layerColors]
                          layerColors[i] = e.target.value
                          return { ...p, layerColors }
                        })}
                      />
                    </label>
                  ))}
                  <div className="controls-break" />
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
                </>)}
                {generateMode === 'grow' && (<>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    density
                    <input
                      type="range" min="10" max="60" step="2"
                      value={branchParams.density}
                      onChange={e => setBranchParams(p => ({ ...p, density: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2em' }}>{branchParams.density}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    influence
                    <input
                      type="range" min="20" max="150" step="5"
                      value={branchParams.influence}
                      onChange={e => setBranchParams(p => ({ ...p, influence: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2.5em' }}>{branchParams.influence}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    kill dist
                    <input
                      type="range" min="4" max="40" step="2"
                      value={branchParams.killDistance}
                      onChange={e => setBranchParams(p => ({ ...p, killDistance: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2em' }}>{branchParams.killDistance}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    step
                    <input
                      type="range" min="3" max="20" step="1"
                      value={branchParams.stepSize}
                      onChange={e => setBranchParams(p => ({ ...p, stepSize: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2em' }}>{branchParams.stepSize}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    iterations
                    <input
                      type="range" min="0" max="150" step="5"
                      value={branchParams.iterations}
                      onChange={e => setBranchParams(p => ({ ...p, iterations: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2.5em' }}>{branchParams.iterations}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    seed
                    <input
                      type="range" min="1" max="50" step="1"
                      value={branchParams.seed}
                      onChange={e => setBranchParams(p => ({ ...p, seed: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2em' }}>{branchParams.seed}</span>
                  </label>
                  <div className="controls-break" />
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    reach
                    <input
                      type="range" min="40" max="250" step="10"
                      value={branchParams.maxReach}
                      onChange={e => setBranchParams(p => ({ ...p, maxReach: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2.5em' }}>{branchParams.maxReach}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    base radius
                    <input
                      type="range" min="2" max="30" step="1"
                      value={branchParams.baseRadius}
                      onChange={e => setBranchParams(p => ({ ...p, baseRadius: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2em' }}>{branchParams.baseRadius}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    min radius
                    <input
                      type="range" min="0.5" max="6" step="0.5"
                      value={branchParams.minRadius}
                      onChange={e => setBranchParams(p => ({ ...p, minRadius: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2em' }}>{branchParams.minRadius}</span>
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    taper depth
                    <input
                      type="range" min="4" max="40" step="2"
                      value={branchParams.maxDepthForTaper}
                      onChange={e => setBranchParams(p => ({ ...p, maxDepthForTaper: parseFloat(e.target.value) }))}
                    />
                    <span style={{ minWidth: '2em' }}>{branchParams.maxDepthForTaper}</span>
                  </label>
                  <div className="controls-break" />
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    backbone
                    <input
                      type="checkbox"
                      checked={branchParams.backbone}
                      onChange={e => setBranchParams(p => ({ ...p, backbone: e.target.checked }))}
                    />
                  </label>
                  <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                    colour
                    <input
                      type="color"
                      value={branchParams.color}
                      onChange={e => setBranchParams(p => ({ ...p, color: e.target.value }))}
                    />
                  </label>
                  {branchParams.backbone && (
                    <label style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
                      backbone colour
                      <input
                        type="color"
                        value={branchParams.backboneColor}
                        onChange={e => setBranchParams(p => ({ ...p, backboneColor: e.target.value }))}
                      />
                    </label>
                  )}
                  <div className="controls-break" />
                  <span style={{ display: 'flex', alignItems: 'center', gap: '4px', marginLeft: '4px' }}>
                    <button
                      className="icon-button"
                      onClick={handleCopyBranch}
                      disabled={savingBranch || !text}
                      title="Copy PNG to clipboard"
                    >
                      <span className="material-symbols-outlined" style={{ fontSize: '20px' }}>
                        {branchCopied ? 'check' : 'content_copy'}
                      </span>
                    </button>
                    {/* Download defaults to PNG; the caret opens a PNG/SVG menu. */}
                    <span ref={branchMenuRef} className="grow-download-split" style={{ display: 'flex', alignItems: 'center', gap: '4px', position: 'relative' }}>
                      <button
                        className="icon-button"
                        onClick={() => handleDownloadBranch('png')}
                        disabled={savingBranch || !text}
                        title="Download PNG (transparent, high-res)"
                      >
                        <span className="material-symbols-outlined" style={{ fontSize: '20px' }}>
                          {savingBranch ? 'hourglass_empty' : 'download'}
                        </span>
                      </button>
                      <button
                        className="icon-button"
                        onClick={() => setBranchMenuOpen(o => !o)}
                        disabled={savingBranch || !text}
                        title="Choose download format"
                        aria-haspopup="menu"
                        aria-expanded={branchMenuOpen}
                        style={{ width: '24px', minWidth: '24px', padding: '6px 0' }}
                      >
                        <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>arrow_drop_down</span>
                      </button>
                      {branchMenuOpen && (
                        <div
                          role="menu"
                          style={{
                            position: 'absolute', top: '100%', right: 0, marginTop: '4px', zIndex: 20,
                            background: 'var(--panel-bg)', border: '1px solid var(--border-color)',
                            borderRadius: 'var(--radius-md)', boxShadow: '0 4px 12px rgba(0,0,0,0.4)', overflow: 'hidden', minWidth: '160px',
                          }}
                        >
                          <button className="grow-menu-item" role="menuitem" onClick={() => handleDownloadBranch('png')}>
                            <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>image</span>
                            PNG <span style={{ opacity: 0.55, marginLeft: 'auto', fontSize: '0.8em' }}>transparent</span>
                          </button>
                          <button className="grow-menu-item" role="menuitem" onClick={() => handleDownloadBranch('svg')}>
                            <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>polyline</span>
                            SVG <span style={{ opacity: 0.55, marginLeft: 'auto', fontSize: '0.8em' }}>vector</span>
                          </button>
                        </div>
                      )}
                    </span>
                  </span>
                </>)}
              </div>
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
                <div className="controls-break" />
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
              <div className="controls-panel">
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
              </div>
            )
          })()}
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
            {/* Image export for the typed string, alongside the zoom buttons.
                Font tab only for now — the other tabs render debug overlays,
                grids or their own canvases that don't export meaningfully. */}
            {activeTab === 'font' && (
              <>
                <button
                  onClick={handleCopyFontImage}
                  disabled={savingFontImage || !text}
                  title="Copy PNG to clipboard"
                >
                  <span className="material-symbols-outlined">
                    {fontCopied ? 'check' : 'content_copy'}
                  </span>
                </button>
                {/* Download defaults to PNG; the caret opens a PNG/SVG menu. */}
                <span ref={fontMenuRef} style={{ display: 'flex', alignItems: 'center', position: 'relative' }}>
                  <button
                    onClick={() => handleDownloadFontImage('png')}
                    disabled={savingFontImage || !text}
                    title="Download PNG (transparent, high-res)"
                  >
                    <span className="material-symbols-outlined">
                      {savingFontImage ? 'hourglass_empty' : 'download'}
                    </span>
                  </button>
                  {/* overflow:hidden keeps the caret glyph inside its button, so it
                      can't sit on top of the download button and swallow its clicks */}
                  <button
                    onClick={() => setFontMenuOpen(o => !o)}
                    disabled={savingFontImage || !text}
                    title="Choose download format"
                    aria-haspopup="menu"
                    aria-expanded={fontMenuOpen}
                    style={{ width: '20px', minWidth: '20px', padding: '6px 0', overflow: 'hidden' }}
                  >
                    <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>arrow_drop_down</span>
                  </button>
                  {fontMenuOpen && (
                    <div
                      role="menu"
                      style={{
                        position: 'absolute', top: '100%', right: 0, marginTop: '8px', zIndex: 20,
                        background: 'var(--panel-bg)', border: '1px solid var(--border-color)',
                        borderRadius: 'var(--radius-md)', boxShadow: '0 4px 12px rgba(0,0,0,0.4)', overflow: 'hidden', minWidth: '160px',
                      }}
                    >
                      <button className="grow-menu-item" role="menuitem" onClick={() => handleDownloadFontImage('png')}>
                        <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>image</span>
                        PNG <span style={{ opacity: 0.55, marginLeft: 'auto', fontSize: '0.8em' }}>transparent</span>
                      </button>
                      <button className="grow-menu-item" role="menuitem" onClick={() => handleDownloadFontImage('svg')}>
                        <span className="material-symbols-outlined" style={{ fontSize: '18px' }}>polyline</span>
                        SVG <span style={{ opacity: 0.55, marginLeft: 'auto', fontSize: '0.8em' }}>vector</span>
                      </button>
                    </div>
                  )}
                </span>
                <span style={{ width: '1px', background: 'rgba(255,255,255,0.2)', margin: '2px 2px' }} />
              </>
            )}
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
            <div style={activeTab === 'splines' ? { display: 'contents' } : { transform: (activeTab === 'tweens' || activeTab === 'proofs' || (activeTab === 'generate' && generateMode === 'bubble' && supportsWebGL2)) ? 'none' : `scale(${zoom})`, transformOrigin: 'top left', minHeight: '100%' }}>
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
