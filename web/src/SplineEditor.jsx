import { useState, useEffect, useRef, useCallback, useMemo } from 'react'
import './SplineEditor.css'

const POINT_TYPES = [
  { value: 0, label: 'Corner' },
  { value: 1, label: 'Smooth' },
  { value: 2, label: 'Line-Curve' },
  { value: 3, label: 'Curve-Line' },
]

const HANDLE_LEN = 150 // length of tangent handles in SVG coords

const CARDINAL_PRESETS = [
  { label: '→', value: 0 },
  { label: '↑', value: Math.PI / 2 },
  { label: '←', value: Math.PI },
  { label: '↓', value: -Math.PI / 2 },
  { label: '↗', value: Math.PI / 4 },
  { label: '↖', value: 3 * Math.PI / 4 },
  { label: '↙', value: -3 * Math.PI / 4 },
  { label: '↘', value: -Math.PI / 4 },
]

// Angles are stored in radians internally but displayed in degrees
const toDeg = r => Math.round(r * 180 / Math.PI * 10) / 10
const toRad = d => Math.round(d * Math.PI / 180 * 1000) / 1000

const deepClone = obj => JSON.parse(JSON.stringify(obj))

// Returns sensible default x/y coordinates based on guide midpoints
const getDefaultCoords = (guides) => {
  if (!guides) return { cx: 400, cy: 500 }
  const midIdx = Math.floor(guides.yGuides.length / 2)
  return {
    cx: Math.round((guides.xGuides[0].value + guides.xGuides[guides.xGuides.length - 1].value) / 2),
    cy: Math.round(guides.yGuides[midIdx].value),
  }
}

const isEditingInput = (e) =>
  e.target.tagName === 'INPUT' || e.target.tagName === 'SELECT' || e.target.tagName === 'TEXTAREA'

// Snap a value to the nearest guide if within threshold units
const snapToGuide = (value, guideArray, thresholdUnits) => {
  if (!guideArray) return value
  for (const g of guideArray) {
    if (Math.abs(value - g.value) <= thresholdUnits) return g.value
  }
  return value
}

// --- Curvature graph helpers ---

// Evaluate a cubic bezier at t: returns {x, y}
// SVG curvature graph component — data comes pre-computed from F# via solveResult.curvatureData
function CurvatureGraph({ curvatureData, width = 400, height = 100 }) {
  const { samples, knotArcs } = curvatureData || { samples: [], knotArcs: [] }

  if (!samples.length) return null

  const totalArc = samples[samples.length - 1]?.arcLen || 1
  const maxAbs = Math.max(1e-6, ...samples.map(s => Math.abs(s.curvature)))

  const pad = { t: 6, b: 6, l: 4, r: 4 }
  const gw = width - pad.l - pad.r
  const gh = height - pad.t - pad.b
  const midY = pad.t + gh / 2

  const toX = (arc) => pad.l + (arc / totalArc) * gw
  const toY = (k) => midY - (k / maxAbs) * (gh / 2)

  const pts = samples.map(s => `${toX(s.arcLen).toFixed(1)},${toY(s.curvature).toFixed(1)}`).join(' ')

  return (
    <svg className="se-curvature-graph" width={width} height={height}>
      {/* Zero baseline */}
      <line x1={pad.l} y1={midY} x2={pad.l + gw} y2={midY} stroke="#444" strokeWidth="1" />
      {/* Curvature polyline */}
      <polyline points={pts} fill="none" stroke="#4af" strokeWidth="1.5" />
      {/* Knot markers */}
      {knotArcs.map((arc, i) => {
        const x = toX(arc)
        return <line key={i} x1={x} y1={pad.t} x2={x} y2={pad.t + gh} stroke="#f84" strokeWidth="1" strokeDasharray="2,2" />
      })}
    </svg>
  )
}

function SplineEditor({ axes }) {
  const [glyphList, setGlyphList] = useState([])
  const [selectedChar, setSelectedChar] = useState(
    () => localStorage.getItem('splineSelectedChar') || 'a'
  )
  const [curves, setCurves] = useState([]) // array of { isClosed, points[] }
  const [activeCurve, setActiveCurve] = useState(0)
  const [guides, setGuides] = useState(null)
  const [solveResult, setSolveResult] = useState(null)
  const [allCurvePaths, setAllCurvePaths] = useState({}) // curveIdx → pathSvg for inactive curves
  const [selectedPt, setSelectedPt] = useState(null) // index into active curve's points
  const [maxIter, setMaxIter] = useState(1000)
  const [showComb, setShowComb] = useState(false)
  const [showTangents, setShowTangents] = useState(true)
  const [showOutline, setShowOutline] = useState(false)
  const [showCurvatureGraph, setShowCurvatureGraph] = useState(false)
  const [dragRowIdx, setDragRowIdx] = useState(null)    // index of row being dragged to reorder
  const [dragOverRowIdx, setDragOverRowIdx] = useState(null) // row being hovered over during drag
  const svgRef = useRef(null)
  const workerRef = useRef(null)
  const solveIdRef = useRef(0)
  const dragRef = useRef(null) // { type: 'knot'|'th_in'|'th_out', curveIdx, idx }

  // Always-current snapshot of curves without triggering stale closures in callbacks
  const curvesRef = useRef(curves)
  useEffect(() => { curvesRef.current = curves }, [curves])

  // Refs for guides and viewBox — accessed in pointer callbacks without stale closure risk
  const guidesRef = useRef(guides)
  useEffect(() => { guidesRef.current = guides }, [guides])
  const viewBoxRef = useRef(null) // set after viewBox useMemo

  // Undo/redo stacks kept in refs so mutations don't cause re-renders
  const undoStackRef = useRef([])
  const redoStackRef = useRef([])

  const pushUndo = useCallback(() => {
    undoStackRef.current.push(deepClone(curvesRef.current))
    if (undoStackRef.current.length > 50) undoStackRef.current.shift()
    redoStackRef.current = []
  }, [])

  const undo = useCallback(() => {
    if (!undoStackRef.current.length) return
    redoStackRef.current.push(deepClone(curvesRef.current))
    setCurves(undoStackRef.current.pop())
  }, [])

  const redo = useCallback(() => {
    if (!redoStackRef.current.length) return
    undoStackRef.current.push(deepClone(curvesRef.current))
    setCurves(redoStackRef.current.pop())
  }, [])

  // Create a persistent worker
  useEffect(() => {
    workerRef.current = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    workerRef.current.postMessage({ id: -1, type: 'getGlyphList', args: [] })
    return () => workerRef.current?.terminate()
  }, [])

  // Load guides when axes change
  useEffect(() => {
    if (!workerRef.current) return
    const w = workerRef.current
    const handler = (e) => {
      if (e.data.id === -2 && e.data.result) setGuides(e.data.result)
    }
    w.addEventListener('message', handler)
    w.postMessage({ id: -2, type: 'getGuides', args: [axes] })
    return () => w.removeEventListener('message', handler)
  }, [axes])

  // Handle worker messages
  useEffect(() => {
    if (!workerRef.current) return
    const w = workerRef.current
    const handler = (e) => {
      const { id, result, error } = e.data
      if (id === -1 && result) setGlyphList(result)
      if (id === -3 && result) {
        if (result.length > 0) {
          setCurves(result)
          setAllCurvePaths({})
          undoStackRef.current = []
          redoStackRef.current = []
          setActiveCurve(0)
          setSelectedPt(null)
        }
      }
if (id > 0 && id === solveIdRef.current && result) setSolveResult(result)
      if (error && id > 0) console.error('Spline solve error:', error)
    }
    w.addEventListener('message', handler)
    return () => w.removeEventListener('message', handler)
  }, [])

  // Parse glyph when char or axes change; persist selection across tab switches
  useEffect(() => {
    if (!workerRef.current || !selectedChar) return
    localStorage.setItem('splineSelectedChar', selectedChar)
    workerRef.current.postMessage({ id: -3, type: 'parseGlyph', args: [selectedChar, axes] })
  }, [selectedChar, axes])

// Cache solved path per curve index (for inactive curve rendering)
  useEffect(() => {
    if (solveResult?.pathSvg) {
      setAllCurvePaths(prev => ({ ...prev, [activeCurve]: solveResult.pathSvg }))
    }
  }, [solveResult, activeCurve])

  // Solve spline whenever control points change (debounced to avoid queueing solves during drag)
  useEffect(() => {
    if (!workerRef.current || !curves[activeCurve]) return
    const curve = curves[activeCurve]
    if (curve.points.length < 2) { setSolveResult(null); return }
    const timer = setTimeout(() => {
      const id = ++solveIdRef.current
      const ctrlPts = curve.points.map(p => ({
        ty: p.ty,
        x: p.x,
        y: p.y,
        th_in: p.th_in ?? undefined,
        th_out: p.th_out ?? undefined,
      }))
      workerRef.current.postMessage({ id, type: 'solveSpline', args: [ctrlPts, curve.isClosed, maxIter] })
    }, 80)
    return () => clearTimeout(timer)
  }, [curves, activeCurve, maxIter])

  // SVG coordinate space: compute viewBox from guides
  const viewBox = useMemo(() => {
    if (!guides) return { x: -50, y: -50, w: 800, h: 1100 }
    const xs = guides.xGuides.map(g => g.value)
    const ys = guides.yGuides.map(g => g.value)
    const minX = Math.min(...xs) - 80
    const maxX = Math.max(...xs) + 80
    const minY = Math.min(...ys) - 80
    const maxY = Math.max(...ys) + 80
    const vb = { x: minX, y: -(maxY), w: maxX - minX, h: maxY - minY }
    viewBoxRef.current = vb
    return vb
  }, [guides])

  const svgPoint = useCallback((clientX, clientY) => {
    const svg = svgRef.current
    if (!svg) return { x: 0, y: 0 }
    const pt = svg.createSVGPoint()
    pt.x = clientX
    pt.y = clientY
    const ctm = svg.getScreenCTM().inverse()
    const svgPt = pt.matrixTransform(ctm)
    return { x: svgPt.x, y: -svgPt.y } // flip Y back to math coords
  }, [])

  // updatePoint does NOT push undo — callers must call pushUndo first
  const updatePoint = useCallback((curveIdx, ptIdx, updates) => {
    setCurves(prev => {
      const next = [...prev]
      const curve = { ...next[curveIdx], points: [...next[curveIdx].points] }
      curve.points[ptIdx] = { ...curve.points[ptIdx], ...updates }
      next[curveIdx] = curve
      return next
    })
  }, [])

  // Drag handlers — undo pushed once at drag start, not on every move
  const handlePointerDown = useCallback((e, type, curveIdx, idx) => {
    e.preventDefault()
    e.stopPropagation()
    pushUndo()
    dragRef.current = { type, curveIdx, idx }
    setActiveCurve(curveIdx)
    setSelectedPt(idx)
    svgRef.current?.setPointerCapture(e.pointerId)
  }, [pushUndo])

  // Dragging an auto handle: lock the angle to the current solved value, then allow drag
  const handleAutoHandlePointerDown = useCallback((e, type, curveIdx, idx, autoAngle) => {
    e.preventDefault()
    e.stopPropagation()
    pushUndo()
    const locked = Math.round(autoAngle * 100) / 100
    if (type === 'th_out' && curvesRef.current[curveIdx]?.points[idx]?.ty === 1) {
      updatePoint(curveIdx, idx, { th_in: locked, th_out: locked })
    } else {
      updatePoint(curveIdx, idx, { [type]: locked })
    }
    dragRef.current = { type, curveIdx, idx }
    setActiveCurve(curveIdx)
    setSelectedPt(idx)
    svgRef.current?.setPointerCapture(e.pointerId)
  }, [pushUndo, updatePoint])

  const handlePointerMove = useCallback((e) => {
    if (!dragRef.current) return
    const { type, curveIdx, idx } = dragRef.current
    const { x, y } = svgPoint(e.clientX, e.clientY)
    const curve = curvesRef.current[curveIdx]
    if (!curve) return
    const pt = curve.points[idx]

    if (type === 'knot') {
      // Snap to guide lines: compute threshold in viewBox units from SVG pixel size
      let sx = x, sy = y
      const svg = svgRef.current
      const vb = viewBoxRef.current
      const g = guidesRef.current
      if (svg && vb && g) {
        const rect = svg.getBoundingClientRect()
        const threshold = 12 * vb.w / rect.width // 12px in viewBox units
        sx = snapToGuide(x, g.xGuides, threshold)
        sy = snapToGuide(y, g.yGuides, threshold)
      }
      updatePoint(curveIdx, idx, { x: Math.round(sx), y: Math.round(sy) })
    } else if (type === 'th_in' || type === 'th_out') {
      const px = pt.x ?? 0
      const py = pt.y ?? 0
      const angle = Math.atan2(y - py, x - px)
      // th_in handle is rendered at (th_in + π) from the knot, so dragging the handle
      // to angle α means th_in = α + π; th_out handle is at th_out, so th_out = α directly.
      let value = type === 'th_in' ? angle + Math.PI : angle
      // Normalise to (-π, π]
      if (value > Math.PI) value -= 2 * Math.PI
      if (value <= -Math.PI) value += 2 * Math.PI
      updatePoint(curveIdx, idx, { [type]: Math.round(value * 100) / 100 })
    }
  }, [svgPoint, updatePoint])

  const handlePointerUp = useCallback(() => {
    dragRef.current = null
  }, [])

  const handleSvgClick = useCallback((e) => {
    if (e.target === svgRef.current || e.target.tagName === 'svg') setSelectedPt(null)
  }, [])

  const handleDblClick = useCallback((e) => {
    if (e.target !== svgRef.current && e.target.tagName !== 'svg') return
    e.preventDefault() // prevent text selection on double-click
    const { x, y } = svgPoint(e.clientX, e.clientY)
    const newIdx = curvesRef.current[activeCurve]?.points.length ?? 0
    pushUndo()
    setCurves(prev => {
      if (!prev[activeCurve]) return prev
      const next = [...prev]
      const curve = { ...next[activeCurve], points: [...next[activeCurve].points] }
      curve.points.push({ ty: 1, x: Math.round(x), y: Math.round(y), th_in: null, th_out: null, label: '' })
      next[activeCurve] = curve
      return next
    })
    setSelectedPt(newIdx)
  }, [pushUndo, svgPoint, activeCurve])

  // Table editing — angle inputs receive degrees, stored as radians
  const handleTableChange = useCallback((idx, field, value) => {
    pushUndo()
    const updates = {}
    if (field === 'ty') {
      updates.ty = parseInt(value)
    } else if (field === 'x' || field === 'y') {
      updates[field] = value === '' ? null : Math.round(parseFloat(value))
    } else if (field === 'th_in' || field === 'th_out') {
      updates[field] = value === '' ? null : toRad(parseFloat(value))
      // For smooth points keep both angles in sync
      if (field === 'th_out' && curves[activeCurve]?.points[idx]?.ty === 1) {
        updates.th_in = updates.th_out
      }
    }
    updatePoint(activeCurve, idx, updates)
  }, [activeCurve, pushUndo, updatePoint])

  // Label edits skip undo — pushing an entry per keystroke would flood the stack
  const handleLabelChange = useCallback((idx, value) => {
    updatePoint(activeCurve, idx, { label: value })
  }, [activeCurve, updatePoint])

  const handleToggleAuto = useCallback((idx, field) => {
    const pt = curves[activeCurve]?.points[idx]
    if (!pt) return
    pushUndo()
    if (field === 'x') {
      if (pt.x === null) {
        updatePoint(activeCurve, idx, { x: getDefaultCoords(guides).cx })
      } else if (pt.y !== null) {
        updatePoint(activeCurve, idx, { x: null })
      }
    } else if (field === 'y') {
      if (pt.y === null) {
        updatePoint(activeCurve, idx, { y: getDefaultCoords(guides).cy })
      } else if (pt.x !== null) {
        updatePoint(activeCurve, idx, { y: null })
      }
    } else if (field === 'th_in') {
      updatePoint(activeCurve, idx, { th_in: pt.th_in === null ? 0 : null })
    } else if (field === 'th_out') {
      // For smooth points, toggle both angles together
      if (pt.ty === 1) {
        const v = pt.th_out === null ? 0 : null
        updatePoint(activeCurve, idx, { th_in: v, th_out: v })
      } else {
        updatePoint(activeCurve, idx, { th_out: pt.th_out === null ? 0 : null })
      }
    }
  }, [activeCurve, curves, guides, pushUndo, updatePoint])

  const addPoint = useCallback(() => {
    pushUndo()
    const { cx, cy } = getDefaultCoords(guides)
    setCurves(prev => {
      const next = [...prev]
      const curve = { ...next[activeCurve], points: [...next[activeCurve].points] }
      curve.points.push({ ty: 1, x: cx, y: cy, th_in: null, th_out: null, label: '' })
      next[activeCurve] = curve
      return next
    })
  }, [activeCurve, guides, pushUndo])

  const deletePoint = useCallback((idx) => {
    pushUndo()
    setCurves(prev => {
      const next = [...prev]
      const curve = { ...next[activeCurve], points: [...next[activeCurve].points] }
      curve.points.splice(idx, 1)
      next[activeCurve] = curve
      return next
    })
    if (selectedPt === idx) setSelectedPt(null)
    else if (selectedPt > idx) setSelectedPt(selectedPt - 1)
  }, [activeCurve, selectedPt, pushUndo])

  const movePoint = useCallback((fromIdx, toIdx) => {
    if (fromIdx === toIdx) return
    pushUndo()
    setCurves(prev => {
      const next = [...prev]
      const curve = { ...next[activeCurve], points: [...next[activeCurve].points] }
      const [removed] = curve.points.splice(fromIdx, 1)
      curve.points.splice(toIdx, 0, removed)
      next[activeCurve] = curve
      return next
    })
    setSelectedPt(toIdx)
  }, [activeCurve, pushUndo])

  const toggleClosed = useCallback(() => {
    pushUndo()
    setCurves(prev => {
      const next = [...prev]
      next[activeCurve] = { ...next[activeCurve], isClosed: !next[activeCurve].isClosed }
      return next
    })
  }, [activeCurve, pushUndo])

  // Keyboard shortcuts
  useEffect(() => {
    const handler = (e) => {
      if (isEditingInput(e)) return

      if (e.ctrlKey && e.key === 'z') {
        e.preventDefault()
        undo()
      } else if (e.ctrlKey && (e.key === 'y' || (e.shiftKey && e.key === 'Z'))) {
        e.preventDefault()
        redo()
      } else if (e.key === 'Escape') {
        setSelectedPt(null)
      } else if ((e.key === 'Delete' || e.key === 'Backspace') && selectedPt !== null) {
        e.preventDefault()
        deletePoint(selectedPt)
      } else if (selectedPt !== null && (e.key === 'ArrowLeft' || e.key === 'ArrowRight' || e.key === 'ArrowUp' || e.key === 'ArrowDown')) {
        e.preventDefault()
        const pt = curvesRef.current[activeCurve]?.points[selectedPt]
        if (!pt || pt.x === null || pt.y === null) return
        const step = e.shiftKey ? 10 : 1
        const dx = e.key === 'ArrowLeft' ? -step : e.key === 'ArrowRight' ? step : 0
        const dy = e.key === 'ArrowUp' ? step : e.key === 'ArrowDown' ? -step : 0
        // e.repeat is true while a key is held — only push one undo entry per press gesture
        if (!e.repeat) pushUndo()
        updatePoint(activeCurve, selectedPt, { x: pt.x + dx, y: pt.y + dy })
      }
    }
    document.addEventListener('keydown', handler)
    return () => document.removeEventListener('keydown', handler)
  }, [selectedPt, activeCurve, deletePoint, updatePoint, pushUndo, undo, redo])

  const currentCurve = curves[activeCurve]
  const points = currentCurve?.points || []

  // Render a knot marker (square=corner, circle=smooth) for any curve
  const renderKnot = (p, i, curveIdx, isActive) => {
    if (p.x == null || p.y == null) return null
    const isSelected = isActive && i === selectedPt
    const isCorner = p.ty === 0
    const size = isSelected ? 24 : 18
    const fill = isActive
      ? (isSelected ? '#ff4444' : (isCorner ? '#4488ff' : '#44cc44'))
      : '#555'
    const stroke = isActive ? '#fff' : '#888'
    return (
      <g key={`knot-${curveIdx}-${i}`}>
        {isCorner ? (
          <rect
            x={p.x - size / 2} y={-p.y - size / 2} width={size} height={size}
            fill={fill} stroke={stroke} strokeWidth="1.5"
            style={{ cursor: 'grab' }}
            onPointerDown={e => handlePointerDown(e, 'knot', curveIdx, i)}
          />
        ) : (
          <circle
            cx={p.x} cy={-p.y} r={size / 2}
            fill={fill} stroke={stroke} strokeWidth="1.5"
            style={{ cursor: 'grab' }}
            onPointerDown={e => handlePointerDown(e, 'knot', curveIdx, i)}
          />
        )}
        {isActive && (
          <text x={p.x} y={-p.y - size / 2 - 4} textAnchor="middle" fontSize="20" fill="#ccc">
            {p.label || i}
          </text>
        )}
      </g>
    )
  }

  // Angle table cell — shared between th_in and th_out columns
  const renderAngleCell = (field, value, idx) => {
    if (value === null) {
      return (
        <td className="se-auto-cell-td">
          <div className="se-auto-cell">
            <button className="se-auto-active" onClick={() => handleToggleAuto(idx, field)} title="Click to set value">auto</button>
          </div>
        </td>
      )
    }
    return (
      <td className="se-auto-cell-td">
        <div className="se-auto-cell">
          <input
            type="number" step="1"
            value={toDeg(value).toFixed(1)}
            onChange={e => handleTableChange(idx, field, e.target.value)}
          />
          <select
            className="se-preset-select"
            value=""
            onChange={e => {
              if (e.target.value !== '') {
                pushUndo()
                updatePoint(activeCurve, idx, { [field]: parseFloat(e.target.value) })
              }
              e.target.value = ''
            }}
            title="Cardinal preset"
          >
            <option value="">·</option>
            {CARDINAL_PRESETS.map(pr => <option key={pr.label} value={pr.value}>{pr.label}</option>)}
          </select>
          <button className="se-auto-btn" onClick={() => handleToggleAuto(idx, field)} title="Set to auto">↺</button>
        </div>
      </td>
    )
  }

  // Single theta cell for smooth knots (th_in and th_out are always equal)
  const renderSmoothAngleCell = (p, idx) => {
    const value = p.th_out  // th_in === th_out for smooth; use th_out as canonical
    if (value === null) {
      return (
        <td className="se-auto-cell-td" colSpan={2}>
          <div className="se-auto-cell">
            <button className="se-auto-active" onClick={() => handleToggleAuto(idx, 'th_out')} title="Click to set value">auto</button>
          </div>
        </td>
      )
    }
    return (
      <td className="se-auto-cell-td" colSpan={2}>
        <div className="se-auto-cell">
          <input
            type="number" step="1"
            value={toDeg(value).toFixed(1)}
            onChange={e => handleTableChange(idx, 'th_out', e.target.value)}
          />
          <select
            className="se-preset-select"
            value=""
            onChange={e => {
              if (e.target.value !== '') {
                pushUndo()
                const v = parseFloat(e.target.value)
                updatePoint(activeCurve, idx, { th_in: v, th_out: v })
              }
              e.target.value = ''
            }}
            title="Cardinal preset"
          >
            <option value="">·</option>
            {CARDINAL_PRESETS.map(pr => <option key={pr.label} value={pr.value}>{pr.label}</option>)}
          </select>
          <button className="se-auto-btn" onClick={() => handleToggleAuto(idx, 'th_out')} title="Set to auto">↺</button>
        </div>
      </td>
    )
  }

  return (
    <div className="spline-editor">
      <div className="se-toolbar">
        <div className="se-glyph-picker">
          <label>Glyph:</label>
          <select value={selectedChar} onChange={e => setSelectedChar(e.target.value)}>
            {glyphList.map(g => (
              <option key={g.char} value={g.char}>{g.char}</option>
            ))}
          </select>
        </div>
        {curves.length > 1 && (
          <div className="se-curve-picker">
            <label>Curve:</label>
            {curves.map((_, i) => (
              <button
                key={i}
                className={`se-curve-btn ${i === activeCurve ? 'active' : ''}`}
                onClick={() => { setActiveCurve(i); setSelectedPt(null) }}
              >
                {i + 1}
              </button>
            ))}
          </div>
        )}
        <div className="se-options">
          {currentCurve && (
            <label className="se-toggle">
              <input type="checkbox" checked={currentCurve.isClosed} onChange={toggleClosed} />
              Closed
            </label>
          )}
          <label className="se-toggle">
            <input type="checkbox" checked={showComb} onChange={e => setShowComb(e.target.checked)} />
            Comb
          </label>
          <label className="se-toggle">
            <input type="checkbox" checked={showTangents} onChange={e => setShowTangents(e.target.checked)} />
            Tangents
          </label>
          <label className="se-toggle">
            <input type="checkbox" checked={showOutline} onChange={e => setShowOutline(e.target.checked)} />
            Outline
          </label>
          <label className="se-toggle">
            <input type="checkbox" checked={showCurvatureGraph} onChange={e => setShowCurvatureGraph(e.target.checked)} />
            Curvature
          </label>
          <label className="se-iter">
            Iter:
            <input
              type="number" min={1} max={2000} value={maxIter}
              onChange={e => { const v = parseInt(e.target.value); if (v > 0) setMaxIter(v) }}
            />
          </label>
          <div className="se-undo-btns">
            <button className="se-btn-sm" onClick={undo} title="Undo (Ctrl+Z)"
              disabled={undoStackRef.current.length === 0}>↩</button>
            <button className="se-btn-sm" onClick={redo} title="Redo (Ctrl+Y)"
              disabled={redoStackRef.current.length === 0}>↪</button>
          </div>
        </div>
      </div>

      <div className="se-main">
        <div className="se-canvas-area">
          <svg
            ref={svgRef}
            className="se-canvas"
            viewBox={`${viewBox.x} ${viewBox.y} ${viewBox.w} ${viewBox.h}`}
            onPointerMove={handlePointerMove}
            onPointerUp={handlePointerUp}
            onClick={handleSvgClick}
            onDoubleClick={handleDblClick}
          >
            {/* Guide lines */}
            {guides && (
              <g className="se-guides" opacity="0.3">
                {guides.xGuides.map(g => (
                  <g key={`x-${g.name}`}>
                    <line x1={g.value} y1={-viewBox.y} x2={g.value} y2={-(viewBox.y + viewBox.h)} stroke="#888" strokeDasharray="4,4" />
                    <text x={g.value} y={-viewBox.y + 15} textAnchor="middle" fontSize="14" fill="#888">{g.name}</text>
                  </g>
                ))}
                {guides.yGuides.map(g => (
                  <g key={`y-${g.name}`}>
                    <line x1={viewBox.x} y1={-g.value} x2={viewBox.x + viewBox.w} y2={-g.value} stroke="#888" strokeDasharray="4,4" />
                    <text x={viewBox.x + 5} y={-g.value - 3} fontSize="14" fill="#888">{g.name}</text>
                  </g>
                ))}
              </g>
            )}

            {/* All path data uses Y-up math coords; scale(1,-1) flips to SVG space */}
            <g transform="scale(1,-1)">
              {/* Outline overlay: current curve path offset by -thickness in x and y */}
              {showOutline && solveResult?.pathSvg && (
                <g transform={`translate(${-axes.thickness}, ${-axes.thickness})`}>
                  <path d={solveResult.pathSvg} fill="none" stroke="rgba(100,180,255,0.6)" strokeWidth="2" strokeDasharray="6,3" />
                </g>
              )}

              {/* Inactive curve spline paths (greyed out) */}
              {curves.map((_, ci) => ci !== activeCurve && allCurvePaths[ci] && (
                <path key={`ghost-path-${ci}`} d={allCurvePaths[ci]}
                  fill="none" stroke="#555" strokeWidth="2" opacity="0.6" />
              ))}

              {/* Active curve solved spline */}
              {solveResult && (
                <>
                  <path d={solveResult.pathSvg} fill="none" stroke="#4488ff" strokeWidth="2" />
                  {showComb && <path d={solveResult.combSvg} fill="none" stroke="#888" strokeWidth="1" />}
                  {showTangents && <path d={solveResult.tangentSvg} fill="none" stroke="#e00000" strokeWidth="1" />}
                </>
              )}
            </g>

            {/* Inactive curve knot markers */}
            {curves.map((curve, ci) => ci !== activeCurve && (
              <g key={`ghost-${ci}`} opacity="0.5">
                {curve.points.map((p, i) => renderKnot(p, i, ci, false))}
              </g>
            ))}

            {/* Active curve: tangent handles (drawn under knots).
                Auto angles come from solveResult.bezierPoints; shown as dashed/unfilled. */}
            {points.map((p, i) => {
              if (p.x == null || p.y == null) return null
              const px = p.x
              const py = -p.y
              const bp = solveResult?.bezierPoints?.[i]
              const handles = []

              // th_in — explicit value or auto-solved angle
              const autoIn = p.th_in === null
              const thIn = autoIn ? (bp && !isNaN(bp.th_in) ? bp.th_in : null) : p.th_in
              if (thIn != null) {
                const hx = px + HANDLE_LEN * Math.cos(thIn + Math.PI)
                const hy = py - HANDLE_LEN * Math.sin(thIn + Math.PI)
                handles.push(
                  <g key={`h-in-${i}`}>
                    <line x1={px} y1={py} x2={hx} y2={hy}
                      stroke="#e07020" strokeWidth={autoIn ? 1 : 2}
                      strokeDasharray={autoIn ? '5,3' : undefined} opacity={autoIn ? 0.45 : 1} />
                    {autoIn
                      ? <circle cx={hx} cy={hy} r={10} fill="none" stroke="#e07020" strokeWidth="1.5" opacity="0.55"
                          style={{ cursor: 'grab' }}
                          onPointerDown={e => handleAutoHandlePointerDown(e, 'th_in', activeCurve, i, thIn)} />
                      : <circle cx={hx} cy={hy} r={14} fill="#e07020" stroke="#fff" strokeWidth="2"
                          style={{ cursor: 'grab' }}
                          onPointerDown={e => handlePointerDown(e, 'th_in', activeCurve, i)} />
                    }
                  </g>
                )
              }

              // th_out — explicit value or auto-solved angle
              const autoOut = p.th_out === null
              const thOut = autoOut ? (bp && !isNaN(bp.th_out) ? bp.th_out : null) : p.th_out
              if (thOut != null) {
                const hx = px + HANDLE_LEN * Math.cos(thOut)
                const hy = py - HANDLE_LEN * Math.sin(thOut)
                handles.push(
                  <g key={`h-out-${i}`}>
                    <line x1={px} y1={py} x2={hx} y2={hy}
                      stroke="#2070e0" strokeWidth={autoOut ? 1 : 2}
                      strokeDasharray={autoOut ? '5,3' : undefined} opacity={autoOut ? 0.45 : 1} />
                    {autoOut
                      ? <circle cx={hx} cy={hy} r={10} fill="none" stroke="#2070e0" strokeWidth="1.5" opacity="0.55"
                          style={{ cursor: 'grab' }}
                          onPointerDown={e => handleAutoHandlePointerDown(e, 'th_out', activeCurve, i, thOut)} />
                      : <circle cx={hx} cy={hy} r={14} fill="#2070e0" stroke="#fff" strokeWidth="2"
                          style={{ cursor: 'grab' }}
                          onPointerDown={e => handlePointerDown(e, 'th_out', activeCurve, i)} />
                    }
                  </g>
                )
              }

              return <g key={`handles-${i}`}>{handles}</g>
            })}

            {/* Active curve: knot points (on top) */}
            {points.map((p, i) => renderKnot(p, i, activeCurve, true))}
          </svg>

          {/* Curvature graph below the canvas */}
          {showCurvatureGraph && solveResult?.curvatureData && (
            <CurvatureGraph curvatureData={solveResult.curvatureData} />
          )}
        </div>

        {/* Table panel */}
        <div className="se-table-panel">
          <div className="se-table-header">
            <span>Control Points ({points.length})</span>
            <button onClick={addPoint} title="Add Point (or double-click canvas)" className="se-btn-sm">+ Add</button>
          </div>
          <div className="se-table-scroll">
            <table className="se-table">
              <thead>
                <tr>
                  <th className="se-drag-handle-th"></th>
                  <th>#</th>
                  <th>Label</th>
                  <th>Type</th>
                  <th>X</th>
                  <th>Y</th>
                  <th colSpan={2}>&theta; °</th>
                  <th></th>
                </tr>
              </thead>
              <tbody>
                {points.map((p, i) => (
                  <tr key={i}
                    className={[i === selectedPt ? 'selected' : '', i === dragOverRowIdx ? 'se-drag-over' : ''].filter(Boolean).join(' ')}
                    onClick={() => setSelectedPt(i)}
                    onDragOver={e => { e.preventDefault(); setDragOverRowIdx(i) }}
                    onDragLeave={() => setDragOverRowIdx(null)}
                    onDrop={() => {
                      if (dragRowIdx !== null) movePoint(dragRowIdx, i)
                      setDragRowIdx(null); setDragOverRowIdx(null)
                    }}
                    onDragEnd={() => { setDragRowIdx(null); setDragOverRowIdx(null) }}
                  >
                    <td className="se-drag-handle"
                      draggable
                      onDragStart={e => { e.stopPropagation(); setDragRowIdx(i) }}
                      title="Drag to reorder"
                    >⠿</td>
                    <td className="se-idx">{i}</td>
                    <td>
                      <input
                        type="text"
                        className="se-label-input"
                        value={p.label || ''}
                        onChange={e => handleLabelChange(i, e.target.value)}
                        onClick={e => e.stopPropagation()}
                        placeholder="—"
                      />
                    </td>
                    <td>
                      <select value={p.ty} onChange={e => handleTableChange(i, 'ty', e.target.value)}>
                        {POINT_TYPES.map(t => (
                          <option key={t.value} value={t.value}>{t.label}</option>
                        ))}
                      </select>
                    </td>
                    <td className="se-auto-cell-td">
                      <div className="se-auto-cell">
                        {p.x === null
                          ? <button className="se-auto-active" onClick={() => handleToggleAuto(i, 'x')} title="Click to set value">auto</button>
                          : <>
                              <input type="number" step="1" value={Math.round(p.x)} onChange={e => handleTableChange(i, 'x', e.target.value)} />
                              <button className="se-auto-btn" onClick={() => handleToggleAuto(i, 'x')} disabled={p.y === null} title="Set to auto">↺</button>
                            </>
                        }
                      </div>
                    </td>
                    <td className="se-auto-cell-td">
                      <div className="se-auto-cell">
                        {p.y === null
                          ? <button className="se-auto-active" onClick={() => handleToggleAuto(i, 'y')} title="Click to set value">auto</button>
                          : <>
                              <input type="number" step="1" value={Math.round(p.y)} onChange={e => handleTableChange(i, 'y', e.target.value)} />
                              <button className="se-auto-btn" onClick={() => handleToggleAuto(i, 'y')} disabled={p.x === null} title="Set to auto">↺</button>
                            </>
                        }
                      </div>
                    </td>
                    {p.ty === 1
                      ? renderSmoothAngleCell(p, i)
                      : <>{renderAngleCell('th_in', p.th_in, i)}{renderAngleCell('th_out', p.th_out, i)}</>
                    }
                    <td><button className="se-btn-del" onClick={() => deletePoint(i)} title="Delete">&times;</button></td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>

          {solveResult?.bezierPoints && (
            <details className="se-bezier-output">
              <summary>Bezier Output ({solveResult.bezierPoints.length} pts)</summary>
              <div className="se-table-scroll">
                <table className="se-table se-table-readonly">
                  <thead>
                    <tr><th>#</th><th>X</th><th>Y</th><th>&theta;in</th><th>&theta;out</th><th>Ld</th><th>Rd</th></tr>
                  </thead>
                  <tbody>
                    {solveResult.bezierPoints.map((bp, i) => (
                      <tr key={i}>
                        <td className="se-idx">{i}</td>
                        <td>{bp.x.toFixed(1)}</td>
                        <td>{bp.y.toFixed(1)}</td>
                        <td>{bp.th_in.toFixed(3)}</td>
                        <td>{bp.th_out.toFixed(3)}</td>
                        <td>{bp.ld.toFixed(1)}</td>
                        <td>{bp.rd.toFixed(1)}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </details>
          )}
        </div>
      </div>
    </div>
  )
}

export default SplineEditor
