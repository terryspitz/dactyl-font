import { useState, useEffect, useRef, useCallback, useMemo } from 'react'
import './SplineEditor.css'

const POINT_TYPES = [
  { value: 0, label: 'Corner' },
  { value: 1, label: 'Smooth' },
  { value: 2, label: 'Line-Curve' },
  { value: 3, label: 'Curve-Line' },
]

const HANDLE_LEN = 150 // length of tangent handles in SVG coords

// Fix #1: Use exact Math.PI values instead of approximate decimals
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

// Fix #2: Angle display helpers — store radians internally, display degrees
const toDeg = r => Math.round(r * 180 / Math.PI * 10) / 10
const toRad = d => Math.round(d * Math.PI / 180 * 1000) / 1000

function SplineEditor({ axes }) {
  const [glyphList, setGlyphList] = useState([])
  const [selectedChar, setSelectedChar] = useState('a')
  const [curves, setCurves] = useState([]) // array of { isClosed, points[] }
  const [activeCurve, setActiveCurve] = useState(0)
  const [guides, setGuides] = useState(null)
  const [solveResult, setSolveResult] = useState(null)
  const [selectedPt, setSelectedPt] = useState(null) // index into active curve's points
  const [maxIter, setMaxIter] = useState(1000)
  const [showComb, setShowComb] = useState(false)
  const [showTangents, setShowTangents] = useState(true)
  const svgRef = useRef(null)
  const workerRef = useRef(null)
  const solveIdRef = useRef(0)
  const dragRef = useRef(null) // { type: 'knot'|'th_in'|'th_out', curveIdx, idx }

  // Fix #3: curvesRef always holds latest curves value without stale closures
  const curvesRef = useRef(curves)
  useEffect(() => { curvesRef.current = curves }, [curves])

  // Fix #3: Undo/redo stacks (refs so mutations don't trigger re-renders)
  const undoStackRef = useRef([])
  const redoStackRef = useRef([])

  const pushUndo = useCallback(() => {
    undoStackRef.current.push(JSON.parse(JSON.stringify(curvesRef.current)))
    if (undoStackRef.current.length > 50) undoStackRef.current.shift()
    redoStackRef.current = []
  }, [])

  const undo = useCallback(() => {
    if (!undoStackRef.current.length) return
    redoStackRef.current.push(JSON.parse(JSON.stringify(curvesRef.current)))
    setCurves(undoStackRef.current.pop())
  }, [])

  const redo = useCallback(() => {
    if (!redoStackRef.current.length) return
    undoStackRef.current.push(JSON.parse(JSON.stringify(curvesRef.current)))
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
          // Clear history when loading a new glyph
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

  // Parse glyph when char or axes change
  useEffect(() => {
    if (!workerRef.current || !selectedChar) return
    workerRef.current.postMessage({ id: -3, type: 'parseGlyph', args: [selectedChar, axes] })
  }, [selectedChar, axes])

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
    return { x: minX, y: -(maxY), w: maxX - minX, h: maxY - minY }
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

  // updatePoint does NOT push undo — callers are responsible for calling pushUndo first
  const updatePoint = useCallback((curveIdx, ptIdx, updates) => {
    setCurves(prev => {
      const next = [...prev]
      const curve = { ...next[curveIdx], points: [...next[curveIdx].points] }
      curve.points[ptIdx] = { ...curve.points[ptIdx], ...updates }
      next[curveIdx] = curve
      return next
    })
  }, [])

  // Drag handlers
  const handlePointerDown = useCallback((e, type, curveIdx, idx) => {
    e.preventDefault()
    e.stopPropagation()
    pushUndo() // Fix #7: push undo at drag start, not on every move
    dragRef.current = { type, curveIdx, idx }
    setActiveCurve(curveIdx)
    setSelectedPt(idx)
    svgRef.current?.setPointerCapture(e.pointerId)
  }, [pushUndo])

  // Fix #4: Use curvesRef to avoid stale closure during rapid drag updates
  const handlePointerMove = useCallback((e) => {
    if (!dragRef.current) return
    const { type, curveIdx, idx } = dragRef.current
    const { x, y } = svgPoint(e.clientX, e.clientY)
    const curve = curvesRef.current[curveIdx]
    if (!curve) return
    const pt = curve.points[idx]

    if (type === 'knot') {
      updatePoint(curveIdx, idx, { x: Math.round(x), y: Math.round(y) })
    } else if (type === 'th_in' || type === 'th_out') {
      const px = pt.x ?? 0
      const py = pt.y ?? 0
      const angle = Math.round(Math.atan2(y - py, x - px) * 100) / 100
      updatePoint(curveIdx, idx, { [type]: angle })
    }
  }, [svgPoint, updatePoint]) // removed 'curves' dep — use curvesRef instead

  const handlePointerUp = useCallback(() => {
    dragRef.current = null
  }, [])

  const handleSvgClick = useCallback((e) => {
    if (e.target === svgRef.current || e.target.tagName === 'svg') setSelectedPt(null)
  }, [])

  // Fix #8: Double-click on empty canvas to add a point at that position
  const handleDblClick = useCallback((e) => {
    if (e.target !== svgRef.current && e.target.tagName !== 'svg') return
    e.preventDefault() // prevent text selection
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

  // Table editing
  const handleTableChange = useCallback((idx, field, value) => {
    pushUndo() // Fix #7
    const updates = {}
    if (field === 'ty') {
      updates.ty = parseInt(value)
    } else if (field === 'x' || field === 'y') {
      updates[field] = value === '' ? null : Math.round(parseFloat(value))
    } else if (field === 'th_in' || field === 'th_out') {
      // Fix #2: input shows degrees, convert to radians for storage
      updates[field] = value === '' ? null : toRad(parseFloat(value))
    }
    updatePoint(activeCurve, idx, updates)
  }, [activeCurve, pushUndo, updatePoint])

  // Label changes are frequent (per keystroke) so don't push undo for each one
  const handleLabelChange = useCallback((idx, value) => {
    updatePoint(activeCurve, idx, { label: value })
  }, [activeCurve, updatePoint])

  const handleToggleAuto = useCallback((idx, field) => {
    const pt = curves[activeCurve]?.points[idx]
    if (!pt) return
    pushUndo() // Fix #7
    if (field === 'x') {
      if (pt.x === null) {
        const def = guides ? Math.round((guides.xGuides[0].value + guides.xGuides[guides.xGuides.length - 1].value) / 2) : 0
        updatePoint(activeCurve, idx, { x: def })
      } else if (pt.y !== null) {
        updatePoint(activeCurve, idx, { x: null })
      }
    } else if (field === 'y') {
      if (pt.y === null) {
        // Fix #4: use midpoint index instead of hardcoded index 2
        const midIdx = guides ? Math.floor(guides.yGuides.length / 2) : 0
        const def = guides ? Math.round(guides.yGuides[midIdx]?.value ?? 0) : 0
        updatePoint(activeCurve, idx, { y: def })
      } else if (pt.x !== null) {
        updatePoint(activeCurve, idx, { y: null })
      }
    } else if (field === 'th_in') {
      updatePoint(activeCurve, idx, { th_in: pt.th_in === null ? 0 : null })
    } else if (field === 'th_out') {
      updatePoint(activeCurve, idx, { th_out: pt.th_out === null ? 0 : null })
    }
  }, [activeCurve, curves, guides, pushUndo, updatePoint])

  const addPoint = useCallback(() => {
    pushUndo() // Fix #7
    setCurves(prev => {
      const next = [...prev]
      const curve = { ...next[activeCurve], points: [...next[activeCurve].points] }
      const cx = guides ? Math.round((guides.xGuides[0].value + guides.xGuides[guides.xGuides.length - 1].value) / 2) : 400
      // Fix #6: use midpoint index instead of hardcoded index 2
      const midIdx = guides ? Math.floor(guides.yGuides.length / 2) : 0
      const cy = guides ? Math.round(guides.yGuides[midIdx].value) : 500
      curve.points.push({ ty: 1, x: cx, y: cy, th_in: null, th_out: null, label: '' })
      next[activeCurve] = curve
      return next
    })
  }, [activeCurve, guides, pushUndo])

  const deletePoint = useCallback((idx) => {
    pushUndo() // Fix #7
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

  const toggleClosed = useCallback(() => {
    pushUndo() // Fix #7
    setCurves(prev => {
      const next = [...prev]
      next[activeCurve] = { ...next[activeCurve], isClosed: !next[activeCurve].isClosed }
      return next
    })
  }, [activeCurve, pushUndo])

  // Fix #9: Keyboard shortcuts
  useEffect(() => {
    const handler = (e) => {
      // Don't intercept when focus is in an input/select
      if (e.target.tagName === 'INPUT' || e.target.tagName === 'SELECT' || e.target.tagName === 'TEXTAREA') return

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
        pushUndo()
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
          <label className="se-iter">
            Iter:
            {/* Fix #5: don't fall back to 200 on empty — only update when value is valid */}
            <input
              type="number" min={1} max={2000} value={maxIter}
              onChange={e => { const v = parseInt(e.target.value); if (v > 0) setMaxIter(v) }}
            />
          </label>
          {/* Fix #3: Undo/redo buttons */}
          <div className="se-undo-btns">
            <button
              className="se-btn-sm"
              onClick={undo}
              title="Undo (Ctrl+Z)"
              disabled={undoStackRef.current.length === 0}
            >↩</button>
            <button
              className="se-btn-sm"
              onClick={redo}
              title="Redo (Ctrl+Y)"
              disabled={redoStackRef.current.length === 0}
            >↪</button>
          </div>
        </div>
      </div>

      <div className="se-main">
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

          {/* Solved spline path — path data is in math coords (Y up), so flip Y */}
          {solveResult && (
            <g transform="scale(1,-1)">
              <path d={solveResult.pathSvg} fill="none" stroke="#4488ff" strokeWidth="2" />
              {showComb && <path d={solveResult.combSvg} fill="none" stroke="#888" strokeWidth="1" />}
              {showTangents && <path d={solveResult.tangentSvg} fill="none" stroke="#e00000" strokeWidth="1" />}
            </g>
          )}

          {/* All curves: inactive ones drawn first (behind), active on top */}
          {curves.map((curve, ci) => ci !== activeCurve && (
            <g key={`ghost-${ci}`} opacity="0.5">
              {curve.points.map((p, i) => renderKnot(p, i, ci, false))}
            </g>
          ))}

          {/* Active curve: tangent handles (drawn under knots) */}
          {points.map((p, i) => {
            if (p.x == null || p.y == null) return null
            const px = p.x
            const py = -p.y
            const handles = []
            if (p.th_in != null) {
              const hx = px + HANDLE_LEN * Math.cos(p.th_in + Math.PI)
              const hy = py - HANDLE_LEN * Math.sin(p.th_in + Math.PI)
              handles.push(
                <g key={`h-in-${i}`}>
                  <line x1={px} y1={py} x2={hx} y2={hy} stroke="#e07020" strokeWidth="2" />
                  <circle cx={hx} cy={hy} r={14} fill="#e07020" stroke="#fff" strokeWidth="2"
                    style={{ cursor: 'grab' }}
                    onPointerDown={e => handlePointerDown(e, 'th_in', activeCurve, i)} />
                </g>
              )
            }
            if (p.th_out != null) {
              const hx = px + HANDLE_LEN * Math.cos(p.th_out)
              const hy = py - HANDLE_LEN * Math.sin(p.th_out)
              handles.push(
                <g key={`h-out-${i}`}>
                  <line x1={px} y1={py} x2={hx} y2={hy} stroke="#2070e0" strokeWidth="2" />
                  <circle cx={hx} cy={hy} r={14} fill="#2070e0" stroke="#fff" strokeWidth="2"
                    style={{ cursor: 'grab' }}
                    onPointerDown={e => handlePointerDown(e, 'th_out', activeCurve, i)} />
                </g>
              )
            }
            return <g key={`handles-${i}`}>{handles}</g>
          })}

          {/* Active curve: knot points (on top) */}
          {points.map((p, i) => renderKnot(p, i, activeCurve, true))}
        </svg>

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
                  <th>#</th>
                  <th>Label</th>
                  <th>Type</th>
                  <th>X</th>
                  <th>Y</th>
                  {/* Fix #2: column headers now indicate degrees */}
                  <th>&theta;in °</th>
                  <th>&theta;out °</th>
                  <th></th>
                </tr>
              </thead>
              <tbody>
                {points.map((p, i) => (
                  <tr key={i} className={i === selectedPt ? 'selected' : ''} onClick={() => setSelectedPt(i)}>
                    <td className="se-idx">{i}</td>
                    {/* Fix #10: label column */}
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
                    <td className="se-auto-cell">
                      {p.x === null
                        ? <button className="se-auto-active" onClick={() => handleToggleAuto(i, 'x')} title="Click to set value">auto</button>
                        : <>
                            <input type="number" step="1" value={Math.round(p.x)} onChange={e => handleTableChange(i, 'x', e.target.value)} />
                            {/* Fix #11: replace cryptic '~' with '↺' */}
                            <button className="se-auto-btn" onClick={() => handleToggleAuto(i, 'x')} disabled={p.y === null} title="Set to auto">↺</button>
                          </>
                      }
                    </td>
                    <td className="se-auto-cell">
                      {p.y === null
                        ? <button className="se-auto-active" onClick={() => handleToggleAuto(i, 'y')} title="Click to set value">auto</button>
                        : <>
                            <input type="number" step="1" value={Math.round(p.y)} onChange={e => handleTableChange(i, 'y', e.target.value)} />
                            <button className="se-auto-btn" onClick={() => handleToggleAuto(i, 'y')} disabled={p.x === null} title="Set to auto">↺</button>
                          </>
                      }
                    </td>
                    <td className="se-auto-cell">
                      {p.th_in === null
                        ? <button className="se-auto-active" onClick={() => handleToggleAuto(i, 'th_in')} title="Click to set value">auto</button>
                        : <>
                            {/* Fix #2: display degrees, step by 1° */}
                            <input
                              type="number" step="1"
                              value={toDeg(p.th_in).toFixed(1)}
                              onChange={e => handleTableChange(i, 'th_in', e.target.value)}
                            />
                            {/* Fix #12: preset select calls updatePoint directly with radian values */}
                            <select
                              className="se-preset-select"
                              value=""
                              onChange={e => {
                                if (e.target.value !== '') {
                                  pushUndo()
                                  updatePoint(activeCurve, i, { th_in: parseFloat(e.target.value) })
                                }
                                e.target.value = ''
                              }}
                              title="Cardinal preset"
                            >
                              <option value="">·</option>
                              {CARDINAL_PRESETS.map(pr => <option key={pr.label} value={pr.value}>{pr.label}</option>)}
                            </select>
                            <button className="se-auto-btn" onClick={() => handleToggleAuto(i, 'th_in')} title="Set to auto">↺</button>
                          </>
                      }
                    </td>
                    <td className="se-auto-cell">
                      {p.th_out === null
                        ? <button className="se-auto-active" onClick={() => handleToggleAuto(i, 'th_out')} title="Click to set value">auto</button>
                        : <>
                            <input
                              type="number" step="1"
                              value={toDeg(p.th_out).toFixed(1)}
                              onChange={e => handleTableChange(i, 'th_out', e.target.value)}
                            />
                            <select
                              className="se-preset-select"
                              value=""
                              onChange={e => {
                                if (e.target.value !== '') {
                                  pushUndo()
                                  updatePoint(activeCurve, i, { th_out: parseFloat(e.target.value) })
                                }
                                e.target.value = ''
                              }}
                              title="Cardinal preset"
                            >
                              <option value="">·</option>
                              {CARDINAL_PRESETS.map(pr => <option key={pr.label} value={pr.value}>{pr.label}</option>)}
                            </select>
                            <button className="se-auto-btn" onClick={() => handleToggleAuto(i, 'th_out')} title="Set to auto">↺</button>
                          </>
                      }
                    </td>
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
