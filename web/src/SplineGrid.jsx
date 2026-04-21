import { useState, useEffect, useRef } from 'react'

const TYPE_LABELS = ['Corner', 'Smooth', 'LineCurve', 'CurveLine']
const TYPE_SHORT = ['C', 'G2', 'LC', 'CL']

// Triangle vertices in math/y-up coords:
//   P0 = (50, 50)  bottom-left  (red)
//   P1 = (250, 50) bottom-right (red)
//   P2 = (150, 200) apex        (green)
// After scale(1,-1) flip to SVG screen coords:
//   P0 → (50, -50), P1 → (250, -50), P2 → (150, -200)
// ViewBox covers x:[30,270], y:[-215,-30] with padding
const VIEWBOX = '30 -215 240 185'
const CELL_W = 90
const CELL_H = 70

// Secondary defence: reject paths that contain scientific-notation numbers,
// which indicate the solver diverged and produced astronomically large coordinates.
const isValidPath = (p) => p && !p.includes('E+') && !p.includes('E-') && !p.includes('NaN') && !p.includes('Infinity')

const SplineCell = ({ pathSvg }) => {
  const valid = isValidPath(pathSvg)
  return (
    <td style={{ border: '1px solid #e0e0e0', padding: '1px', background: '#fff' }}>
      <svg width={CELL_W} height={CELL_H} viewBox={VIEWBOX} style={{ display: 'block' }}>
        {/* Triangle outline in SVG screen coords (y already flipped) */}
        <polygon
          points="50,-50 250,-50 150,-200"
          fill="none"
          stroke="#e0e0e0"
          strokeWidth="3"
          strokeDasharray="6,4"
        />
        {/* Spline path is in math y-up coords; flip with scale(1,-1) */}
        {valid ? (
          <g transform="scale(1,-1)">
            <path d={pathSvg} fill="none" stroke="#3366cc" strokeWidth="5" />
          </g>
        ) : (
          <text x="150" y="-120" textAnchor="middle" fontSize="28" fill="#ddd">✗</text>
        )}
        {/* Control point dots: base vertices red, apex green */}
        <circle cx="50"  cy="-50"  r="5" fill="#cc3333" />
        <circle cx="250" cy="-50"  r="5" fill="#cc3333" />
        <circle cx="150" cy="-200" r="5" fill="#33aa33" />
      </svg>
    </td>
  )
}

export default function SplineGrid() {
  const [gridData, setGridData] = useState(null)
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState(null)
  const workerRef = useRef(null)

  useEffect(() => {
    const worker = new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
    workerRef.current = worker

    worker.onmessage = (e) => {
      const { id, result, error: err } = e.data
      if (id !== 1) return
      if (err) {
        setError(err)
      } else {
        setGridData(result)
      }
      setLoading(false)
    }

    worker.postMessage({ id: 1, type: 'solveSplineGrid', args: [] })
    return () => worker.terminate()
  }, [])

  if (loading) return (
    <div style={{ padding: '20px', color: '#666' }}>
      Computing spline grid (256 splines)…
    </div>
  )
  if (error) return (
    <div style={{ padding: '20px', color: 'red' }}>Error: {error}</div>
  )

  const getCell = (isClosed, withTangent, t0, t1, t2) =>
    gridData.find(d =>
      d.isClosed === isClosed &&
      d.withTangent === withTangent &&
      d.types[0] === t0 &&
      d.types[1] === t1 &&
      d.types[2] === t2
    )

  const thStyle = {
    padding: '4px 8px',
    background: '#e8e8e8',
    color: '#111',
    fontWeight: 'bold',
    fontSize: '12px',
    border: '1px solid #ccc',
    whiteSpace: 'nowrap',
  }

  const sections = [
    { isClosed: false, withTangent: false, label: 'Open — No Tangent' },
    { isClosed: false, withTangent: true,  label: 'Open — With Horizontal Tangent at Apex' },
    { isClosed: true,  withTangent: false, label: 'Closed — No Tangent' },
    { isClosed: true,  withTangent: true,  label: 'Closed — With Horizontal Tangent at Apex' },
  ]

  return (
    <div style={{ padding: '20px', fontFamily: 'sans-serif', color: '#1a1a1a' }}>
      <h2 style={{ marginBottom: '6px' }}>Spline Grid</h2>
      <p style={{ marginBottom: '4px', fontSize: '13px', color: '#333', maxWidth: '700px' }}>
        All 4³=64 combinations of point types on an isosceles triangle.
        {' '}<strong>P0</strong> = bottom-left (red), <strong>P1</strong> = bottom-right (red),
        {' '}<strong>P2</strong> = apex (green). Rows: P0/P1 types. Columns: P2 type.
      </p>
      <p style={{ marginBottom: '20px', fontSize: '12px', color: '#555' }}>
        Types: C=Corner · G2=Smooth · LC=LineToCurve · CL=CurveToLine
      </p>

      {sections.map(({ isClosed, withTangent, label }) => (
        <div key={label} style={{ marginBottom: '40px' }}>
          <h3 style={{ marginBottom: '8px', color: '#111' }}>{label}</h3>
          <table style={{ borderCollapse: 'collapse', color: '#1a1a1a' }}>
            <thead>
              <tr>
                <th style={{ ...thStyle, textAlign: 'left', minWidth: '72px' }}>P0/P1</th>
                {TYPE_SHORT.map((s, t2) => (
                  <th key={t2} style={{ ...thStyle, textAlign: 'center', width: CELL_W + 'px' }}>
                    P2={s}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {[0, 1, 2, 3].flatMap(t0 =>
                [0, 1, 2, 3].map(t1 => (
                  <tr
                    key={`${t0}-${t1}`}
                    style={{ borderTop: t1 === 0 && t0 > 0 ? '2px solid #bbb' : undefined }}
                  >
                    <td style={{
                      ...thStyle,
                      fontWeight: 'normal',
                      background: t1 === 0 ? '#e8e8e8' : '#f8f8f8',
                    }}>
                      {TYPE_SHORT[t0]}/{TYPE_SHORT[t1]}
                    </td>
                    {[0, 1, 2, 3].map(t2 => {
                      const cell = getCell(isClosed, withTangent, t0, t1, t2)
                      return <SplineCell key={t2} pathSvg={cell?.pathSvg ?? ''} />
                    })}
                  </tr>
                ))
              )}
            </tbody>
          </table>
        </div>
      ))}
    </div>
  )
}
