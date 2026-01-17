import { useState, useMemo } from 'react'
import { generateSvg, defaultAxes, controlDefinitions } from './lib/fable/Api' // Adjust path if needed
import './App.css'

function App() {
  const [text, setText] = useState('Dactyl')
  // Initialize axes from the F# defaultAxes
  // We convert to a plain object to ensure React state updates work smoothly if it's a class
  const [axes, setAxes] = useState({ ...defaultAxes })

  // Memoize SVG generation
  const svgContent = useMemo(() => {
    try {
      if (!text) return ""
      return generateSvg(text, axes)
    } catch (e) {
      console.error("Error generating SVG:", e)
      return `<text y="20" fill="red">Error: ${e.message}</text>`
    }
  }, [text, axes])

  const handleControlChange = (name, value) => {
    setAxes(prev => ({ ...prev, [name]: value }))
  }

  return (
    <div className="container">
      <div className="sidebar">
        <h2>Controls</h2>
        <div className="controls-list">
          {controlDefinitions.map(ctrl => (
            <div key={ctrl.name} className="control-group">
              <label>
                {ctrl.name}
              </label>
              <div className="control-input">
                <input
                  type={ctrl.type_}
                  min={ctrl.min}
                  max={ctrl.max}
                  step={ctrl.step}
                  checked={ctrl.type_ === 'checkbox' ? axes[ctrl.name] : undefined}
                  value={ctrl.type_ === 'checkbox' ? undefined : axes[ctrl.name]}
                  onChange={e => {
                    const val = ctrl.type_ === 'checkbox' ? e.target.checked : parseFloat(e.target.value)
                    handleControlChange(ctrl.name, val)
                  }}
                />
                {ctrl.type_ === 'range' && <span className="value-display">{Number(axes[ctrl.name]).toFixed(2)}</span>}
              </div>
            </div>
          ))}
        </div>
      </div>
      <div className="main">
        <div className="input-area">
          <textarea
            value={text}
            onChange={e => setText(e.target.value)}
            rows={3}
            placeholder="Type here..."
          />
        </div>
        <div className="preview">
          <div
            className="svg-container"
            dangerouslySetInnerHTML={{ __html: svgContent }}
          />
        </div>
      </div>
    </div>
  )
}

export default App
