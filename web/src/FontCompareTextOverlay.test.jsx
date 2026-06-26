import { describe, it, expect } from 'vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import FontCompareTextOverlay from './FontCompareTextOverlay'

const render = (props) =>
  renderToStaticMarkup(
    <FontCompareTextOverlay
      text="Hi"
      fontFamily="'Roboto'"
      dactylFamily="DactylCompare"
      labelB="Roboto"
      {...props}
    />
  )

describe('FontCompareTextOverlay', () => {
  it('renders a three-column cell per character', () => {
    const html = render()
    // 2 chars × 3 columns
    expect((html.match(/compare-text-col/g) || []).length).toBe(6)
    expect((html.match(/compare-text-cell/g) || []).length).toBe(2)
  })

  it('renders the overlaid column with both font families', () => {
    const html = render()
    // React HTML-escapes the quotes inside the inline style attribute.
    expect(html).toContain('font-family:DactylCompare')
    expect(html).toContain('font-family:&#x27;Roboto&#x27;')
    expect((html.match(/compare-text-overlay/g) || []).length).toBe(2)
  })

  it('shows the comparison label in the key', () => {
    const html = render({ labelB: 'Open Sans' })
    expect(html).toContain('Open Sans')
  })

  it('strips newlines from the text', () => {
    const html = render({ text: 'A\nB' })
    expect((html.match(/compare-text-cell/g) || []).length).toBe(2)
  })
})
