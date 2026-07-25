import { describe, it, expect } from 'vitest'
import { svgBlob, filenameBase } from './growthExport'

// svgToPngBlob needs a DOM Image/canvas, so it is exercised by the Playwright
// checks; the pure helpers are covered here.

describe('filenameBase', () => {
    it('derives a safe basename from the mode and text', () => {
        expect(filenameBase('bubble', 'dactyl')).toBe('dactyl-bubble-dactyl')
        expect(filenameBase('grow', 'hi there')).toBe('dactyl-grow-hi-there')
    })

    it('strips unsafe characters and trims separators', () => {
        expect(filenameBase('bubble', 'a/b\\c?*')).toBe('dactyl-bubble-abc')
        expect(filenameBase('bubble', '  spaced  ')).toBe('dactyl-bubble-spaced')
        expect(filenameBase('bubble', '!!!')).toBe('dactyl-bubble')
    })

    it('falls back to a bare name for empty text', () => {
        expect(filenameBase('bubble', '')).toBe('dactyl-bubble')
        expect(filenameBase('bubble', null)).toBe('dactyl-bubble')
    })

    it('caps very long text', () => {
        const base = filenameBase('bubble', 'x'.repeat(100))
        expect(base.length).toBeLessThanOrEqual('dactyl-bubble-'.length + 24)
    })
})

describe('svgBlob', () => {
    it('wraps an SVG string in an image/svg+xml blob', () => {
        const b = svgBlob('<svg></svg>')
        expect(b.type).toContain('image/svg+xml')
        expect(b.size).toBe(11)
    })
})
