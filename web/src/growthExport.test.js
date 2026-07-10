import { describe, it, expect } from 'vitest'
import { svgBlob, growFilenameBase } from './growthExport'

// svgToPngBlob needs a DOM Image/canvas, so it is exercised by the Playwright
// checks; the pure helpers are covered here.

describe('growFilenameBase', () => {
    it('derives a safe basename from the text', () => {
        expect(growFilenameBase('dactyl')).toBe('dactyl-grow-dactyl')
        expect(growFilenameBase('hi there')).toBe('dactyl-grow-hi-there')
    })

    it('strips unsafe characters and trims separators', () => {
        expect(growFilenameBase('a/b\\c?*')).toBe('dactyl-grow-abc')
        expect(growFilenameBase('  spaced  ')).toBe('dactyl-grow-spaced')
        expect(growFilenameBase('!!!')).toBe('dactyl-grow')
    })

    it('falls back to a bare name for empty text', () => {
        expect(growFilenameBase('')).toBe('dactyl-grow')
        expect(growFilenameBase(null)).toBe('dactyl-grow')
    })

    it('caps very long text', () => {
        const base = growFilenameBase('x'.repeat(100))
        expect(base.length).toBeLessThanOrEqual('dactyl-grow-'.length + 24)
    })
})

describe('svgBlob', () => {
    it('wraps an SVG string in an image/svg+xml blob', () => {
        const b = svgBlob('<svg></svg>')
        expect(b.type).toContain('image/svg+xml')
        expect(b.size).toBe(11)
    })
})
