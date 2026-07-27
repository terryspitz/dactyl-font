import { describe, it, expect } from 'vitest'
import { svgBlob, growFilenameBase, filenameBase, svgPixelSize } from './growthExport'

describe('svgPixelSize', () => {
    it('prefers explicit width/height attributes', () => {
        expect(svgPixelSize('<svg width="800" height="200" viewBox="0 0 10 10">')).toEqual({ w: 800, h: 200 })
    })

    it('falls back to the viewBox when there are no width/height attributes', () => {
        // what the F# generator's toSvgDocument emits
        expect(svgPixelSize("<svg xmlns='http://www.w3.org/2000/svg'\nviewBox='-50 -50 5497 1060'>"))
            .toEqual({ w: 5497, h: 1060 })
    })

    it('handles comma-separated and negative viewBox origins', () => {
        expect(svgPixelSize('<svg viewBox="-10,-20,300,150">')).toEqual({ w: 300, h: 150 })
    })

    it('falls back to a square when there is nothing to go on', () => {
        expect(svgPixelSize('<svg>')).toEqual({ w: 1000, h: 1000 })
        expect(svgPixelSize('not svg at all')).toEqual({ w: 1000, h: 1000 })
    })
})

describe('filenameBase', () => {
    it('joins prefix, cleaned text and suffix', () => {
        expect(filenameBase('dactyl', 'hi there', 'random42')).toBe('dactyl-hi-there-random42')
    })

    it('omits empty parts', () => {
        expect(filenameBase('dactyl', '', '')).toBe('dactyl')
        expect(filenameBase('dactyl', 'abc')).toBe('dactyl-abc')
        expect(filenameBase('dactyl', '', 'random42')).toBe('dactyl-random42')
    })

    it('strips unsafe characters from the text but not the suffix', () => {
        expect(filenameBase('dactyl', 'a/b\\c?*', 'random7')).toBe('dactyl-abc-random7')
    })
})

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
