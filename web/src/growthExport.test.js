import { describe, it, expect } from 'vitest'
import { svgBlob, growFilenameBase, filenameBase, svgPixelSize } from './growthExport'

// svgToPngBlob needs a DOM Image/canvas, so it is exercised by the Playwright
// checks; the pure helpers are covered here.

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

    it('derives a safe basename for the Generate tab modes', () => {
        expect(filenameBase('dactyl-bubble', 'dactyl')).toBe('dactyl-bubble-dactyl')
        expect(filenameBase('dactyl-grow', 'hi there')).toBe('dactyl-grow-hi-there')
    })

    it('caps very long text', () => {
        const base = filenameBase('dactyl-bubble', 'x'.repeat(100))
        expect(base.length).toBeLessThanOrEqual('dactyl-bubble-'.length + 24)
    })
})

describe('growFilenameBase', () => {
    it('is a dactyl-grow-prefixed shorthand', () => {
        expect(growFilenameBase('dactyl')).toBe('dactyl-grow-dactyl')
    })
})

describe('svgBlob', () => {
    it('wraps an SVG string in an image/svg+xml blob', () => {
        const b = svgBlob('<svg></svg>')
        expect(b.type).toContain('image/svg+xml')
        expect(b.size).toBe(11)
    })
})
