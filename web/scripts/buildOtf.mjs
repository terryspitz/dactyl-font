// Headless OTF builder: runs the same Fable API + opentype.js pipeline the
// browser uses, so we can produce a real font file for QA (fontbakery) offline.
// Usage: node scripts/buildOtf.mjs [outPath]
import { writeFileSync } from 'node:fs'
import { generateFontGlyphData, defaultAxes } from '../src/lib/fable/Api.js'
import { buildFont } from '../src/fontExport.js'

const out = process.argv[2] || '/tmp/Dactyl-Regular.otf'
const glyphData = generateFontGlyphData(defaultAxes, undefined)
const font = buildFont(glyphData, 'Dactyl', 'Regular')
writeFileSync(out, Buffer.from(font.toArrayBuffer()))
console.log(`wrote ${out} (${glyphData.glyphs.length} glyphs)`)
