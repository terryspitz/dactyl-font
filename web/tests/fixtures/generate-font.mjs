// Generates tests/fixtures/test-font.ttf — a tiny synthetic font used by the
// font-compare visual test. Each glyph is a simple rectangle (cap-height box
// for uppercase/digits, x-height box for lowercase) so the comparison overlay
// is deterministic and self-contained (no third-party font / network needed).
//
// Regenerate with:  node tests/fixtures/generate-font.mjs
import opentype from 'opentype.js'
import { writeFileSync } from 'fs'
import { fileURLToPath } from 'url'
import { dirname, join } from 'path'

const UPM = 1000
const ADV = 600
const CAP = 700   // uppercase / digit height
const XH = 500    // lowercase height
const PAD = 90    // side padding inside the advance

function box(top) {
  const p = new opentype.Path()
  p.moveTo(PAD, 0)
  p.lineTo(ADV - PAD, 0)
  p.lineTo(ADV - PAD, top)
  p.lineTo(PAD, top)
  p.close()
  return p
}

const glyphs = [new opentype.Glyph({ name: '.notdef', advanceWidth: ADV, path: new opentype.Path() })]
glyphs.push(new opentype.Glyph({ name: 'space', unicode: 32, advanceWidth: ADV, path: new opentype.Path() }))

const ranges = [
  ['A', 'Z', CAP],
  ['a', 'z', XH],
  ['0', '9', CAP],
]
for (const [from, to, top] of ranges) {
  for (let c = from.charCodeAt(0); c <= to.charCodeAt(0); c++) {
    glyphs.push(new opentype.Glyph({
      name: `g${c}`,
      unicode: c,
      advanceWidth: ADV,
      path: box(top),
    }))
  }
}

const font = new opentype.Font({
  familyName: 'DactylTestFixture',
  styleName: 'Regular',
  unitsPerEm: UPM,
  ascender: 800,
  descender: -200,
  glyphs,
})

const out = join(dirname(fileURLToPath(import.meta.url)), 'test-font.ttf')
writeFileSync(out, Buffer.from(font.toArrayBuffer()))
console.log('wrote', out, glyphs.length, 'glyphs')
