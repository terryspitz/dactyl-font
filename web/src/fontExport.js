import opentype from 'opentype.js'

/**
 * Parse a SVG path data string (as produced by the F# generator) into an array
 * of command objects.  The generator emits commands in the form:
 *   "M x,y"  "L x,y"  "C x1,y1 x2,y2 x,y"  "Z"
 * separated by spaces.
 */
export function parseSvgPath(pathData) {
  if (!pathData) return []
  const commands = []
  const tokens = pathData.trim().split(/\s+/)
  let i = 0
  while (i < tokens.length) {
    const token = tokens[i]
    if (token === 'M') {
      const [x, y] = tokens[i + 1].split(',').map(Number)
      commands.push({ type: 'M', x, y })
      i += 2
    } else if (token === 'L') {
      const [x, y] = tokens[i + 1].split(',').map(Number)
      commands.push({ type: 'L', x, y })
      i += 2
    } else if (token === 'C') {
      const [x1, y1] = tokens[i + 1].split(',').map(Number)
      const [x2, y2] = tokens[i + 2].split(',').map(Number)
      const [x, y] = tokens[i + 3].split(',').map(Number)
      commands.push({ type: 'C', x1, y1, x2, y2, x, y })
      i += 4
    } else if (token === 'Z' || token === 'z') {
      commands.push({ type: 'Z' })
      i++
    } else {
      i++ // skip unrecognised tokens
    }
  }
  return commands
}

/**
 * Build an opentype.Font from the glyph data returned by generateFontGlyphData.
 * Coordinates from the F# generator are already Y-up (baseline at 0, positive
 * values go up), which matches the opentype.js coordinate convention directly.
 */
function buildFont(glyphData, familyName = 'Dactyl') {
  const { glyphs: glyphsData, ascender, descender, unitsPerEm } = glyphData

  const notdef = new opentype.Glyph({
    name: '.notdef',
    advanceWidth: Math.round(unitsPerEm / 2),
    path: new opentype.Path(),
  })

  const glyphs = [
    notdef,
    ...glyphsData
      .filter(g => g.pathData && g.unicode >= 32)
      .sort((a, b) => a.unicode - b.unicode)
      .map(g => {
        const path = new opentype.Path()
        for (const cmd of parseSvgPath(g.pathData)) {
          switch (cmd.type) {
            case 'M': path.moveTo(cmd.x, cmd.y); break
            case 'L': path.lineTo(cmd.x, cmd.y); break
            case 'C': path.bezierCurveTo(cmd.x1, cmd.y1, cmd.x2, cmd.y2, cmd.x, cmd.y); break
            case 'Z': path.close(); break
          }
        }
        return new opentype.Glyph({
          name: `uni${g.unicode.toString(16).padStart(4, '0')}`,
          unicode: g.unicode,
          advanceWidth: Math.round(g.advanceWidth),
          path,
        })
      }),
  ]

  return new opentype.Font({
    familyName,
    styleName: 'Regular',
    unitsPerEm: Math.round(unitsPerEm),
    ascender: Math.round(ascender),
    descender: Math.round(descender),
    glyphs,
  })
}

/**
 * Build a font from the glyph data and return it as an OTF data URL suitable
 * for a CSS @font-face src declaration.
 */
export function buildFontDataUrl(glyphData, familyName = 'DactylPreview') {
  const font = buildFont(glyphData, familyName)
  const buffer = font.toArrayBuffer()
  const bytes = new Uint8Array(buffer)
  let binary = ''
  for (let i = 0; i < bytes.byteLength; i++) binary += String.fromCharCode(bytes[i])
  return 'data:font/otf;base64,' + btoa(binary)
}

/**
 * Build a font from the glyph data and trigger a browser download of the OTF file.
 */
export function downloadFont(glyphData, filename = 'dactyl.otf') {
  const font = buildFont(glyphData)
  const buffer = font.toArrayBuffer()
  const blob = new Blob([buffer], { type: 'font/otf' })
  const url = URL.createObjectURL(blob)
  const a = Object.assign(document.createElement('a'), { href: url, download: filename })
  a.click()
  URL.revokeObjectURL(url)
}
