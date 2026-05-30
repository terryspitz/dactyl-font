import opentype from 'opentype.js'
import paper from 'paper'

// paper.js needs a project/scope set up once before any geometry can be built.
// We run it headlessly (no canvas) — only boolean geometry is used, never rendering.
let paperReady = false
function ensurePaper() {
  if (!paperReady) {
    paper.setup(new paper.Size(1, 1))
    paperReady = true
  }
}

/**
 * Remove overlaps from a glyph's outline.
 *
 * The F# generator strokes each spiro curve into its own closed "ribbon"
 * contour, so where two strokes meet (the bowl and stem of 'a', 'b', 'd', …)
 * the ribbons overlap.  OTF/CFF expects non-self-intersecting outlines, so we
 * union all the contours into a single set of non-overlapping paths.
 *
 * The target fill is the non-zero winding of all the raw contours together —
 * that is exactly what the on-screen preview renders (Font.fs uses
 * `fill-rule: nonzero`).  We try two strategies and verify each against that
 * target before accepting it, falling back to the raw contours if neither
 * works:
 *
 *   1. A single non-zero self-union (`CompoundPath.unite()`).  This is correct
 *      and cheap for the vast majority of glyphs.  paper.js's boolean operator
 *      table only covers winding numbers ±1/±2, though, so it mis-resolves
 *      places where three strokes overlap (e.g. the middle bar of 'B', where
 *      the stem and both bowls meet → winding 3).
 *   2. An iterative union: each contour is self-resolved, then accumulated with
 *      pairwise unite.  Because the accumulator is normalised to winding ≤ 1
 *      after every step, no individual boolean op ever sees winding 3, so this
 *      cleans the winding-3 joins that strategy 1 drops.  Contours that bound a
 *      counter (the reversed inner offset of a closed bowl like 'o') are
 *      detected by sampling just inside their edge and subtracted instead.
 *
 * Returns an array of command objects ({type:'M'|'C'|'Z', …}) ready to feed to
 * an opentype.Path.  Falls back to the raw parsed path if anything goes wrong.
 */
export function unionPath(pathData) {
  if (!pathData) return []
  let original
  try {
    ensurePaper()
    original = new paper.CompoundPath(pathData)
    original.fillRule = 'nonzero'

    // Strategy 1: a single non-zero self-union.
    const simple = original.clone().unite()
    simple.fillRule = 'nonzero'
    if (unionMatchesFill(original, simple)) return paperItemToCommands(simple)
    simple.remove()

    // Strategy 2: iterative union (handles winding-3 joins like 'B').
    const iterative = iterativeUnion(original, pathData)
    if (iterative && unionMatchesFill(original, iterative)) {
      return paperItemToCommands(iterative)
    }
    iterative?.remove()

    // Neither matched: keep the raw contours, which are themselves non-zero
    // correct (they are what the preview renders) — they just keep the overlap.
    return parseSvgPath(pathData)
  } catch (err) {
    // Geometry libraries can choke on degenerate input; never lose a glyph over it.
    console.warn('unionPath failed, using raw outline', err)
    return parseSvgPath(pathData)
  } finally {
    original?.remove()
  }
}

/**
 * Union a glyph's contours one at a time so paper.js never sees a winding ≥ 3.
 * Each contour is self-resolved first (winding ≤ 2, which paper handles) and
 * then united into the accumulator (winding ≤ 1) → every op stays within the
 * supported range.  Contours whose interior is empty in the target fill bound a
 * counter and are subtracted rather than added.
 */
function iterativeUnion(original, pathData) {
  const contours = contourPaths(pathData)
  if (!contours.length) return null

  const solids = []
  const holes = []
  for (const c of contours) {
    ;(original.contains(interiorEdgePoint(c)) ? solids : holes).push(c)
  }

  let acc = new paper.Path()
  for (const s of solids) {
    const resolved = s.unite()
    const next = acc.unite(resolved)
    acc.remove()
    resolved.remove()
    acc = next
  }
  for (const h of holes) {
    const resolved = h.unite()
    const next = acc.subtract(resolved)
    acc.remove()
    resolved.remove()
    acc = next
  }
  // Final self-union: merges contours that meet only at a point (e.g. the two
  // bowls of 'B' touching the stem at the waist), removing residual crossings.
  acc.fillRule = 'nonzero'
  const cleaned = acc.unite()
  acc.remove()
  cleaned.fillRule = 'nonzero'
  return cleaned
}

/** Split a glyph's path data into one paper.Path per contour. */
function contourPaths(pathData) {
  const paths = []
  let cur = null
  for (const cmd of parseSvgPath(pathData)) {
    switch (cmd.type) {
      case 'M': cur = new paper.Path(); cur.moveTo(new paper.Point(cmd.x, cmd.y)); paths.push(cur); break
      case 'L': cur.lineTo(new paper.Point(cmd.x, cmd.y)); break
      case 'C': cur.cubicCurveTo(new paper.Point(cmd.x1, cmd.y1), new paper.Point(cmd.x2, cmd.y2), new paper.Point(cmd.x, cmd.y)); break
      case 'Z': cur.closePath(); break
    }
  }
  return paths.filter(p => p.segments.length > 1)
}

/** A point a hair inside a contour's edge, used to test whether it bounds solid or counter. */
function interiorEdgePoint(contour) {
  const at = contour.length * 0.37
  const point = contour.getPointAt(at)
  const normal = contour.getNormalAt(at).multiply(0.5)
  const a = point.add(normal)
  return contour.contains(a) ? a : point.subtract(normal)
}

/**
 * Sanity-check that `candidate` covers the same filled region as `original`
 * under the non-zero rule, by sampling a grid over the glyph's bounding box.
 * Returns false if any sample disagrees, which signals the boolean op produced
 * a wrong outline and we should try the next strategy instead.
 */
function unionMatchesFill(original, candidate, grid = 32) {
  const b = original.bounds
  if (!b.width || !b.height) return true
  for (let i = 0; i < grid; i++) {
    for (let j = 0; j < grid; j++) {
      const x = b.left + (b.width * (i + 0.5)) / grid
      const y = b.top + (b.height * (j + 0.5)) / grid
      const p = new paper.Point(x, y)
      if (original.contains(p) !== candidate.contains(p)) return false
    }
  }
  return true
}

/** Convert a paper Path / CompoundPath into absolute M/C/Z command objects. */
function paperItemToCommands(item) {
  const paths = item.children && item.children.length ? item.children : [item]
  const cmds = []
  for (const p of paths) {
    const segs = p.segments
    if (!segs || segs.length === 0) continue
    cmds.push({ type: 'M', x: segs[0].point.x, y: segs[0].point.y })
    const n = p.closed ? segs.length : segs.length - 1
    for (let i = 0; i < n; i++) {
      const a = segs[i]
      const b = segs[(i + 1) % segs.length]
      cmds.push({
        type: 'C',
        x1: a.point.x + a.handleOut.x,
        y1: a.point.y + a.handleOut.y,
        x2: b.point.x + b.handleIn.x,
        y2: b.point.y + b.handleIn.y,
        x: b.point.x,
        y: b.point.y,
      })
    }
    cmds.push({ type: 'Z' })
  }
  return cmds
}

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
      .filter(g => g.unicode >= 32 && (g.pathData || g.unicode === 32))
      .sort((a, b) => a.unicode - b.unicode)
      .filter((g, i, arr) => i === 0 || g.unicode !== arr[i - 1].unicode)
      .map(g => {
        const path = new opentype.Path()
        for (const cmd of unionPath(g.pathData)) {
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
