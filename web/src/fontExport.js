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

/** Convert a paper Path / CompoundPath into absolute M/C/Z command objects.
 * Coordinates are rounded to 3 decimal places to eliminate sub-UPM floating-point
 * noise from paper.js boolean operations, keeping font output deterministic. */
function paperItemToCommands(item) {
  const paths = item.children && item.children.length ? item.children : [item]
  const cmds = []
  const r = (n) => Math.round(n * 1000) / 1000
  for (const p of paths) {
    const segs = p.segments
    if (!segs || segs.length === 0) continue
    cmds.push({ type: 'M', x: r(segs[0].point.x), y: r(segs[0].point.y) })
    const n = p.closed ? segs.length : segs.length - 1
    for (let i = 0; i < n; i++) {
      const a = segs[i]
      const b = segs[(i + 1) % segs.length]
      cmds.push({
        type: 'C',
        x1: r(a.point.x + a.handleOut.x),
        y1: r(a.point.y + a.handleOut.y),
        x2: r(b.point.x + b.handleIn.x),
        y2: r(b.point.y + b.handleIn.y),
        x: r(b.point.x),
        y: r(b.point.y),
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

/** Find axes whose values differ from the defaults. */
export function getOverriddenAxes(axes, defaultAxes) {
  return Object.entries(axes).filter(([key, val]) => val !== defaultAxes[key])
}

/** Format one axis override as a compact string for filenames and style names. */
function formatAxisOverride(key, val) {
  if (typeof val === 'boolean') return val ? key : `no_${key}`
  const n = typeof val === 'number' ? val : Number(val)
  const s = Number.isInteger(n) ? String(n) : n.toFixed(2).replace(/0+$/, '').replace(/\.$/, '')
  return `${key}${s}`
}

// A per-glyph-random export is not described by its axes — every glyph has its
// own — so the seed goes in the name instead.  It is the whole state, so naming
// the file after it makes a saved font traceable back to the settings that
// produced it.
function randomTag(glyphSeed) {
  return glyphSeed === null || glyphSeed === undefined ? '' : `Random${glyphSeed}`
}

export function buildStyleName(overrides, glyphSeed = null) {
  const parts = overrides.map(([k, v]) => formatAxisOverride(k, v))
  const tag = randomTag(glyphSeed)
  if (tag) parts.push(tag)
  return parts.length ? parts.join(' ') : 'Regular'
}

export function buildFilename(overrides, glyphSeed = null, family = 'Dactyl') {
  const parts = overrides.map(([k, v]) => formatAxisOverride(k, v))
  const tag = randomTag(glyphSeed)
  if (tag) parts.push(tag)
  return `${family}-${parts.length ? parts.join('-') : 'Regular'}.otf`
}

// The font is always exported at this em size.  The F# generator uses a
// variable em (font.charHeight ≈ 960–1150 depending on axes), but CFF/OTF
// fonts must declare unitsPerEm = 1000 to render on Windows: GDI (Font Viewer,
// Word, PowerPoint) and Adobe apps assume a 1000-unit em for PostScript
// outlines and draw glyphs blank or mis-scaled at any other value, even though
// browsers, macOS and FreeType honour the declared UPM.  See opentype.js
// issue #115.  We scale every coordinate and metric to this em on export.
const EXPORT_UPM = 1000

// Font metadata, kept in sync with OFL.txt at the repo root.  fontbakery's
// Google Fonts profile compares the embedded copyright/version against these,
// so any change here must match the licence file.
const COPYRIGHT =
  'Copyright 2020 Terry Spitz (https://github.com/terryspitz/dactyl-font), with Reserved Font Name "Dactyl".'
const FONT_VERSION = 1.0
const VERSION_STRING = 'Version 1.000'

// A visible outline for .notdef (a hollow box), which OpenType requires to be a
// real drawing rather than blank — the inner contour winds opposite the outer
// so the non-zero fill leaves it hollow.
function notdefPath() {
  const p = new opentype.Path()
  p.moveTo(80, 0); p.lineTo(520, 0); p.lineTo(520, 700); p.lineTo(80, 700); p.close()
  p.moveTo(160, 80); p.lineTo(160, 620); p.lineTo(440, 620); p.lineTo(440, 80); p.close()
  return p
}

/**
 * opentype.js cannot WRITE a `kern` table — `tables/kern.js` only exports a
 * `parse` function (for reading one on load), and the table list assembled by
 * `tables/sfnt.js`'s `fontToTable()` never references `font.kerningPairs` or
 * emits `kern`/`GPOS`. Setting `font.kerningPairs` (as buildFont does, below)
 * only feeds opentype.js's own in-memory `getKerningValue()` — nothing in
 * this codebase calls that, and it has zero effect on `toArrayBuffer()`'s
 * output. So real kerning has to be spliced into the compiled binary by
 * hand: build the raw table bytes ourselves (buildKernTableBytes,
 * buildGposTableBytes), then inject them into the sfnt (injectKerningTables)
 * by rebuilding the table directory, relaying out every table's offset, and
 * recomputing the whole-file checksum the `head` table's checkSumAdjustment
 * field commits to. Both a legacy `kern` table and a modern GPOS table are
 * written, because browsers' `@font-face` text shaping only honours GPOS —
 * see the comment on injectKerningTables for why `kern` alone isn't enough.
 */

/** Binary-search parameters every sfnt-style sorted table needs in its header. */
function binarySearchParams(n, unitSize) {
  let entrySelector = 0
  while ((1 << (entrySelector + 1)) <= n) entrySelector++
  const searchRange = (1 << entrySelector) * unitSize
  const rangeShift = n * unitSize - searchRange
  return { searchRange, entrySelector, rangeShift }
}

function padTo4(bytes) {
  const rem = bytes.length % 4
  if (rem === 0) return bytes
  const padded = new Uint8Array(bytes.length + (4 - rem))
  padded.set(bytes)
  return padded
}

/** Simple sfnt checksum: sum of big-endian uint32 words over 4-byte-padded bytes. */
function tableChecksum(bytes) {
  let sum = 0
  for (let i = 0; i < bytes.length; i += 4) {
    const word = ((bytes[i] << 24) | (bytes[i + 1] << 16) | (bytes[i + 2] << 8) | bytes[i + 3]) >>> 0
    sum = (sum + word) >>> 0
  }
  return sum >>> 0
}

/**
 * Raw bytes for a Windows-format (format 0) `kern` table: one subtable,
 * horizontal pair kerning. `pairs` is `{ "leftGlyphIndex,rightGlyphIndex": value }`.
 * https://learn.microsoft.com/en-us/typography/opentype/spec/kern
 */
export function buildKernTableBytes(pairs) {
  const entries = Object.entries(pairs)
    .map(([key, value]) => {
      const [left, right] = key.split(',').map(Number)
      return { left, right, value }
    })
    // Format 0 requires pairs sorted ascending by (left, right).
    .sort((a, b) => (a.left - b.left) || (a.right - b.right))

  const nPairs = entries.length
  const subtableHeaderSize = 14 // version+length+coverage+nPairs+searchRange+entrySelector+rangeShift
  const pairSize = 6 // left(uint16) + right(uint16) + value(int16)
  const subtableLength = subtableHeaderSize + nPairs * pairSize
  const { searchRange, entrySelector, rangeShift } = binarySearchParams(nPairs, pairSize)

  const buf = new ArrayBuffer(4 + subtableLength)
  const view = new DataView(buf)
  let o = 0
  view.setUint16(o, 0); o += 2               // kern table version
  view.setUint16(o, 1); o += 2               // nTables
  view.setUint16(o, 0); o += 2               // subtable version
  view.setUint16(o, subtableLength); o += 2
  view.setUint16(o, 0x0001); o += 2          // coverage: horizontal, format 0
  view.setUint16(o, nPairs); o += 2
  view.setUint16(o, searchRange); o += 2
  view.setUint16(o, entrySelector); o += 2
  view.setUint16(o, rangeShift); o += 2
  for (const { left, right, value } of entries) {
    view.setUint16(o, left); o += 2
    view.setUint16(o, right); o += 2
    view.setInt16(o, value); o += 2
  }
  return new Uint8Array(buf)
}

/**
 * Insert one or more tables into an already-built sfnt (OTF) ArrayBuffer.
 * Rebuilds the table directory (sorted by tag, per spec), relays out every
 * table's offset, and recomputes the whole-file checksum
 * `head.checkSumAdjustment` commits to — the standard "zero the field,
 * checksum the whole file, patch in 0xB1B0AFBA minus that" scheme
 * opentype.js itself uses when building the original buffer.
 */
function spliceTables(arrayBuffer, newTables) {
  if (!newTables.length) return arrayBuffer

  const src = new Uint8Array(arrayBuffer)
  const srcView = new DataView(arrayBuffer)
  const sfntVersion = srcView.getUint32(0)
  const numTables = srcView.getUint16(4)

  const tables = []
  for (let i = 0; i < numTables; i++) {
    const recOff = 12 + i * 16
    const tag = String.fromCharCode(src[recOff], src[recOff + 1], src[recOff + 2], src[recOff + 3])
    const checksum = srcView.getUint32(recOff + 4)
    const offset = srcView.getUint32(recOff + 8)
    const length = srcView.getUint32(recOff + 12)
    tables.push({ tag, checksum, bytes: padTo4(src.slice(offset, offset + length)), length })
  }

  for (const { tag, bytes: rawBytes } of newTables) {
    const bytes = padTo4(rawBytes)
    tables.push({ tag, checksum: tableChecksum(bytes), bytes, length: rawBytes.length })
  }

  // Table directory entries must be sorted ascending by tag (as a big-endian uint32).
  const tagValue = (tag) =>
    ((tag.charCodeAt(0) << 24) | (tag.charCodeAt(1) << 16) | (tag.charCodeAt(2) << 8) | tag.charCodeAt(3)) >>> 0
  tables.sort((a, b) => tagValue(a.tag) - tagValue(b.tag))

  const newNumTables = tables.length
  const dirSize = 12 + newNumTables * 16
  const { searchRange, entrySelector, rangeShift } = binarySearchParams(newNumTables, 16)

  // Lay out table payloads sequentially after the (larger) directory.
  let cursor = dirSize
  const layout = tables.map(t => {
    const offset = cursor
    cursor += t.bytes.length // already 4-byte padded
    return { ...t, offset }
  })

  const out = new Uint8Array(cursor)
  const outView = new DataView(out.buffer)

  outView.setUint32(0, sfntVersion)
  outView.setUint16(4, newNumTables)
  outView.setUint16(6, searchRange)
  outView.setUint16(8, entrySelector)
  outView.setUint16(10, rangeShift)

  layout.forEach((t, i) => {
    const recOff = 12 + i * 16
    for (let c = 0; c < 4; c++) out[recOff + c] = t.tag.charCodeAt(c)
    outView.setUint32(recOff + 4, t.checksum)
    outView.setUint32(recOff + 8, t.offset)
    outView.setUint32(recOff + 12, t.length)
    out.set(t.bytes, t.offset)
  })

  // head.checkSumAdjustment (bytes 8..11 of its payload) must be zero while
  // computing the whole-file checksum, then patched with the real value.
  const headEntry = layout.find(t => t.tag === 'head')
  const adjOffset = headEntry.offset + 8
  outView.setUint32(adjOffset, 0)
  const fileChecksum = tableChecksum(out) // `out` length is always a multiple of 4
  const checkSumAdjustment = (0xB1B0AFBA - fileChecksum) >>> 0
  outView.setUint32(adjOffset, checkSumAdjustment)

  return out.buffer
}

/** Insert a legacy `kern` table into an already-built sfnt ArrayBuffer. */
export function injectKernTable(arrayBuffer, kerningPairs) {
  if (!kerningPairs || Object.keys(kerningPairs).length === 0) return arrayBuffer
  return spliceTables(arrayBuffer, [{ tag: 'kern', bytes: buildKernTableBytes(kerningPairs) }])
}

/** Insert a `GPOS` table (Lookup Type 2 pair positioning) into an already-built sfnt ArrayBuffer. */
export function injectGposTable(arrayBuffer, kerningPairs) {
  if (!kerningPairs || Object.keys(kerningPairs).length === 0) return arrayBuffer
  return spliceTables(arrayBuffer, [{ tag: 'GPOS', bytes: buildGposTableBytes(kerningPairs) }])
}

/**
 * Insert both a legacy `kern` table and a `GPOS` table into an already-built
 * sfnt ArrayBuffer, in a single directory rebuild.
 *
 * Both carry the same pair values: `kern` is read directly by HarfBuzz (and
 * other shapers) when invoked outside a browser, e.g. by a native desktop
 * app opening the downloaded OTF. But Chromium's (and other browsers')
 * `@font-face` text-shaping path only honours **GPOS** pair positioning for
 * web fonts — the legacy `kern` table is never consulted there, even though
 * the underlying HarfBuzz engine can apply it when shaping is invoked
 * directly. Confirmed by: `uharfbuzz` shaping the font directly with only a
 * `kern` table applies the kern; the same font rendered via a real Chromium
 * `@font-face` (Playwright) does not move the text at all. So both tables
 * are needed to get kerning in front of a human, wherever they open the font.
 */
export function injectKerningTables(arrayBuffer, kerningPairs) {
  if (!kerningPairs || Object.keys(kerningPairs).length === 0) return arrayBuffer
  return spliceTables(arrayBuffer, [
    { tag: 'kern', bytes: buildKernTableBytes(kerningPairs) },
    { tag: 'GPOS', bytes: buildGposTableBytes(kerningPairs) },
  ])
}

// ---------------------------------------------------------------------------
// GPOS (Lookup Type 2, Pair Adjustment Positioning, format 1) table builder.
//
// opentype.js can parse GPOS but, like `kern`, has no writer for it (see
// tables/gpos.js: `makeGposTable` exists but `fontToTable()` in sfnt.js never
// calls it), so — same as the kern table above — the bytes are hand-built and
// spliced in. Format 1 (explicit per-pair lists, one PairSet per left glyph)
// is used rather than format 2 (class-based) because the kerns are already a
// sparse explicit pair list, exactly matching how buildKernTableBytes lays
// out the legacy table.
// https://learn.microsoft.com/en-us/typography/opentype/spec/gpos#lookup-type-2-pair-adjustment-positioning-subtable
// ---------------------------------------------------------------------------

function u16(v) {
  const b = new Uint8Array(2)
  new DataView(b.buffer).setUint16(0, v)
  return b
}

function i16(v) {
  const b = new Uint8Array(2)
  new DataView(b.buffer).setInt16(0, v)
  return b
}

function tag4(s) {
  return new Uint8Array([s.charCodeAt(0), s.charCodeAt(1), s.charCodeAt(2), s.charCodeAt(3)])
}

function concatBytes(...arrs) {
  const len = arrs.reduce((n, a) => n + a.length, 0)
  const out = new Uint8Array(len)
  let o = 0
  for (const a of arrs) { out.set(a, o); o += a.length }
  return out
}

/** PairPos format 1 subtable: one PairSet (list of {secondGlyph, xAdvance}) per covered left glyph. */
function buildPairPosSubtable(entries) {
  const byLeft = new Map()
  for (const { left, right, value } of entries) {
    if (!byLeft.has(left)) byLeft.set(left, [])
    byLeft.get(left).push({ right, value })
  }
  const lefts = [...byLeft.keys()].sort((a, b) => a - b)
  for (const left of lefts) byLeft.get(left).sort((a, b) => a.right - b.right)

  const pairSetCount = lefts.length
  const headerSize = 10 + 2 * pairSetCount // posFormat+coverageOffset+valueFormat1+valueFormat2+pairSetCount + offsets[]
  const pairSetBytes = lefts.map(left => {
    const pairs = byLeft.get(left)
    // ValueFormat1 = 0x0004 (XAdvance only) so each record is secondGlyph + one int16 field.
    // ValueFormat2 = 0x0000 (no adjustment to the second glyph) so value2 is omitted entirely.
    const records = pairs.map(({ right, value }) => concatBytes(u16(right), i16(value)))
    return concatBytes(u16(pairs.length), ...records)
  })

  let cursor = headerSize
  const pairSetOffsets = []
  for (const b of pairSetBytes) { pairSetOffsets.push(cursor); cursor += b.length }
  const coverageOffset = cursor
  const coverage = concatBytes(u16(1), u16(pairSetCount), ...lefts.map(u16)) // format 1: sorted glyph list

  const header = concatBytes(
    u16(1), // PosFormat 1
    u16(coverageOffset),
    u16(0x0004),
    u16(0x0000),
    u16(pairSetCount),
    ...pairSetOffsets.map(u16),
  )
  return concatBytes(header, ...pairSetBytes, coverage)
}

function buildLookupTable(subtableBytes) {
  // lookupType=2 (Pair Adjustment), lookupFlag=0, subTableCount=1, subtable offset=8 (right after this header)
  return concatBytes(u16(2), u16(0), u16(1), u16(8), subtableBytes)
}

function buildLookupListTable(lookupBytes) {
  return concatBytes(u16(1), u16(4), lookupBytes) // lookupCount=1, offset to the one Lookup table = 4
}

function buildFeatureListTable() {
  // Single 'kern' feature whose one lookup is lookup index 0.
  const feature = concatBytes(u16(0), u16(1), u16(0)) // featureParams=NULL, lookupIndexCount=1, lookupListIndex[0]=0
  const header = concatBytes(u16(1), tag4('kern'), u16(8)) // featureCount=1, FeatureRecord{tag, offset=8}
  return concatBytes(header, feature)
}

/** LangSys enabling feature index 0 ('kern'), with no language-specific override. */
function buildScriptTable() {
  const langSys = concatBytes(u16(0), u16(0xFFFF), u16(1), u16(0))
  const header = concatBytes(u16(4), u16(0)) // defaultLangSysOffset=4, langSysCount=0
  return concatBytes(header, langSys)
}

/**
 * DFLT + latn, both pointing at the same default LangSys — DFLT covers
 * shapers that don't resolve a script at all, latn covers ones that do
 * (Dactyl is Latin-only). Tags are listed in binary order per spec, which
 * for these two tags is also alphabetical ('D' < 'l' in ASCII).
 */
function buildScriptListTable() {
  const scripts = [
    { tag: 'DFLT', table: buildScriptTable() },
    { tag: 'latn', table: buildScriptTable() },
  ]
  const headerSize = 2 + scripts.length * 6
  let cursor = headerSize
  const records = scripts.map(s => {
    const offset = cursor
    cursor += s.table.length
    return { tag: s.tag, offset }
  })
  const header = concatBytes(u16(scripts.length), ...records.map(r => concatBytes(tag4(r.tag), u16(r.offset))))
  return concatBytes(header, ...scripts.map(s => s.table))
}

/**
 * Raw bytes for a minimal GPOS table (version 1.0) carrying exactly one
 * lookup: horizontal pair kerning via the 'kern' feature, under both the
 * DFLT and latn scripts. `pairs` is
 * `{ "leftGlyphIndex,rightGlyphIndex": value }`, the same shape buildFont
 * stashes on `font.kerningPairs` and buildKernTableBytes already consumes.
 */
export function buildGposTableBytes(pairs) {
  const entries = Object.entries(pairs).map(([key, value]) => {
    const [left, right] = key.split(',').map(Number)
    return { left, right, value }
  })

  const scriptList = buildScriptListTable()
  const featureList = buildFeatureListTable()
  const pairPos = buildPairPosSubtable(entries)
  const lookupList = buildLookupListTable(buildLookupTable(pairPos))

  const headerSize = 10
  const scriptListOffset = headerSize
  const featureListOffset = scriptListOffset + scriptList.length
  const lookupListOffset = featureListOffset + featureList.length

  const header = concatBytes(
    u16(1), u16(0), // version 1.0
    u16(scriptListOffset),
    u16(featureListOffset),
    u16(lookupListOffset),
  )
  return concatBytes(header, scriptList, featureList, lookupList)
}

/**
 * Build an opentype.Font from the glyph data returned by generateFontGlyphData.
 * Coordinates from the F# generator are already Y-up (baseline at 0, positive
 * values go up), which matches the opentype.js coordinate convention directly.
 * All geometry is scaled from the generator's variable em to EXPORT_UPM.
 */
export function buildFont(glyphData, familyName = 'Dactyl', styleName = 'Regular') {
  const { glyphs: glyphsData, ascender, descender, unitsPerEm, kerningPairs } = glyphData

  // Map generator units → the fixed 1000-unit export em.
  const scale = EXPORT_UPM / unitsPerEm
  const s = (n) => Math.round(n * scale)

  const charGlyphs = glyphsData
    .filter(g => g.unicode >= 32 && (g.pathData || g.unicode === 32))
    .sort((a, b) => a.unicode - b.unicode)
    .filter((g, i, arr) => i === 0 || g.unicode !== arr[i - 1].unicode)
    .map(g => {
      const path = new opentype.Path()
      for (const cmd of unionPath(g.pathData)) {
        switch (cmd.type) {
          case 'M': path.moveTo(s(cmd.x), s(cmd.y)); break
          case 'L': path.lineTo(s(cmd.x), s(cmd.y)); break
          case 'C': path.bezierCurveTo(s(cmd.x1), s(cmd.y1), s(cmd.x2), s(cmd.y2), s(cmd.x), s(cmd.y)); break
          case 'Z': path.close(); break
        }
      }
      return new opentype.Glyph({
        name: `uni${g.unicode.toString(16).padStart(4, '0')}`,
        unicode: g.unicode,
        advanceWidth: s(g.advanceWidth),
        path,
      })
    })

  // U+00A0 no-break space: required whitespace glyph, drawn as an empty outline
  // with the same advance as the regular space (U+0020) so the two match.
  const space = charGlyphs.find(g => g.unicode === 32)
  if (space) {
    charGlyphs.push(new opentype.Glyph({
      name: 'uni00a0',
      unicode: 0x00a0,
      advanceWidth: space.advanceWidth,
      path: new opentype.Path(),
    }))
  }

  // opentype.js derives hhea.advanceWidthMax from the character glyphs, so give
  // .notdef the same max advance rather than a wider one that would make hmtx
  // and hhea disagree.
  const maxAdvance = charGlyphs.reduce((m, g) => Math.max(m, g.advanceWidth), 0)
  const notdef = new opentype.Glyph({
    name: '.notdef',
    advanceWidth: maxAdvance,
    path: notdefPath(),
  })

  const glyphs = [notdef, ...charGlyphs]

  const font = new opentype.Font({
    familyName,
    styleName,
    unitsPerEm: EXPORT_UPM,
    ascender: s(ascender),
    descender: s(descender),
    version: FONT_VERSION,
    copyright: COPYRIGHT,
    designer: 'Terry Spitz',
    // Points at the live Explorer site; the source repo is in the copyright.
    designerURL: 'https://terryspitz.github.io/dactyl-font',
    manufacturer: 'Terry Spitz',
    license: 'This Font Software is licensed under the SIL Open Font License, Version 1.1.',
    licenseURL: 'https://openfontlicense.org',
    glyphs,
  })

  // Stash the pair kerns as `{ "leftGlyphIndex,rightGlyphIndex": value }` on
  // the Font object — keyed by glyph index via charToGlyphIndex, dropping
  // pairs whose chars aren't in the font. This does NOT reach the compiled
  // binary (opentype.js has no `kern`-table writer); callers must pass
  // font.kerningPairs to injectKernTable after calling toArrayBuffer().
  if (kerningPairs && kerningPairs.length) {
    const pairs = {}
    for (const kp of kerningPairs) {
      const li = font.charToGlyphIndex(String.fromCodePoint(kp.left))
      const ri = font.charToGlyphIndex(String.fromCodePoint(kp.right))
      if (li > 0 && ri > 0) pairs[`${li},${ri}`] = s(kp.value)
    }
    font.kerningPairs = pairs
  }

  // opentype.js 1.3.4 fills every unset name field with a single space, which
  // fontbakery flags as empty / trailing-whitespace records.  Drop the ones we
  // don't populate, and set explicit version and unique-id strings (the latter
  // is otherwise built from the now-removed manufacturer field).
  for (const key of ['manufacturerURL', 'description', 'trademark']) delete font.names[key]
  font.names.version = { en: VERSION_STRING }
  font.names.uniqueID = { en: `1.000;Terry Spitz;${familyName}-${styleName}` }

  return font
}

/**
 * Build a font from the glyph data and return it as an OTF data URL suitable
 * for a CSS @font-face src declaration.
 */
export function buildFontDataUrl(glyphData, familyName = 'DactylPreview') {
  const font = buildFont(glyphData, familyName)
  const buffer = injectKerningTables(font.toArrayBuffer(), font.kerningPairs)
  const bytes = new Uint8Array(buffer)
  let binary = ''
  for (let i = 0; i < bytes.byteLength; i++) binary += String.fromCharCode(bytes[i])
  return 'data:font/otf;base64,' + btoa(binary)
}

/**
 * Build a font from the glyph data and trigger a browser download of the OTF file.
 * Pass axes and defaultAxes to embed overridden axis values in the filename and style name.
 */
export function downloadFont(glyphData, axes, defaultAxes, glyphSeed = null) {
  const overrides = axes && defaultAxes ? getOverriddenAxes(axes, defaultAxes) : []
  const styleName = buildStyleName(overrides, glyphSeed)
  const filename = buildFilename(overrides, glyphSeed)
  const font = buildFont(glyphData, 'Dactyl', styleName)
  const buffer = injectKerningTables(font.toArrayBuffer(), font.kerningPairs)
  const blob = new Blob([buffer], { type: 'font/otf' })
  const url = URL.createObjectURL(blob)
  const a = Object.assign(document.createElement('a'), { href: url, download: filename })
  a.click()
  URL.revokeObjectURL(url)
}
