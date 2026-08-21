import uppercase from './proofs/uppercase.txt?raw'
import lowercase from './proofs/lowercase.txt?raw'
import { allChars, alphabetChars } from './lib/fable/Api'

const stripComments = (s) =>
  s.split('\n').filter((l) => !l.trim().startsWith('#')).join('\n').trim()

const wrap = (s, width) => {
  const out = []
  for (const para of s.split('\n')) {
    if (para.length <= width) { out.push(para); continue }
    const words = para.split(/\s+/)
    let line = ''
    for (const w of words) {
      if (line && line.length + 1 + w.length > width) {
        out.push(line)
        line = w
      } else {
        line = line ? line + ' ' + w : w
      }
    }
    if (line) out.push(line)
  }
  return out.join('\n')
}

const WRAP_WIDTH = 80

// "Spacing Strings" technique: https://www.a-plus-type.com/writing/spacing-strings
// A fixed set of control strings (n/o for lowercase, H/O for uppercase), followed
// by two templated strings per glyph that sandwich it between straight (n/H) and
// round (o/O) neighbours, doubled up for interior straight-to-straight and
// round-to-round comparisons.
const SPACING_CONTROL_STRINGS = [
  'nnnnnnnn',
  'nnnnononoooo',
  'HHHOHOHOOO',
  'HHnnHOHOnnOO',
  'HHooHOHOooOO',
  'nnHHnonoHHoo',
  'nnOOnonoOOoo',
]

const spacingStringsForGlyph = (c) =>
  `${c} nn${c}nonouu${c}nono${c}oo HH${c}HOHO${c}OO`

const spacingStringsText = [
  ...SPACING_CONTROL_STRINGS,
  '',
  ...Array.from(alphabetChars.replace(/\n/g, '')).map(spacingStringsForGlyph),
].join('\n')

export const proofTexts = {
  uppercase: wrap(stripComments(uppercase), WRAP_WIDTH),
  lowercase: wrap(stripComments(lowercase), WRAP_WIDTH),
  alphabet: alphabetChars,
  allGlyphs: allChars,
  spacingStrings: spacingStringsText,
}

export const proofLabels = {
  uppercase: 'Uppercase',
  lowercase: 'Lowercase',
  alphabet: 'Alphabet',
  allGlyphs: 'All glyphs',
  spacingStrings: 'Spacing Strings',
}

export const proofCases = ['lowercase', 'uppercase', 'alphabet', 'allGlyphs', 'spacingStrings']

export { classicBooks } from './proofs/books'
