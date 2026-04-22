import uppercase from './proofs/uppercase.txt?raw'
import lowercase from './proofs/lowercase.txt?raw'
import { allChars } from './lib/fable/Api'

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

export const proofTexts = {
  uppercase: wrap(stripComments(uppercase), WRAP_WIDTH),
  lowercase: wrap(stripComments(lowercase), WRAP_WIDTH),
  alphabet: allChars,
}

export const proofLabels = {
  uppercase: 'Uppercase',
  lowercase: 'Lowercase',
  alphabet: 'Alphabet',
}

export const proofCases = ['lowercase', 'uppercase', 'alphabet']

export { classicBooks } from './proofs/books'
