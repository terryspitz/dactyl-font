import uppercase from './proofs/uppercase.txt?raw'
import lowercase from './proofs/lowercase.txt?raw'
import mixed from './proofs/mixed.txt?raw'

const stripComments = (s) =>
  s.split('\n').filter((l) => !l.trim().startsWith('#')).join('\n').trim()

export const proofTexts = {
  uppercase: stripComments(uppercase),
  lowercase: stripComments(lowercase),
  mixed: stripComments(mixed),
}

export const proofLabels = {
  uppercase: 'Uppercase',
  lowercase: 'Lowercase',
  mixed: 'Mixed (small caps)',
}

export const proofCases = ['uppercase', 'lowercase', 'mixed']
