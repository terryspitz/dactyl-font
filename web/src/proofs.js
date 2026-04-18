import uppercase from './proofs/uppercase.txt?raw'
import lowercase from './proofs/lowercase.txt?raw'

const stripComments = (s) =>
  s.split('\n').filter((l) => !l.trim().startsWith('#')).join('\n').trim()

export const proofTexts = {
  uppercase: stripComments(uppercase),
  lowercase: stripComments(lowercase),
}

export const proofLabels = {
  uppercase: 'Uppercase',
  lowercase: 'Lowercase',
}

export const proofCases = ['uppercase', 'lowercase']
