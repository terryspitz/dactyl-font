// Kerning and sidebearing helpers. Keep logic here so Font.fs stays focused on
// glyph geometry and outline generation.
//
// The design is intentionally minimal in this first cut: only a manual pair-
// kern override map. Bezier-sampled optical kerning is planned on top of this
// (see Axes.opticalKerning) and will live in a follow-up GlyphProfile module.
module Spacing

/// Kerning pair overrides, in glyph coordinate units. Negative values pull
/// the second glyph closer to the first; positive values push it further.
/// Values are hand-tuned on the default Dactyl axes (thickness ~30) against
/// the Hoefler uppercase + lowercase proofs. Expect to retune once the
/// bezier profile sampler lands.
let kerningOverrides : Map<char * char, int> =
    Map.ofList [
        // ---------------- UPPERCASE: tighten diagonals ----------------
        ('A', 'V'), -75
        ('A', 'W'), -75
        ('A', 'Y'), -75
        ('A', 'T'), -65
        ('A', 'C'), -25
        ('A', 'G'), -25
        ('A', 'O'), -20
        ('A', 'Q'), -20
        ('F', 'A'), -60
        ('F', 'o'), -25
        ('F', 'e'), -25
        ('F', 'i'), -15
        ('L', 'T'), -70
        ('L', 'V'), -60
        ('L', 'W'), -60
        ('L', 'Y'), -60
        ('L', 'O'), -20
        ('L', 'U'), -15
        ('P', 'A'), -55
        ('P', 'o'), -20
        ('P', 'e'), -20
        ('R', 'V'), -40
        ('R', 'W'), -40
        ('R', 'Y'), -40
        ('R', 'T'), -25
        ('T', 'A'), -65
        ('T', 'a'), -55
        ('T', 'c'), -55
        ('T', 'e'), -55
        ('T', 'o'), -55
        ('T', 'r'), -45
        ('T', 's'), -45
        ('T', 'u'), -40
        ('T', 'w'), -40
        ('T', 'y'), -45
        ('V', 'A'), -75
        ('V', 'a'), -45
        ('V', 'e'), -40
        ('V', 'i'), -15
        ('V', 'o'), -40
        ('V', 'r'), -35
        ('V', 'u'), -30
        ('V', 'y'), -30
        ('W', 'A'), -75
        ('W', 'a'), -45
        ('W', 'e'), -40
        ('W', 'i'), -15
        ('W', 'o'), -40
        ('W', 'r'), -35
        ('W', 'u'), -30
        ('W', 'y'), -30
        ('Y', 'A'), -75
        ('Y', 'a'), -55
        ('Y', 'e'), -50
        ('Y', 'i'), -20
        ('Y', 'o'), -50
        ('Y', 'p'), -35
        ('Y', 'u'), -35
        ('K', 'e'), -20
        ('K', 'o'), -20
        ('K', 'u'), -15

        // -------- UPPERCASE: open flat-sided slab pairs --------
        ('I', 'N'), 15
        ('I', 'M'), 15
        ('I', 'H'), 12
        ('I', 'E'), 10
        ('I', 'D'), 15
        ('I', 'U'), 15
        ('N', 'I'), 15
        ('N', 'U'), 20
        ('N', 'H'), 15
        ('N', 'D'), 18
        ('N', 'E'), 15
        ('N', 'M'), 18
        ('N', 'N'), 18
        ('M', 'M'), 20
        ('M', 'N'), 18
        ('M', 'U'), 20
        ('M', 'I'), 15
        ('M', 'H'), 15
        ('U', 'N'), 18
        ('U', 'M'), 20
        ('U', 'I'), 12
        ('U', 'H'), 12
        ('U', 'U'), 15
        ('D', 'N'), 15
        ('D', 'M'), 15
        ('D', 'U'), 12
        ('D', 'I'), 12
        ('D', 'D'), 18
        ('D', 'H'), 12
        ('H', 'I'), 12
        ('H', 'N'), 15
        ('H', 'M'), 15
        ('H', 'U'), 15
        ('H', 'H'), 15
        ('H', 'D'), 12
        ('E', 'N'), 12
        ('E', 'M'), 12
        ('E', 'I'), 10
        ('B', 'I'), 10
        ('B', 'U'), 10
        ('B', 'D'), 12

        // ---------------- LOWERCASE: tighten existing pairs ----------------
        ('f', 'f'), -20
        ('f', 'i'), -25
        ('f', 'l'), -25
        ('f', 'j'), -30
        ('f', 'a'), -15
        ('f', 'e'), -15
        ('f', 'o'), -15
        ('r', 'n'), -18
        ('r', 'm'), -18
        ('r', 'u'), -15
        ('r', 'v'), -15
        ('r', 'w'), -15
        ('r', 'y'), -15
        ('r', 'a'), -8
        ('c', 'k'), -8
        ('v', 'a'), -12
        ('v', 'e'), -12
        ('v', 'o'), -12
        ('w', 'a'), -12
        ('w', 'e'), -12
        ('w', 'o'), -12
        ('y', 'a'), -12
        ('y', 'e'), -12
        ('y', 'o'), -12

        // -------- LOWERCASE: open flat-sided pairs (din, Num, etc.) --------
        ('i', 'n'), 14
        ('i', 'm'), 14
        ('i', 'h'), 12
        ('i', 'd'), 12
        ('i', 'u'), 10
        ('i', 'i'), 8
        ('n', 'i'), 10
        ('n', 'u'), 12
        ('n', 'h'), 10
        ('n', 'd'), 12
        ('n', 'n'), 12
        ('n', 'm'), 12
        ('m', 'm'), 12
        ('m', 'n'), 12
        ('m', 'u'), 12
        ('m', 'i'), 10
        ('m', 'h'), 10
        ('u', 'n'), 12
        ('u', 'm'), 12
        ('u', 'i'), 8
        ('u', 'h'), 8
        ('u', 'u'), 10
        ('d', 'n'), 12
        ('d', 'm'), 12
        ('d', 'u'), 10
        ('d', 'i'), 14
        ('d', 'h'), 14
        ('d', 'd'), 12
        ('d', 'b'), 12
        ('h', 'i'), 10
        ('h', 'n'), 10
        ('h', 'm'), 10
        ('h', 'u'), 10
        ('b', 'i'), 10
        ('b', 'u'), 10

        // -------- LOWERCASE: left of 'l' needs more room from almost everything --------
        ('a', 'l'), 12
        ('b', 'l'), 14
        ('c', 'l'), 12
        ('d', 'l'), 14
        ('e', 'l'), 12
        ('g', 'l'), 12
        ('h', 'l'), 14
        ('i', 'l'), 14
        ('k', 'l'), 14
        ('m', 'l'), 14
        ('n', 'l'), 14
        ('o', 'l'), 12
        ('p', 'l'), 14
        ('q', 'l'), 12
        ('r', 'l'), 10
        ('s', 'l'), 10
        ('t', 'l'), 10
        ('u', 'l'), 12
        ('v', 'l'), 8
        ('w', 'l'), 8
        ('x', 'l'), 12
        ('y', 'l'), 8
        ('z', 'l'), 12

        // -------- LOWERCASE: right of 'l' — less room (flip to tight / negative) --------
        ('l', 'a'), -8
        ('l', 'c'), -8
        ('l', 'd'), -6
        ('l', 'e'), -8
        ('l', 'g'), -8
        ('l', 'i'), -4
        ('l', 'l'), -2
        ('l', 'n'), -6
        ('l', 'o'), -8
        ('l', 'q'), -8
        ('l', 's'), -6
        ('l', 'u'), -6
        ('l', 'y'), -6

        // -------- LOWERCASE: left of 'j' — tighter against non-descenders --------
        ('a', 'j'), -15
        ('b', 'j'), -15
        ('c', 'j'), -15
        ('d', 'j'), -15
        ('e', 'j'), -15
        ('h', 'j'), -15
        ('i', 'j'), -15
        ('k', 'j'), -15
        ('l', 'j'), -15
        ('m', 'j'), -15
        ('n', 'j'), -15
        ('o', 'j'), -15
        ('r', 'j'), -15
        ('s', 'j'), -15
        ('t', 'j'), -15
        ('u', 'j'), -15
        ('v', 'j'), -12
        ('w', 'j'), -12
        ('x', 'j'), -15
        ('z', 'j'), -15
    ]

/// Kern value for a glyph pair, in glyph coord units. Returns 0 when the pair
/// is absent from the override map. Optical kerning is a no-op for now and
/// will plug in here once GlyphProfile lands.
let pairKernInt (a: char) (b: char) : int =
    Map.tryFind (a, b) kerningOverrides |> Option.defaultValue 0

let pairKern (a: char) (b: char) : float =
    float (pairKernInt a b)
