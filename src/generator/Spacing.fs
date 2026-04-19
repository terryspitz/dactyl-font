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
        ('c', 'l'), -10
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
        ('i', 'n'), 8
        ('i', 'm'), 8
        ('i', 'l'), 6
        ('i', 'h'), 8
        ('i', 'd'), 8
        ('i', 'u'), 8
        ('i', 'i'), 6
        ('n', 'i'), 8
        ('n', 'u'), 10
        ('n', 'h'), 8
        ('n', 'd'), 10
        ('n', 'n'), 10
        ('n', 'm'), 10
        ('n', 'l'), 6
        ('m', 'm'), 10
        ('m', 'n'), 10
        ('m', 'u'), 10
        ('m', 'i'), 8
        ('m', 'h'), 8
        ('m', 'l'), 6
        ('u', 'n'), 10
        ('u', 'm'), 10
        ('u', 'i'), 6
        ('u', 'l'), 6
        ('u', 'h'), 6
        ('u', 'u'), 8
        ('d', 'n'), 8
        ('d', 'm'), 8
        ('d', 'u'), 6
        ('d', 'i'), 6
        ('d', 'h'), 6
        ('d', 'd'), 8
        ('h', 'i'), 8
        ('h', 'n'), 8
        ('h', 'm'), 8
        ('h', 'u'), 8
        ('h', 'l'), 6
        ('l', 'i'), 6
        ('l', 'l'), 6
        ('l', 'u'), 6
        ('l', 'n'), 6
        ('b', 'i'), 6
        ('b', 'u'), 6
        ('b', 'l'), 6
    ]

/// Kern value for a glyph pair, in glyph coord units. Returns 0 when the pair
/// is absent from the override map. Optical kerning is a no-op for now and
/// will plug in here once GlyphProfile lands.
let pairKernInt (a: char) (b: char) : int =
    Map.tryFind (a, b) kerningOverrides |> Option.defaultValue 0

let pairKern (a: char) (b: char) : float =
    float (pairKernInt a b)
