// Kerning and sidebearing helpers. Keep logic here so Font.fs stays focused on
// glyph geometry and outline generation.
//
// The design is intentionally minimal in this first cut: only a manual pair-
// kern override map. Bezier-sampled optical kerning is planned on top of this
// (see Axes.opticalKerning) and will live in a follow-up GlyphProfile module.
module Spacing

/// Kerning pair overrides, in glyph coordinate units. Negative values pull
/// the second glyph closer to the first; positive values push it further.
/// Values are conservative starting points chosen by eye on the default
/// Dactyl axes; expect to retune as the bezier profile sampler lands.
let kerningOverrides : Map<char * char, int> =
    Map.ofList [
        // Upper-case diagonals against straight / round neighbours.
        ('A', 'V'), -50
        ('A', 'W'), -50
        ('A', 'Y'), -50
        ('A', 'T'), -45
        ('F', 'A'), -40
        ('L', 'T'), -45
        ('L', 'V'), -40
        ('L', 'W'), -40
        ('L', 'Y'), -40
        ('P', 'A'), -35
        ('R', 'V'), -25
        ('R', 'W'), -25
        ('R', 'Y'), -25
        ('T', 'A'), -45
        ('T', 'a'), -40
        ('T', 'c'), -40
        ('T', 'e'), -40
        ('T', 'o'), -40
        ('T', 'y'), -30
        ('V', 'A'), -50
        ('V', 'a'), -30
        ('V', 'o'), -25
        ('W', 'A'), -50
        ('W', 'a'), -30
        ('W', 'o'), -25
        ('Y', 'A'), -50
        ('Y', 'a'), -35
        ('Y', 'o'), -30
        // Lower-case pairs with notorious spacing artefacts.
        ('f', 'f'), -10
        ('f', 'i'), -15
        ('f', 'l'), -15
        ('f', 'j'), -20
        ('r', 'n'), -10
        ('c', 'l'), -5
    ]

/// Kern value for a glyph pair, in glyph coord units. Returns 0 when the pair
/// is absent from the override map. Optical kerning is a no-op for now and
/// will plug in here once GlyphProfile lands.
let pairKernInt (a: char) (b: char) : int =
    Map.tryFind (a, b) kerningOverrides |> Option.defaultValue 0

let pairKern (a: char) (b: char) : float =
    float (pairKernInt a b)
