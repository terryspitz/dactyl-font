// Kerning and sidebearing helpers. Keep logic here so Font.fs stays focused on
// glyph geometry and outline generation.
//
// Manual pair overrides are empty for now — kerning is driven entirely by
// GlyphProfile's outline-sampled optical kerning plus Axes.sidebearingScale.
// This map is where a small hand-tuned exception list would go for pairs
// that still look wrong after reviewing optical's own output.
module Spacing

/// Kerning pair overrides, in glyph coordinate units. Negative values pull
/// the second glyph closer to the first; positive values push it further.
let kerningOverrides : Map<char * char, int> = Map.empty

/// Kern value for a glyph pair, in glyph coord units. Returns 0 when the pair
/// is absent from the override map (currently always, since the map is empty).
let pairKernInt (a: char) (b: char) : int =
    Map.tryFind (a, b) kerningOverrides |> Option.defaultValue 0

let pairKern (a: char) (b: char) : float =
    float (pairKernInt a b)
