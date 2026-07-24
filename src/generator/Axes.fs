module Axes

/// UI element type for an axis
type Controls =
    | Range of from: int * upto: int
    | FracRange of from: float * upto: float
    | Checkbox

/// A named instance ("fallback") on an axis, mirroring the repeated `fallback`
/// blocks of a Google Fonts axis-registry entry.
type AxisFallback = { name: string; value: float }

/// Google Fonts axis-registry metadata for a control, mirroring the fields of a
/// registry `.textproto` entry:
/// https://github.com/googlefonts/axisregistry/tree/main/Lib/axisregistry/data
///
/// NOTE: `minValue`/`defaultValue`/`maxValue` are in the *standard variable-font
/// designspace* as published in the registry (e.g. wght 1..1000, wdth 25..200),
/// NOT the generator's internal geometry units used by the UI sliders in
/// `Axes.controls`.  They describe how a control maps onto a registered
/// OpenType variation axis for labelling/export, and deliberately do not
/// influence glyph generation.
type Registry =
    { tag: string //4-char OpenType axis tag (registry `tag`)
      displayName: string //registry `display_name`
      minValue: float //registry `min_value`
      defaultValue: float //registry `default_value`
      maxValue: float //registry `max_value`
      precision: int //registry `precision`
      fallbackOnly: bool //registry `fallback_only`
      description: string //registry `description`
      fallbacks: AxisFallback list } //registry repeated `fallback` blocks

// Variable which define the font characteristics (named after Variable Font terminology)
type Axes =
    { dactyl_spline: bool //use new dactyl splines with new glyph definitions
      spline2: bool //use Raph Levian's new spline-research splines, vs. his original spiro splines
      constraints: bool //constrain tangents to within borders
      width: int //width of normal glyph
      height: int //capital height
      x_height: float //height of lower case as a fraction of capitals
      descender_depth: float //depth of descenders below the baseline, as a fraction of capital height
      weight: int //stroke width (GF Weight axis, wght)
      contrast: float //make vertical lines thicker
      roundedness: int //roundedness
      softness: float //radius of rounding applied at angled corners (0=sharp, 1=max) (GF Softness axis, SOFT)
      // overshoot : int          //curves are larger by this amount to compensate for looking smaller
      spacing: int //gap between glyphs (GF Spacing axis, SPAC)
      leading: int //gap between lines
      monospace: float //fraction to interpolate widths to monospaces
      italic: float //fraction to sheer glyphs
      alt_a_g: bool //use two-storey alternate shapes for 'a' and 'g'
      serif: int //serif size
      end_bulb: float //fraction of thickness to apply curves to endcaps
      flare: float //end caps expand by this amount
      axis_align_caps: bool //round angle of caps to horizontal/vertical
      //spine : bool              //show the single width glyph, use with outline off or filled off
      filled: bool //(svg only) filled or empty outlines
      outline: bool //use thickness to expand stroke width
      stroked: bool //each stroke is 4 parallel lines
      scratches: bool //horror/paint strokes font
      nib: float //broad-nib pen: stroke width follows stroke direction (0=off, 1=full nib effect)
      nib_angle: int //nib angle in degrees anticlockwise from horizontal
      taper: float //strokes taper to points at their ends (0=off, 1=pointed all the way to the middle)
      taper_end: float //width at the tapered ends as a fraction of full width (0=sharp point, 1=no narrowing)
      wobble: float //hand-drawn waviness: spine displacement amplitude in units of thickness (0=off)
      roughness: float //random width jitter along the stroke edge, independent per side (0=off)
      mobius: float //strokes are twisting ribbons pinched where edge-on; half-twist density (0=off, 1 ≈ every 300 units)
      constant_offset: bool //prototype: outlines are dense polylines at constant perpendicular distance from the spine
      max_spline_iter: int //max number of iterations to solve spline curves
      show_knots: bool //show small circles for the points used to define lines/curves
      show_tangents: bool //show lines for the tangents at each knot
      joints: bool //check joints to turn off serifs
      smooth: bool //no corners
      clip_rect: bool //clip each glyph to it's bounding rect (helps with degenerate curves)
      flatness: float //weight of flatness (abs m) in objective function
      end_flatness: float //quadratic curvature-span weight for open-curve endpoint segments (higher = more circular arc at stroke tips)
      debug: bool } //show debug info in console

    static member DefaultAxes =
        { dactyl_spline = true
          spline2 = false
          width = 300
          height = 600
          x_height = 0.6
          descender_depth = 0.5
          weight = 30
          contrast = 0.05
          roundedness = 60
          softness = 0.0
          spacing = 40
          leading = 50
          monospace = 0.0
          italic = 0.0
          alt_a_g = false
          serif = 0
          end_bulb = 0.0
          flare = 0.0
          axis_align_caps = true
          filled = true
          outline = true
          stroked = false
          scratches = false
          nib = 0.0
          nib_angle = 30
          taper = 0.0
          taper_end = 0.5
          wobble = 0.0
          roughness = 0.0
          mobius = 0.0
          constant_offset = true
          max_spline_iter = 500
          show_knots = false
          show_tangents = false
          joints = true
          constraints = false
          smooth = false
          clip_rect = true
          flatness = 0.5
          end_flatness = 10.0
          debug = false }

    static member controls =
        [ "dactyl_spline", Checkbox, "experimental", "Use new dactyl splines with new glyph definitions"
          "spline2", Checkbox, "experimental", "Use Raph Levien's new spline-research splines, vs. his original spiro splines"
          "width", Range(100, 1000), "backbone", "Width of normal glyph"
          "height", Range(100, 1000), "backbone", "Capital height"
          "x_height", FracRange(0.2, 1.1), "backbone", "Height of lower case as a fraction of capitals"
          "descender_depth", FracRange(0.2, 1.0), "backbone", "Depth of descenders below the baseline, as a fraction of capital height"
          "spacing", Range(0, 200), "backbone", "Gap between glyphs"
          "leading", Range(-100, 200), "backbone", "Gap between lines"
          "monospace", FracRange(0.0, 1.0), "backbone", "Fraction to interpolate widths to monospace"
          "italic", FracRange(0.0, 1.0), "backbone", "Fraction to shear glyphs"
          "alt_a_g", Checkbox, "backbone", "Use two-storey alternate shapes for 'a' and 'g'"
          "roundedness", Range(0, 100), "backbone", "Roundedness"
          "weight", Range(1, 200), "outline", "Stroke width"
          "contrast", FracRange(-0.5, 0.5), "outline", "Make vertical lines thicker"
          "softness", FracRange(0.0, 1.0), "outline", "Radius of rounding applied at angled corners (0=sharp, 1=max)"
          "axis_align_caps", Checkbox, "outline", "Round angle of caps to horizontal/vertical"
          "outline", Checkbox, "outline", "Use thickness to expand stroke width"
          "filled", Checkbox, "outline", "(SVG only) filled or empty outlines"
          "smooth", Checkbox, "experimental", "No corners"
          "end_bulb", FracRange(-1.0, 3.0), "artistic", "Fraction of thickness to apply curves to endcaps"
          "flare", FracRange(-1.0, 1.0), "artistic", "End caps expand by this amount"
          "stroked", Checkbox, "artistic", "Each stroke is 4 parallel lines"
          "scratches", Checkbox, "artistic", "Horror/paint strokes font"
          "nib", FracRange(0.0, 1.0), "artistic", "Broad-nib pen: stroke width follows stroke direction (0=off, 1=full nib effect)"
          "nib_angle", Range(0, 180), "artistic", "Nib angle in degrees anticlockwise from horizontal"
          "taper", FracRange(0.0, 1.0), "artistic", "Strokes taper to points at their ends (0=off, 1=pointed all the way to the middle)"
          "taper_end", FracRange(0.0, 1.0), "artistic", "Width at the tapered ends as a fraction of full width (0=sharp point, 1=no narrowing)"
          "wobble", FracRange(0.0, 1.0), "artistic", "Hand-drawn waviness: spine displacement amplitude in units of thickness (0=off)"
          "roughness", FracRange(0.0, 1.0), "artistic", "Random width jitter along the stroke edge, independent per side (0=off)"
          "mobius", FracRange(0.0, 3.0), "artistic", "Strokes are twisting ribbons pinched where edge-on; half-twist density (0=off, 1 ≈ every 300 units)"
          "serif", Range(0, 70), "artistic", "Serif size"
          "constraints", Checkbox, "experimental", "Constrain tangents to within borders"
          "constant_offset", Checkbox, "experimental", "Prototype: outlines are dense polylines at constant perpendicular distance from the spine"
          "max_spline_iter", Range(0, 200), "experimental", "Max number of iterations to solve spline curves"
          "show_knots", Checkbox, "debug", "Show small circles for the points used to define lines/curves"
          "show_tangents", Checkbox, "debug", "Show lines for the tangents at each knot"
          "joints", Checkbox, "debug", "Check joints to turn off serifs"
          "clip_rect", Checkbox, "debug", "Clip each glyph to its bounding rect (helps with degenerate curves)"
          "flatness", FracRange(0.0, 10.0), "experimental", "Weight of flatness (abs m) in objective function"
          "end_flatness", FracRange(0.0, 30.0), "experimental", "Quadratic curvature-span weight for open-curve endpoint segments (higher = more circular arc at stroke tips)"
          "debug", Checkbox, "debug", "Show debug info in console" ]

    /// Reconciliation of dactyl controls against the Google Fonts axis registry
    /// (https://github.com/googlefonts/axisregistry).  Keyed by control name,
    /// this holds only the controls that correspond to a *registered* GF axis;
    /// each entry carries the registry's canonical tag, name, designspace
    /// range, precision, fallback_only flag, description and named instances.
    /// Every other control uses a private-use tag (see `privateTags`/`axisTag`).
    static member registry: Map<string, Registry> =
        Map [ "weight", // stroke width -> Weight
                  { tag = "wght"
                    displayName = "Weight"
                    minValue = 1.0
                    defaultValue = 400.0
                    maxValue = 1000.0
                    precision = 0
                    fallbackOnly = false
                    description = """Adjust the style from lighter to bolder in typographic color, by varying stroke weights, spacing and kerning, and other aspects of the type. This typically changes overall width, and so may be used in conjunction with Width and Grade axes."""
                    fallbacks =
                      [ { name = "Thin"; value = 100.0 }
                        { name = "ExtraLight"; value = 200.0 }
                        { name = "Light"; value = 300.0 }
                        { name = "Regular"; value = 400.0 }
                        { name = "Medium"; value = 500.0 }
                        { name = "SemiBold"; value = 600.0 }
                        { name = "Bold"; value = 700.0 }
                        { name = "ExtraBold"; value = 800.0 }
                        { name = "Black"; value = 900.0 } ] }
              "width", // glyph width -> Width
                  { tag = "wdth"
                    displayName = "Width"
                    minValue = 25.0
                    defaultValue = 100.0
                    maxValue = 200.0
                    precision = -1
                    fallbackOnly = false
                    description = """Adjust the style from narrower to wider, by varying the proportions of counters, strokes, spacing and kerning, and other aspects of the type. This typically changes the typographic color in a subtle way, and so may be used in conjunction with Weight and Grade axes."""
                    fallbacks =
                      [ { name = "SuperCondensed"; value = 25.0 }
                        { name = "UltraCondensed"; value = 50.0 }
                        { name = "ExtraCondensed"; value = 62.5 }
                        { name = "Condensed"; value = 75.0 }
                        { name = "SemiCondensed"; value = 87.5 }
                        { name = "Normal"; value = 100.0 }
                        { name = "SemiExpanded"; value = 112.5 }
                        { name = "Expanded"; value = 125.0 }
                        { name = "ExtraExpanded"; value = 150.0 }
                        { name = "UltraExpanded"; value = 200.0 } ] }
              "italic", // shear/oblique -> Slant
                  { tag = "slnt"
                    displayName = "Slant"
                    minValue = -90.0
                    defaultValue = 0.0
                    maxValue = 90.0
                    precision = 0
                    fallbackOnly = false
                    description = """Adjust the style from upright to slanted. Negative values produce right-leaning forms, also known to typographers as an 'oblique' style. Positive values produce left-leaning forms, also called a 'backslanted' or 'reverse oblique' style."""
                    fallbacks = [ { name = "Default"; value = 0.0 } ] }
              "contrast", // vertical/horizontal stroke ratio -> Contrast
                  { tag = "CTRS"
                    displayName = "Contrast"
                    minValue = -100.0
                    defaultValue = 0.0
                    maxValue = 100.0
                    precision = 1
                    fallbackOnly = false
                    description = """Contrast describes the stroke width difference between the thick and thin parts of the font glyphs. A value of zero indicates no visible/apparent contrast. A positive number indicates an increase in contrast relative to the zero-contrast thickness, achieved by making the thin stroke thinner. A value of 100 indicates that the thin stroke has disappeared completely. A negative value indicates "reverse contrast": the strokes which would conventionally be thick in the writing system are instead made thinner. In western-language fonts this might be perceived as a 19th-century, "circus" or "old West" effect. A value of -100 indicates that the strokes which would normally be thick have disappeared completely."""
                    fallbacks =
                      [ { name = "Reversed"; value = -100.0 }
                        { name = "None"; value = 0.0 }
                        { name = "High"; value = 100.0 } ] }
              "roundedness", // corner roundness -> Roundness
                  { tag = "ROND"
                    displayName = "Roundness"
                    minValue = 0.0
                    defaultValue = 0.0
                    maxValue = 100.0
                    precision = 0
                    fallbackOnly = false
                    description = """Adjust shapes from angular defaults (0%) to become increasingly rounded (up to 100%)."""
                    fallbacks = [ { name = "Default"; value = 0.0 } ] }
              "softness", // corner softening -> Softness
                  { tag = "SOFT"
                    displayName = "Softness"
                    minValue = 0.0
                    defaultValue = 0.0
                    maxValue = 100.0
                    precision = -1
                    fallbackOnly = false
                    description = """Adjust letterforms to become more and more soft and rounded."""
                    fallbacks =
                      [ { name = "Sharp"; value = 0.0 }
                        { name = "Soft"; value = 50.0 }
                        { name = "SuperSoft"; value = 100.0 } ] }
              "monospace", // proportional<->fixed width -> Monospace
                  { tag = "MONO"
                    displayName = "Monospace"
                    minValue = 0.0
                    defaultValue = 0.0
                    maxValue = 1.0
                    precision = -2
                    fallbackOnly = false
                    description = """Adjust the style from Proportional (natural widths, default) to Monospace (fixed width). With proportional spacing, each glyph takes up a unique amount of space on a line, while monospace is when all glyphs have the same total character width."""
                    fallbacks =
                      [ { name = "Proportional"; value = 0.0 }
                        { name = "Monospace"; value = 1.0 } ] }
              "flare", // terminal swelling -> Flare
                  { tag = "FLAR"
                    displayName = "Flare"
                    minValue = 0.0
                    defaultValue = 0.0
                    maxValue = 100.0
                    precision = 0
                    fallbackOnly = false
                    description = """As the flare axis grows, the stem terminals go from straight (0%) to develop a swelling (100%)."""
                    fallbacks = [ { name = "Default"; value = 0.0 } ] }
              "filled", // empty<->filled outlines -> Fill
                  { tag = "FILL"
                    displayName = "Fill"
                    minValue = 0.0
                    defaultValue = 0.0
                    maxValue = 1.0
                    precision = -2
                    fallbackOnly = false
                    description = """Fill in transparent forms with opaque ones. Sometimes interior opaque forms become transparent, to maintain contrasting shapes. This can be useful in animation or interaction to convey a state transition. Ranges from 0 (no treatment) to 1 (completely filled)."""
                    fallbacks =
                      [ { name = "Normal"; value = 0.0 }
                        { name = "Filled"; value = 1.0 } ] }
              "spacing", // inter-glyph gap -> Spacing
                  { tag = "SPAC"
                    displayName = "Spacing"
                    minValue = -100.0
                    defaultValue = 0.0
                    maxValue = 100.0
                    precision = -1
                    fallbackOnly = false
                    description = """Adjusts the overall letter spacing of a font. The range is a relative percentage change from the family's default spacing, so the default value is 0."""
                    fallbacks = [ { name = "Default"; value = 0.0 } ] }
              "x_height", // lower-case height -> Lowercase Height (parametric)
                  { tag = "YTLC"
                    displayName = "Lowercase Height"
                    minValue = 0.0
                    defaultValue = 500.0
                    maxValue = 1000.0
                    precision = 0
                    fallbackOnly = false
                    description = """A parametric axis for varying the height of the lowercase."""
                    fallbacks = [ { name = "Normal"; value = 500.0 } ] }
              "height", // capital height -> Uppercase Height (parametric)
                  { tag = "YTUC"
                    displayName = "Uppercase Height"
                    minValue = 0.0
                    defaultValue = 725.0
                    maxValue = 1000.0
                    precision = 0
                    fallbackOnly = false
                    description = """A parametric axis for varying the heights of uppercase letterforms."""
                    fallbacks = [ { name = "Normal"; value = 725.0 } ] }
              "descender_depth", // descender depth -> Descender Depth (parametric)
                  { tag = "YTDE"
                    displayName = "Descender Depth"
                    minValue = -1000.0
                    defaultValue = -250.0
                    maxValue = 0.0
                    precision = 0
                    fallbackOnly = false
                    description = """A parametric axis for varying the depth of lowercase descenders."""
                    fallbacks = [ { name = "Normal"; value = -250.0 } ] } ]

    /// Private-use (author-defined) OpenType axis tags for the controls that do
    /// NOT correspond to a registered Google Fonts axis.  Per the OpenType spec,
    /// author-defined tags are all-uppercase; registered tags are in `registry`.
    static member privateTags: Map<string, string> =
        Map [ "dactyl_spline", "DSPL"
              "spline2", "SPL2"
              "constraints", "CNST"
              "leading", "LEAD"
              "alt_a_g", "ALTG"
              "serif", "SERF"
              "end_bulb", "BULB"
              "axis_align_caps", "ACAP"
              "outline", "OTLN"
              "stroked", "STRK"
              "scratches", "SCRT"
              "nib", "NIBB"
              "nib_angle", "NANG"
              "taper", "TAPR"
              "taper_end", "TPRE"
              "wobble", "WOBL"
              "roughness", "RUFF"
              "mobius", "MOBI"
              "constant_offset", "COFF"
              "max_spline_iter", "ITER"
              "show_knots", "KNOT"
              "show_tangents", "TANG"
              "joints", "JYNT"
              "smooth", "SMTH"
              "clip_rect", "CLIP"
              "flatness", "FLAT"
              "end_flatness", "EFLT"
              "debug", "DBUG" ]

    /// Registry metadata for a control name, if it maps to a registered GF axis.
    static member registryInfo(name: string) : Registry option = Map.tryFind name Axes.registry

    /// The OpenType axis tag for a control: the registered tag when the control
    /// maps to a Google Fonts registry axis, otherwise its private-use tag.
    static member axisTag(name: string) : string =
        match Map.tryFind name Axes.registry with
        | Some r -> r.tag
        | None ->
            match Map.tryFind name Axes.privateTags with
            | Some t -> t
            | None -> (name.ToUpper() + "____").Substring(0, 4)

    /// True when an artistic axis that varies stroke width (or displaces the spine)
    /// along the stroke is active; these require the arc-length sampled outline path.
    member this.sampledArtistic =
        this.nib > 0.0 || this.taper > 0.0 || this.wobble > 0.0 || this.roughness > 0.0 || this.mobius > 0.0
