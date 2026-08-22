module Axes

/// UI element type for an axis
type Controls =
    | Range of from: int * upto: int
    | FracRange of from: float * upto: float
    /// Like FracRange but snaps to multiples of `step` (e.g. 0.5) instead of the default 0.05.
    | SteppedFracRange of from: float * upto: float * step: float
    | Checkbox

// Variable which define the font characteristics (named after Variable Font terminology)
type Axes =
    { dactyl_spline: bool //use new dactyl splines with new glyph definitions
      spline2: bool //use Raph Levian's new spline-research splines, vs. his original spiro splines
      constraints: bool //constrain tangents to within borders
      width: int //width of normal glyph
      height: int //capital height
      x_height: float //height of lower case as a fraction of capitals
      descender_depth: float //depth of descenders below the baseline, as a fraction of capital height
      weight: int //stroke width
      contrast: float //make vertical lines thicker
      roundedness: int //roundedness
      softness: float //radius of rounding applied at angled corners (0=sharp, 1=max)
      overshoot: int //curves and points extend past the flat guides by this amount, to compensate for looking smaller
      balance: int //raise the optical middle (crossbars, waists) this far above the geometric half height
      spacing: int //gap between glyphs
      leading: int //gap between lines
      monospace: float //fraction to interpolate widths to monospaces
      slant: float //fraction to shear glyphs
      cursive: float //cursive (single-storey) a/g forms: 0=Roman two-storey, 0.5=Auto (cursive when slanted), 1=Cursive single-storey
      serif: int //serif size
      end_bulb: float //fraction of thickness to apply curves to endcaps
      flare: float //end caps expand by this amount
      joint_gap: float //interior joints stop short of the stroke they join, in units of thickness (0=flush)
      axis_align_caps: bool //round angle of caps to horizontal/vertical
      //spine : bool              //show the single width glyph, use with outline off or filled off
      filled: bool //(svg only) filled or empty outlines
      outline: bool //use thickness to expand stroke width
      traces: int //number of parallel strokes drawn per spine (1 = one solid stroke)
      trace_spread: float //lateral span from first trace to last, in stroke thicknesses
      trace_weight: float //each trace's width as a fraction of weight
      trace_jitter: float //random per-trace lateral offset and end-length variation
      nib: float //broad-nib pen: stroke width follows stroke direction (0=off, 1=full nib effect)
      nib_angle: int //nib angle in degrees anticlockwise from horizontal
      taper: float //strokes taper to points at their ends (0=off, 1=pointed all the way to the middle)
      taper_end: float //width at the tapered ends as a fraction of full width (0=sharp point, 1=no narrowing)
      wobble: float //hand-drawn waviness: spine displacement amplitude in units of thickness (0=off)
      roughness: float //random width jitter along the stroke edge, independent per side (0=off)
      mobius: float //strokes are twisting ribbons pinched where edge-on; half-twist density (0=off, 1 ≈ every 300 units)
      pressure: float //stroke widens where the spine turns tightly, as a brush loaded on a curve (0=off)
      ink_spread: float //stroke bleeds outward, as ink wicking into paper fibres (0=off)
      gravity: float //strokes sag downward at their middle, like a line drawn with a tired hand (0=off)
      bounce: float //each glyph sits a little above or below the baseline, for a hand-lettered line (0=off)
      constant_offset: bool //prototype: outlines are dense polylines at constant perpendicular distance from the spine
      max_spline_iter: int //max number of iterations to solve spline curves
      show_knots: bool //show small circles for the points used to define lines/curves
      show_tangents: bool //show lines for the tangents at each knot
      joints: bool //check joints to turn off serifs
      smooth: bool //no corners
      clip_rect: bool //clip each glyph to it's bounding rect (helps with degenerate curves)
      flatness: float //weight of flatness (abs m) in objective function
      end_flatness: float //quadratic curvature-span weight for open-curve endpoint segments (higher = more circular arc at stroke tips)
      opticalKerning: float //0=fixed spacing, 0.5=optical sidebearings only, 1=sidebearings + pair kerning
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
          overshoot = 10
          balance = 15
          spacing = 100
          leading = 50
          monospace = 0.0
          slant = 0.0
          cursive = 1.0
          serif = 0
          end_bulb = 0.0
          flare = 0.0
          joint_gap = 0.0
          axis_align_caps = true
          filled = true
          outline = true
          traces = 1
          trace_spread = 2.0
          trace_weight = 0.1
          trace_jitter = 0.0
          nib = 0.0
          nib_angle = 30
          taper = 0.0
          taper_end = 0.5
          wobble = 0.0
          roughness = 0.0
          mobius = 0.0
          pressure = 0.0
          ink_spread = 0.0
          gravity = 0.0
          bounce = 0.0
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
          opticalKerning = 1.0
          debug = false }

    static member controls =
        [ "width", Range(100, 1000), "backbone", "Width of normal glyph"
          "height", Range(100, 1000), "backbone", "Capital height"
          "x_height", FracRange(0.2, 1.1), "backbone", "Height of lower case as a fraction of capitals"
          "descender_depth", FracRange(0.2, 1.0), "backbone", "Depth of descenders below the baseline, as a fraction of capital height"
          "spacing", Range(0, 200), "backbone", "Gap between glyphs, in glyph units: the space left between two flat-sided neighbours (H|H). Shapes that recede from their edge close some of it back up."
          "leading", Range(-100, 200), "backbone", "Gap between lines"
          "monospace", FracRange(0.0, 1.0), "backbone", "Fraction to interpolate widths to monospace"
          "slant", FracRange(0.0, 1.0), "backbone", "Fraction to shear glyphs"
          "cursive", SteppedFracRange(0.0, 1.0, 0.5), "backbone", "Cursive a/g forms: 0=Roman (two-storey), 0.5=Auto (cursive when slanted), 1=Cursive (single-storey)"
          "roundedness", Range(0, 100), "backbone", "Roundedness"
          "overshoot", Range(0, 50), "backbone", "Optical correction: round and pointed extremes (O, S, o, A, V, W) extend this far past the flat cap/x/baseline guides, so they don't look shorter than flat letters (T, H)"
          "balance", Range(0, 60), "backbone", "Optical correction: raise the mid height (crossbars and waists of H, E, B, S, e) this far above the geometric half, so letters don't look bottom-heavy"
          "opticalKerning", SteppedFracRange(0.0, 1.0, 0.5), "backbone", "How much spacing is derived from the sampled outlines: 0=Fixed (plain advance-width padding), 0.5=Sidebearings (per-glyph optical advances), 1=Kerned (adds residual pair kerns on top)"
          "weight", Range(1, 200), "pen", "Stroke width"
          "contrast", FracRange(-0.5, 0.5), "pen", "Make vertical lines thicker"
          "nib", FracRange(0.0, 1.0), "pen", "Broad-nib pen: stroke width follows stroke direction (0=off, 1=full nib effect)"
          "nib_angle", Range(0, 180), "pen", "Nib angle in degrees anticlockwise from horizontal"
          "pressure", FracRange(0.0, 1.0), "pen", "Stroke widens where the spine turns tightly and stays thin on straight runs, like a brush pressed into a curve (0=off)"
          "end_bulb", FracRange(-1.0, 3.0), "ends", "Fraction of thickness to apply curves to endcaps"
          "flare", FracRange(-1.0, 1.0), "ends", "End caps expand by this amount"
          "joint_gap", FracRange(0.0, 1.0), "ends", "Stencil effect: interior joints stop short of the stroke they join (0=flush/off, just above 0=parting from its edge, 1=a full thickness of clear air)"
          "taper", FracRange(0.0, 1.0), "ends", "Strokes taper to points at their ends (0=off, 1=pointed all the way to the middle)"
          "taper_end", FracRange(0.0, 1.0), "ends", "Width at the tapered ends as a fraction of full width (0=sharp point, 1=no narrowing)"
          "serif", Range(0, 70), "ends", "Serif size"
          "wobble", FracRange(0.0, 1.0), "hand", "Hand-drawn waviness: spine displacement amplitude in units of thickness (0=off)"
          "roughness", FracRange(0.0, 1.0), "hand", "Random width jitter along the stroke edge, independent per side (0=off)"
          "ink_spread", FracRange(0.0, 1.0), "hand", "Stroke bleeds outward with a fibrous edge, as ink wicking into paper (0=off)"
          "gravity", FracRange(0.0, 1.0), "hand", "Strokes sag downward at their middle, most on horizontals and not at all on verticals (0=off)"
          "bounce", FracRange(0.0, 1.0), "hand", "Hand-lettered line: each glyph sits a little above or below the baseline, the same way every time (0=off)"
          "traces", SteppedFracRange(1.0, 6.0, 1.0), "traces", "Number of parallel strokes drawn per spine. 1 = a single solid stroke; higher counts give inline, split-nib and sketchy multi-pass looks"
          "trace_spread", FracRange(0.0, 3.0), "traces", "Lateral span from the first trace to the last, in stroke thicknesses. 2.0 puts the outer traces exactly where a solid stroke's edges would be"
          "trace_weight", FracRange(0.02, 1.0), "traces", "Each trace's width as a fraction of the main weight"
          "trace_jitter", FracRange(0.0, 1.0), "traces", "Sketchy multi-pass look: random per-trace lateral offset and end-length variation"
          "mobius", FracRange(0.0, 3.0), "traces", "Strokes are twisting ribbons pinched where edge-on; half-twist density (0=off, 1 ≈ every 300 units)"
          "softness", FracRange(0.0, 1.0), "render", "Radius of rounding applied at angled corners (0=sharp, 1=max)"
          "axis_align_caps", Checkbox, "render", "Round angle of caps to horizontal/vertical"
          "outline", Checkbox, "render", "Use thickness to expand stroke width"
          "filled", Checkbox, "render", "(SVG only) filled or empty outlines"
          "dactyl_spline", Checkbox, "experimental", "Use new dactyl splines with new glyph definitions"
          "spline2", Checkbox, "experimental", "Use Raph Levien's new spline-research splines, vs. his original spiro splines"
          "smooth", Checkbox, "experimental", "No corners"
          "constraints", Checkbox, "experimental", "Constrain tangents to within borders"
          "constant_offset", Checkbox, "experimental", "Prototype: outlines are dense polylines at constant perpendicular distance from the spine"
          "max_spline_iter", Range(0, 200), "experimental", "Max number of iterations to solve spline curves"
          "flatness", FracRange(0.0, 10.0), "experimental", "Weight of flatness (abs m) in objective function"
          "end_flatness", FracRange(0.0, 30.0), "experimental", "Quadratic curvature-span weight for open-curve endpoint segments (higher = more circular arc at stroke tips)"
          "show_knots", Checkbox, "debug", "Show small circles for the points used to define lines/curves"
          "show_tangents", Checkbox, "debug", "Show lines for the tangents at each knot"
          "joints", Checkbox, "debug", "Check joints to turn off serifs"
          "clip_rect", Checkbox, "debug", "Clip each glyph to its bounding rect (helps with degenerate curves)"
          "debug", Checkbox, "debug", "Show debug info in console" ]

    /// Axes a pen preset (below) speaks for.  Applying a preset resets every one
    /// of these to its default and then applies the preset's overrides, so the
    /// chips are mutually exclusive rather than cumulative — clicking `Sketch`
    /// after `Broad nib` gives the sketch, not a sketchy nib.
    ///
    /// Deliberately excluded: `weight`, `contrast` and `serif` (typographic
    /// choices the user makes independently of pen character), `bounce` (a
    /// property of the line, not the stroke) and `joint_gap` (a stencil effect).
    static member presetAxes =
        [ "nib"
          "nib_angle"
          "pressure"
          "taper"
          "taper_end"
          "flare"
          "end_bulb"
          "wobble"
          "roughness"
          "ink_spread"
          "gravity"
          "traces"
          "trace_spread"
          "trace_weight"
          "trace_jitter"
          "mobius" ]

    /// Named bundles of pen axes, offered as a chip row above the controls.
    /// Each is a starting point inside the space the axes describe, not a mode:
    /// every slider stays live afterwards.
    static member presets: (string * (string * float) list) list =
        [ "Solid", []
          "Broad nib", [ "nib", 1.0; "nib_angle", 30.0 ]
          "Brush", [ "pressure", 0.8; "taper", 0.2; "taper_end", 0.45; "ink_spread", 0.15 ]
          "Marker", [ "ink_spread", 0.55; "flare", 0.1; "end_bulb", 0.3 ]
          "Sketch",
          [ "traces", 3.0
            "trace_spread", 2.0
            "trace_weight", 0.33
            "trace_jitter", 0.5
            "roughness", 0.3 ]
          "Inline", [ "traces", 2.0; "trace_spread", 1.2; "trace_weight", 0.15 ]
          "Split nib", [ "traces", 2.0; "trace_spread", 0.8; "trace_weight", 0.2; "nib", 0.8 ]
          "Ribbon", [ "mobius", 1.0; "taper", 0.2 ]
          "Backscratch",
          [ "traces", 5.0
            "trace_spread", 2.4
            "trace_weight", 0.06
            "trace_jitter", 0.9
            "roughness", 0.5
            "wobble", 0.2 ]
          "Tired hand", [ "gravity", 0.7; "wobble", 0.4; "roughness", 0.25 ] ]

    /// (axis, parent) pairs: `axis` has no effect while `parent` sits at its
    /// default, so the UI dims it rather than hiding it.
    static member dependsOn =
        [ "nib_angle", "nib"
          "taper_end", "taper"
          "trace_spread", "traces"
          "trace_weight", "traces"
          "trace_jitter", "traces" ]

    /// Whether the two-storey Roman ("alt") a/g shapes should be used, given the
    /// `cursive` axis and current `slant`.  Cursive: 0=Roman (two-storey alt
    /// shapes), 1=Cursive (single-storey default shapes), 0.5=Auto (Roman when
    /// upright, cursive when slanted).  Single source of truth for both the
    /// generator and the glyph-definition preview.
    static member cursiveUsesAlt (cursive: float) (slant: float) : bool =
        if cursive < 0.25 then true // Roman: two-storey alt shapes
        elif cursive > 0.75 then false // Cursive: single-storey default shapes
        else slant = 0.0 // Auto: Roman when upright, cursive when slanted

    /// Whether this axis set selects the two-storey Roman ("alt") a/g shapes.
    member this.useCursiveAlt = Axes.cursiveUsesAlt this.cursive this.slant

    /// `opticalKerning` is a three-stop axis over the two layers spacing is
    /// built from, so each can be seen (and costed) on its own:
    ///   0.0  Fixed        — plain spine extent + spacing + sidebearing padding
    ///   0.5  Sidebearings — advances measured from each glyph's own silhouette
    ///   1.0  Kerned       — plus the residual pair kerns the per-glyph pass
    ///                       structurally can't cover (diagonals, overhangs)
    /// Bucketed by threshold rather than interpolated: partway between "measure
    /// the outline" and "don't" isn't a meaningful font.
    member this.useOpticalSpacing = this.opticalKerning >= 0.25

    /// True only at the top stop — pair kerning sits on top of optical
    /// sidebearings, never instead of them.
    member this.usePairKerning = this.opticalKerning >= 0.75

    /// True when an artistic axis that varies stroke width (or displaces the spine)
    /// along the stroke is active; these require the arc-length sampled outline path.
    member this.sampledArtistic =
        this.nib > 0.0 || this.taper > 0.0 || this.wobble > 0.0 || this.roughness > 0.0 || this.mobius > 0.0
        // Parallel traces are built by offsetting the sampled spine.
        || this.traces > 1
        // Curvature-driven width, ink bleed and sag all vary along the stroke.
        || this.pressure > 0.0 || this.ink_spread > 0.0 || this.gravity > 0.0
        // joint_gap trims the spine by arc length, which only the sampled path can do.
        || this.joint_gap > 0.0

    /// Actual joint recession, in thicknesses back from the covering stroke's spine.
    ///
    /// The first thickness is invisible: it only walks the joint end back to the
    /// covering stroke's edge, where the two are still touching. Handing that dead
    /// travel to the user wastes half the slider, so the axis maps
    ///     0        -> 0      (flush, off)
    ///     (0, 1]   -> (1, 2]
    /// putting the whole visible range — edge to a full thickness of clear air —
    /// across the axis, and keeping 0 as an exact no-op.
    member this.jointGapRecession =
        if this.joint_gap <= 0.0 then 0.0 else 1.0 + this.joint_gap
