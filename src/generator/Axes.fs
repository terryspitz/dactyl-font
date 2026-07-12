module Axes

/// UI element type for an axis
type Controls =
    | Range of from: int * upto: int
    | FracRange of from: float * upto: float
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
      thickness: int //stroke width
      contrast: float //make vertical lines thicker
      roundedness: int //roundedness
      soft_corners: float //radius of rounding applied at angled corners (0=sharp, 1=max)
      // overshoot : int          //curves are larger by this amount to compensate for looking smaller
      tracking: int //gap between glyphs
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
          thickness = 30
          contrast = 0.05
          roundedness = 60
          soft_corners = 0.0
          tracking = 40
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
          "tracking", Range(0, 200), "backbone", "Gap between glyphs"
          "leading", Range(-100, 200), "backbone", "Gap between lines"
          "monospace", FracRange(0.0, 1.0), "backbone", "Fraction to interpolate widths to monospace"
          "italic", FracRange(0.0, 1.0), "backbone", "Fraction to shear glyphs"
          "alt_a_g", Checkbox, "backbone", "Use two-storey alternate shapes for 'a' and 'g'"
          "roundedness", Range(0, 100), "backbone", "Roundedness"
          "thickness", Range(1, 200), "outline", "Stroke width"
          "contrast", FracRange(-0.5, 0.5), "outline", "Make vertical lines thicker"
          "soft_corners", FracRange(0.0, 1.0), "outline", "Radius of rounding applied at angled corners (0=sharp, 1=max)"
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

    /// True when an artistic axis that varies stroke width (or displaces the spine)
    /// along the stroke is active; these require the arc-length sampled outline path.
    member this.sampledArtistic =
        this.nib > 0.0 || this.taper > 0.0 || this.wobble > 0.0 || this.roughness > 0.0 || this.mobius > 0.0
