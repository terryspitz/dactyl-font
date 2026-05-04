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
      thickness: int //stroke width
      contrast: float //make vertical lines thicker
      roundedness: int //roundedness
      soft_corners: float //radius of rounding applied at angled corners (0=sharp, 1=max)
      // overshoot : int          //curves are larger by this amount to compensate for looking smaller
      tracking: int //gap between glyphs
      leading: int //gap between lines
      monospace: float //fraction to interpolate widths to monospaces
      italic: float //fraction to sheer glyphs
      serif: int //serif size
      end_bulb: float //fraction of thickness to apply curves to endcaps
      flare: float //end caps expand by this amount
      axis_align_caps: bool //round angle of caps to horizontal/vertical
      //spine : bool              //show the single width glyph, use with outline off or filled off
      filled: bool //(svg only) filled or empty outlines
      outline: bool //use thickness to expand stroke width
      stroked: bool //each stroke is 4 parallel lines
      scratches: bool //horror/paint strokes font
      max_spline_iter: int //max number of iterations to solve spline curves
      show_knots: bool //show small circles for the points used to define lines/curves
      show_tangents: bool //show lines for the tangents at each knot
      show_comb: bool //show curvature comb
      joints: bool //check joints to turn off serifs
      smooth: bool //no corners
      clip_rect: bool //clip each glyph to it's bounding rect (helps with degenerate curves)
      flatness: float //weight of flatness (abs m) in objective function
      sidebearingScale: float //multiplier on the per-glyph thickness sidebearing padding
      debug: bool } //show debug info in console

    static member DefaultAxes =
        { dactyl_spline = true
          spline2 = false
          width = 300
          height = 600
          x_height = 0.6
          thickness = 30
          contrast = 0.05
          roundedness = 60
          soft_corners = 0.0
          tracking = 40
          leading = 50
          monospace = 0.0
          italic = 0.0
          serif = 0
          end_bulb = 0.0
          flare = 0.0
          axis_align_caps = true
          filled = true
          outline = true
          stroked = false
          scratches = false
          max_spline_iter = 100
          show_knots = false
          show_tangents = false
          show_comb = false
          joints = true
          constraints = false
          smooth = false
          clip_rect = true
          flatness = 1.0
          sidebearingScale = 1.2
          debug = false }

    static member controls =
        [ "dactyl_spline", Checkbox, "experimental"
          "spline2", Checkbox, "experimental"
          "width", Range(100, 1000), "default"
          "height", Range(100, 1000), "default"
          "x_height", FracRange(0.2, 1.1), "default"
          "thickness", Range(1, 200), "default"
          "contrast", FracRange(-0.5, 0.5), "default"
          "roundedness", Range(0, 100), "default"
          "soft_corners", FracRange(0.0, 1.0), "default"
          "tracking", Range(0, 200), "default"
          "leading", Range(-100, 200), "default"
          "monospace", FracRange(0.0, 1.0), "default"
          "italic", FracRange(0.0, 1.0), "default"
          "serif", Range(0, 70), "default"
          "end_bulb", FracRange(-1.0, 3.0), "default"
          "flare", FracRange(-1.0, 1.0), "default"
          "axis_align_caps", Checkbox, "default"
          "constraints", Checkbox, "experimental"
          "filled", Checkbox, "default"
          "outline", Checkbox, "default"
          "stroked", Checkbox, "default"
          "scratches", Checkbox, "default"
          "max_spline_iter", Range(0, 200), "experimental"
          "show_knots", Checkbox, "debug"
          "show_tangents", Checkbox, "debug"
          "show_comb", Checkbox, "debug"
          "joints", Checkbox, "debug"
          "smooth", Checkbox, "default"
          "clip_rect", Checkbox, "debug"
          "flatness", FracRange(0.0, 200.0), "experimental"
          "sidebearingScale", FracRange(0.0, 2.0), "experimental"
          "debug", Checkbox, "debug" ]
