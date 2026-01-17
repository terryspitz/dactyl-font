module Axes

/// UI element type for an axis
type Controls =
    | Range of from: int * upto: int
    | FracRange of from: float * upto: float
    | Checkbox

// Variable which define the font characteristics (named after Variable Font terminology)
type Axes =
    { new_definitions: bool //use new string based glyph definitions, vs. older code ones
      dactyl_spline: bool //use new dactyl splines with new glyph definitions
      spline2: bool //use Raph Levian's new spline-research splines, vs. his original spiro splines
      constraints: bool //constrain tangents to within borders
      width: int //width of normal glyph
      height: int //capital height
      x_height: float //height of lower case as a fraction of capitals
      thickness: int //stroke width
      contrast: float //make vertical lines thicker
      roundedness: int //roundedness
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
      joints: bool //check joints to turn off serifs
      smooth: bool //no corners
      clip_rect: bool //clip each glyph to it's bounding rect (helps with degenerate curves)
      debug: bool } //show debug info in console

    static member DefaultAxes =
        { new_definitions = false
          dactyl_spline = false
          spline2 = false
          width = 300
          height = 600
          x_height = 0.6
          thickness = 30
          contrast = 0.05
          roundedness = 60
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
          max_spline_iter = 10
          show_knots = false
          show_tangents = false
          joints = false
          constraints = false
          smooth = false
          clip_rect = true
          debug = false }

    static member controls =
        [ "new_definitions", Checkbox, "experimental"
          "dactyl_spline", Checkbox, "experimental"
          "spline2", Checkbox, "experimental"
          "width", Range(100, 1000), "default"
          "height", Range(100, 1000), "default"
          "x_height", FracRange(0., 1.5), "default"
          "thickness", Range(1, 200), "default"
          "contrast", FracRange(-0.5, 0.5), "default"
          "roundedness", Range(0, 300), "default"
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
          "max_spline_iter", Range(0, 40), "experimental"
          "show_knots", Checkbox, "experimental"
          "show_tangents", Checkbox, "experimental"
          "joints", Checkbox, "default"
          "smooth", Checkbox, "default"
          "clip_rect", Checkbox, "experimental"
          "debug", Checkbox, "experimental" ]
