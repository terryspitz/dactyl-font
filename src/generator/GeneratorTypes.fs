module GeneratorTypes

open SpiroPointType
open SpiroSegment
open SpiroControlPoint


type Point =
    { y: float
      x: float
      y_fit: bool
      x_fit: bool }

    member this.GetXY = this.x, this.y

    static member (+)(lhs: Point, rhs: Point) =
        { y = lhs.y + rhs.y
          x = lhs.x + rhs.x
          y_fit = lhs.y_fit || rhs.y_fit
          x_fit = lhs.x_fit || rhs.x_fit }

    static member (-)(lhs: Point, rhs: Point) =
        { y = lhs.y - rhs.y
          x = lhs.x - rhs.x
          y_fit = lhs.y_fit || rhs.y_fit
          x_fit = lhs.x_fit || rhs.x_fit }



/// Class defining important Font guidelines (x, y points) based on axes.
type FontMetrics(axes: Axes.Axes) =
    // X axis guides, from left
    member this.L = 0.0 // Left
    member this.R = float axes.width // Right = standard glyph width
    member this.N = float (axes.width * 4 / 5) // Narrow glyph width
    member this.C = float axes.width / 2.0 // Centre
    member this.monospaceWidth = this.N
    member this.W = float (axes.width * 3 / 2) // Wide glyph width

    // Y axis guides, from bottom-up
    member this.B = 0.0 // Bottom
    member this.X = axes.x_height * float axes.height // x-height
    member this.M = this.X / 2.0 // Midway down from x-height
    member this.T = float axes.height // Top = standard glyph caps height
    member this.H = this.T / 2.0 // Half total height
    member this.D = -float axes.height / 2.0 // descender height

    member this.offset = float axes.roundedness // offset from corners
    member this.dotHeight = max ((this.X + this.T) / 2.0) (this.X + float axes.thickness * 3.0)

    member this.thickness =
        if axes.stroked || axes.scratches then
            max (float axes.thickness) 30.0
        else
            float axes.thickness

type Knot =
    { pt: Point
      ty: SpiroPointType
      th_in: float option
      th_out: float option }

type SCP = SpiroControlPoint

type Element =
    | Glyph of c: char
    | Line of p1: Point * p2: Point
    | PolyLine of list<Point>
    // Curve with optional incoming/outgoing tangent pair per knot and open/closed flag
    | Curve of knots: list<Knot> * isClosed: bool
    | Dot of Point
    | EList of list<Element>
    | Space

// Helpers to create Curve from simpler point lists
let withNoTangents pts =
    List.map (fun (p, t) -> { pt = p; ty = t; th_in = None; th_out = None }) pts

let openCurve pts = Curve(withNoTangents pts, false)
let closedCurve pts = Curve(withNoTangents pts, true)

let rec movePoints fn (e: Element) =
    match e with
    | Curve(pts, isClosed) ->
        Curve(
            [ for k in pts do
                  { k with pt = fn k.pt } ],
            isClosed
        )
    | Line(p1, p2) -> Line(fn p1, fn p2)
    | PolyLine(pts) -> PolyLine(List.map fn pts)
    | Dot(p) -> Dot(fn p)
    | EList(elems) -> EList(List.map (movePoints fn) elems)
    | Space -> Space
    | Glyph(_) -> e // Cannot move points of an unreduced glyph without font metrics

let applyIf b f = if b then f else id

type SpiroElement =
    | SpiroOpenCurve of segments: list<SpiroSegment>
    | SpiroClosedCurve of segments: list<SpiroSegment>
    | SpiroDot of Point
    | SpiroSpace

/// Minimal solved-knot data needed for outline offsetting.
/// Populated either from Spiro/Spline2 (via SpiroSegment) or DactylSpline (via BezierPoint).
type Segment =
    { X: float
      Y: float
      tangentStart: float // entry tangent at this knot
      tangentEnd: float // exit tangent from this knot
      seg_ch: float // chord length to next knot
      Type: SpiroPointType }

// Bring into local namespace, and rename if nec.
let CurveToLine = SpiroPointType.Left
let LineToCurve = SpiroPointType.Right
let G2 = SpiroPointType.G2
let G4 = SpiroPointType.G4
let Start = SpiroPointType.OpenContour
let Corner = SpiroPointType.Corner
let End = SpiroPointType.EndOpenContour
let EndClosed = SpiroPointType.End
let Anchor = SpiroPointType.Anchor
let Handle = SpiroPointType.Handle
