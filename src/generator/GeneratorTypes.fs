module GeneratorTypes

open SpiroPointType
open SpiroSegment
open SpiroControlPoint


type Point =
    // Raw coordinates
    | YX of y: int * x: int
    | OYX of y: int * x: int * y_fit: bool * x_fit: bool

    // Y coordinate: Top,X-height,Half-height,Bottom
    // X coordinate: Left,Centre,Right
    // o adds/subtracts an offset to the dimension it follows
    // Top points: Left, Left offset inward, Centre, Right
    | TL
    | TLo
    | TC
    | TRo
    | TR
    // Top - offset down
    | ToL
    | ToC
    | ToR
    // x-height
    | XL
    | XLo
    | XC
    | XRo
    | XR
    // x-height - offset down
    | XoL
    | XoC
    | XoR
    // Midway down from x-height
    | ML
    | MC
    | MR
    // half glyph height
    | HL
    | HLo
    | HC
    | HRo
    | HR
    // Bottom + offset up
    | BoL
    | BoC
    | BoR
    // Bottom
    | BL
    | BLo
    | BC
    | BRo
    | BR
    // Descender offset up
    | DoL
    // Descender
    | DL
    | DC
    | DR
    // Narrow width points
    | BN
    | BoN
    | HN
    | XoN
    | XN
    | TN
    | Mid of p1: Point * p2: Point
    | Interp of p1: Point * p2: Point * frac: float

    member this.GetXY =
        match this with
        | YX(y, x) -> x, y
        | OYX(y, x, _, _) -> x, y
        | _ -> invalidArg "this" "Point (+) only works with reduced points"

    static member (+)(lhs: Point, rhs: Point) =
        let x1, y1 = lhs.GetXY
        let x2, y2 = rhs.GetXY
        YX(y1 + y2, x1 + x2)

    static member (-)(lhs: Point, rhs: Point) =
        let x1, y1 = lhs.GetXY
        let x2, y2 = rhs.GetXY
        YX(y1 - y2, x1 - x2)

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
