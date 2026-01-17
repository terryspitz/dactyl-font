module GeneratorTypes

open SpiroPointType
open SpiroSegment
open SpiroControlPoint


type Point =
    // Raw coordinates
    | YX of y: int * x: int

    // Y coordinate: Top,X-height,Half-height,Bottom
    // X coordinate: Left,Centre,Right
    // o adds/subtracts an offset to the dimension it follows
    | TL | TLo | TC | TRo | TR        // Top points: Left, Left offset inward, Centre, Right
    | ToL | ToC | ToR           // Top offset down
    | XL | XLo | XC | XRo | XR  // x-height
    | XoL | XoC | XoR           // x-height offset down
    | ML | MC | MR              // Midway down from x-height
    | HL | HLo | HC | HRo | HR  // half glyph height
    | BoL | BoC | BoR           // Bottom offset up
    | BL | BLo | BC | BRo | BR  // Bottom
    | DoL                       // Descender offset up
    | DL | DC | DR              // Descender
    | BN | BoN | HN | XoN | XN | TN         // Narrow width points
    | Mid of p1 : Point * p2 : Point
    | Interp of p1 : Point * p2 : Point * frac : float
    
    member this.GetXY = match this with | YX(y,x) -> x,y | _ -> invalidArg "this" "Point (+) only works with reduced points"
    static member (+) (lhs : Point, rhs : Point) =
        let x1,y1 = lhs.GetXY
        let x2,y2 = rhs.GetXY
        YX(y1+y2, x1+x2)
    static member (-) (lhs : Point, rhs : Point) =
        let x1,y1 = lhs.GetXY
        let x2,y2 = rhs.GetXY
        YX(y1-y2, x1-x2)

type SCP = SpiroControlPoint

type Element = 
    | Glyph of c : char
    | Line of p1 : Point * p2 : Point
    | PolyLine of list<Point>
    | OpenCurve of list<Point * SpiroPointType>
    | ClosedCurve of list<Point * SpiroPointType>
    | TangentCurve of knots : list<Point * SpiroPointType * float option> * isClosed : bool
    | Dot of Point
    | EList of list<Element>
    | Space

type SpiroElement =
    | SpiroOpenCurve of segments: list<SpiroSegment>
    | SpiroClosedCurve of segments: list<SpiroSegment>
    | SpiroDot of Point
    | SpiroSpace

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
