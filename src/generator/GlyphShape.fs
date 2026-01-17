/// A DRAFT set of structures for storing Glyph definitions, 
/// made up of points, line, curves and constraints.

module GlyphShape

type Point = {
    X : float option
    Y : float option
}
type Joint = 
    | Point of point : Point
    | Segment of segment : Segment
with
type Line = {
    start_point : Joint
    end_point : Joint
}
type CurveSegment = {
    points : Joint list
    start_tangent : float option
    end_tangent : float option
}
type Segment = 
    | Line of line : Line
    | Curve of curve : CurveSegment
type Glyph = {
    name : string
    segments : Segment list
}