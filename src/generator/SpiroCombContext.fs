module SpiroCombContext

open System.Text

open IBezierContext
open Curves
open BezPath

/// IBezierContext implementation that captures both SVG path data and a curvature
/// comb in a single Spiro solve pass.  Pass showComb=true to compute comb spurs.
type SpiroCombContext(showComb: bool) =
    let pathSb = StringBuilder()
    let combPath = BezPath()
    let mutable needToClose = false
    let mutable currentX = 0.0
    let mutable currentY = 0.0

    member _.GetPathData =
        if needToClose then
            pathSb.Append("Z") |> ignore
            needToClose <- false
        pathSb.ToString()

    member _.GetCombSvg = combPath.tostringlist()

    interface IBezierContext with
        member _.MoveTo(x, y, isOpen) =
            if needToClose then
                pathSb.AppendLine("Z") |> ignore
            pathSb.AppendLine(sprintf "M %s,%s" (Format x) (Format y)) |> ignore
            currentX <- x
            currentY <- y
            needToClose <- not isOpen

        member _.LineTo(x, y) =
            pathSb.AppendLine(sprintf "L %s,%s" (Format x) (Format y)) |> ignore
            currentX <- x
            currentY <- y

        member _.QuadTo(x1, y1, x2, y2) =
            pathSb.AppendLine(sprintf "Q %s,%s %s,%s" (Format x1) (Format y1) (Format x2) (Format y2)) |> ignore
            currentX <- x2
            currentY <- y2

        member _.CurveTo(x1, y1, x2, y2, x3, y3) =
            pathSb.AppendLine(sprintf "C %s,%s %s,%s %s,%s" (Format x1) (Format y1) (Format x2) (Format y2) (Format x3) (Format y3)) |> ignore
            if showComb then
                let bez = CubicBez([| currentX; currentY; x1; y1; x2; y2; x3; y3 |])
                let COMB_STEPS = 20
                let SCALE = 2000.0
                for s in 0..COMB_STEPS do
                    let t = float s / float COMB_STEPS
                    let kv = bez.curvature t
                    let pt = bez.eval t
                    let d = bez.deriv t
                    let normal = { x = -d.y; y = d.x }
                    let nLen = normal.norm()
                    if nLen > 1e-6 then
                        let n = { x = normal.x / nLen; y = normal.y / nLen }
                        let endPt = { x = pt.x + kv * SCALE * n.x; y = pt.y + kv * SCALE * n.y }
                        combPath.moveto(pt.x, pt.y)
                        combPath.lineto(endPt.x, endPt.y)
            currentX <- x3
            currentY <- y3

        member _.MarkKnot(_index, _theta, _x, _y, _type) = ()
