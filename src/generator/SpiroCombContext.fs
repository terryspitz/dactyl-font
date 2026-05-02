module SpiroCombContext

open System.Text
open System.Collections.Generic

open IBezierContext
open Curves
open BezPath

/// IBezierContext implementation that captures both SVG path data and a curvature
/// comb for Spiro curves.
///
/// Because Spiro's solver recursively subdivides each control-point interval into
/// multiple small Bézier segments, drawing a fixed number of teeth per segment
/// produces far more teeth than DactylSpline (which generates one segment per
/// interval).  Instead, we collect all the cubic segments during the solve, then
/// distribute targetCombTeeth teeth evenly across them in GetCombSvg.
///
/// Pass showComb=false to skip comb computation entirely.
type SpiroCombContext(showComb: bool, targetCombTeeth: int) =
    let pathSb = StringBuilder()
    // Each entry: [x0; y0; x1; y1; x2; y2; x3; y3]
    let curveSegs = List<float[]>()
    let mutable needToClose = false
    let mutable currentX = 0.0
    let mutable currentY = 0.0

    member _.GetPathData =
        if needToClose then
            pathSb.Append("Z") |> ignore
            needToClose <- false
        pathSb.ToString()

    member _.GetCombSvg =
        if not showComb || curveSegs.Count = 0 then []
        else
            let stepsPerSeg = max 1 (targetCombTeeth / curveSegs.Count)
            let SCALE = 2000.0
            let combPath = BezPath()
            for seg in curveSegs do
                let bez = CubicBez(seg)
                for s in 0..stepsPerSeg do
                    let t = float s / float stepsPerSeg
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
            combPath.tostringlist()

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
                curveSegs.Add([| currentX; currentY; x1; y1; x2; y2; x3; y3 |])
            currentX <- x3
            currentY <- y3

        member _.MarkKnot(_index, _theta, _x, _y, _type) = ()
