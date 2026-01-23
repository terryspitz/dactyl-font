// A spline defined by a collection of cubic bezier curves passing through defined points with
// optionally defined tangents.
//  that can be fitted to a set of control points
// with configurable spiro/euler spiral curvature, smoothness and other constraints.
// Fit sampled points to Euler spiral (like spiro spline) where curvature k is linear in curve length

// Gemini Advanced 1.0 says

/// This code defines a system for creating and fitting splines based on a collection
/// of control points. Splines are represented using cubic Bézier curves. The code
/// provides functionality to iteratively fit the Bézier curves to achieve desired
/// smoothness and curvature properties, with an emphasis on approximating Euler spirals.
///
/// Key Concepts:
/// * Control Points: User-defined points that guide the general shape of the spline.
///   Control points have properties for position, tangents, and continuity type.
/// * Bézier Points: Internal representation of points along a Bézier curve segment,
///   including tangent angles and distances to corresponding control points.
/// * Solver: Handles the spline fitting process by initializing Bézier points and
///   iteratively minimizing an error function that measures deviations from a
///   desired curvature profile.
/// * Spline: The final spline object built from fitted Bézier curve segments.
///   Provides methods for solving (fitting) and converting the spline to different
///   representations (e.g., SVG path).



module DactylSpline

open System
//reuse spline-research Vec2, SplinePointType, SplineControlPoint, CubicBez
open Curves
open BezPath
#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop
#else
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
#endif

#if FABLE_COMPILER
[<Import("nelderMead", from = "../../web/src/lib/fmin/src/nelderMead.js")>]
// nelderMead(f, initial, params)
let nelderMead (objective: float array -> float) (initial: float array) (param) : float array = jsNative
#endif

type DControlPoint =
    { mutable ty: SplinePointType //Continuity at this point: Smooth, Corner, LineToCurve or CurveToLine
      x: float option // x coord or None to fit x to 'smoothest' curve
      y: float option // same for y
      mutable th: float option  // optional tangent theta in direction of next point
    // member this.tostring() =
    //     sprintf "CP ty:%A x:%A y:%A th:%A" this.ty this.x this.y this.th
    }

    static member (-)(lhs, rhs) =
        { x = lhs.x.Value - rhs.x.Value
          y = lhs.y.Value - rhs.y.Value }


// type ControlPointOut(ty, x, y, lth, rth) =
//     member val ty : SplinePointType = ty with get, set
//     member val x : float = x with get
//     member val y : float = y with get
//     member val th : float = lth with get, set  // angle of tangent towards next point
//     // member val lth : float = lth with get, set  // angle of curve to previous point
//     // member val rth : float = lth with get, set  // angle of curve to next point

let BEZIER_ARGS = 5
// Internal class for a knot on bezier curve including tangents and distances to control points
type BezierPoint() =
    //     x : float
    //     y : float
    //     th : float   // tangent towards next (right) point
    //     ld : float   // distance to left bezier control point
    //     rd : float   // distance to right bezier control point
    let _arr = Array.create BEZIER_ARGS nan
    let _fit = Array.create BEZIER_ARGS true

    member this.x
        with get () = _arr.[0]
        and set (v) = _arr.[0] <- v

    member this.y
        with get () = _arr.[1]
        and set (v) = _arr.[1] <- v

    member this.th
        with get () = _arr.[2]
        and set (v) = _arr.[2] <- v

    member this.ld
        with get () = _arr.[3]
        and set (v) = _arr.[3] <- v

    member this.rd
        with get () = _arr.[4]
        and set (v) = _arr.[4] <- v

    member this.fit_x
        with get () = _fit.[0]
        and set (v) = _fit.[0] <- v

    member this.fit_y
        with get () = _fit.[1]
        and set (v) = _fit.[1] <- v

    member this.fit_th
        with get () = _fit.[2]
        and set (v) = _fit.[2] <- v

    member this.fit_ld
        with get () = _fit.[3]
        and set (v) = _fit.[3] <- v

    member this.fit_rd
        with get () = _fit.[4]
        and set (v) = _fit.[4] <- v

    member this.vec = { x = this.x; y = this.y }
    member this.arr = _arr
    member this.fit = _fit

    member this.lpt() =
        { x = this.x - this.ld * cos this.th
          y = this.y - this.ld * sin this.th }

    member this.rpt() =
        { x = this.x + this.rd * cos this.th
          y = this.y + this.rd * sin this.th }

    member this.tostring() =
        let fit_str f = if f then "~" else ""

        sprintf
            "BezPt x:%s%f y:%s%f th:%s%f ld:%s%f rd:%s%f"
            (fit_str this.fit_x)
            this.x
            (fit_str this.fit_y)
            this.y
            (fit_str this.fit_th)
            this.th
            (fit_str this.fit_ld)
            this.ld
            (fit_str this.fit_rd)
            this.rd


let averageAngles rth lth =
    if abs (rth - lth) > PI then
        // if angle is more than 180 degrees, take the shorter path
        (lth + rth) / 2. + PI
    else
        (lth + rth) / 2.


let linear_regression xs ys =
    if Array.length xs <> Array.length ys then
        failwith "linear_regression: arrays must be of same length"

    let n = float (Array.length xs)
    let mean_x = (Array.sum xs) / n
    let sum_y = Array.sum ys
    let mean_y = sum_y / n
    // best fit k_i = m * dist_i + c using https://en.wikipedia.org/wiki/Simple_linear_regression
    let m =
        (Array.sumBy (fun (x, y) -> (y - mean_y) * (x - mean_x)) (Array.zip xs ys))
        / (Array.sumBy (fun x -> (x - mean_x) * (x - mean_x)) xs)

    let c = (mean_y - m * mean_x)

    let residuals =
        (Array.zip ys xs
         |> Array.sumBy (fun (k, d) -> let err = k - (m * d + c) in err * err))

    m, c, residuals

type Solver(ctrlPts: DControlPoint array, isClosed: bool, flatness: float, debug: bool) =
    let _points: BezierPoint array = Array.init ctrlPts.Length (fun _ -> BezierPoint())
    member this.ctrlPts = ctrlPts
    member this.isClosed = isClosed
    member val startTh: float option = None with get, set
    member val endTh: float option = None with get, set
    member this.points() = _points
    member this.flatness = flatness
    member this.debug = debug

    member this.initialise() =
        //initialise points
        if this.debug then
            printfn "solver init. Flatness: %f" this.flatness
            printfn "%A" ctrlPts

        // Initialisation of points
        for i in 0 .. ctrlPts.Length - 1 do
            let point = _points.[i]
            let ctrlPt = ctrlPts.[i]

            match ctrlPt.x with
            | Some x ->
                point.x <- x
                point.fit_x <- false
            // possibly: define fit_x/y separately from none, then use x/y for initialisation
            | None ->
                point.x <-
                    if i = 0 then
                        ctrlPts.[i + 1].x.Value
                    elif i = ctrlPts.Length - 1 then
                        ctrlPts.[i - 1].x.Value
                    else
                        (ctrlPts.[i - 1].x.Value + ctrlPts.[i + 1].x.Value) / 2.
            //TODO: fit_y
            match ctrlPt.y with
            | Some y ->
                point.y <- y
                point.fit_y <- false
            | None ->
                point.y <-
                    if i = 0 then
                        ctrlPts.[i + 1].y.Value
                    elif i = ctrlPts.Length - 1 then
                        ctrlPts.[i - 1].y.Value
                    else
                        (ctrlPts.[i - 1].y.Value + ctrlPts.[i + 1].y.Value) / 2.

        // Initialise tangent angle/distance
        for i in 0 .. ctrlPts.Length - 1 do
            let point = _points.[i]
            let ctrlPt = ctrlPts.[i]
            let mutable dpl = point.vec - _points.[max (i - 1) 0].vec //pointing from previous point to this
            let mutable dpr = _points.[min (i + 1) (_points.Length - 1)].vec - point.vec

            match ctrlPt.th with
            | Some th ->
                point.th <- th
                point.fit_th <- false
            | None ->
                let lth = dpl.atan2 ()
                let rth = dpr.atan2 ()

                let new_th =
                    if i = 0 then
                        if isClosed then
                            dpl <- _points.[_points.Length - 1].vec - _points.[_points.Length - 2].vec
                            let lth = dpl.atan2 ()
                            averageAngles rth lth
                        else
                            rth
                    elif i = ctrlPts.Length - 1 then
                        if isClosed then
                            dpr <- _points.[1].vec - _points.[0].vec
                            let rth = dpr.atan2 ()
                            averageAngles rth lth
                        else
                            lth
                    else
                        averageAngles rth lth

                point.th <- new_th

            point.ld <- dpl.norm () / 3.
            point.rd <- dpr.norm () / 3.

            if i = 0 then
                point.fit_ld <- false
            elif i = ctrlPts.Length - 1 then
                point.fit_rd <- false

        if this.debug then
            printfn "solver post init"

            for p in _points do
                printfn "%s" (p.tostring ())

    member this.computeErr() =
        // Sample the curvature and distance (arc length) at STEPS fractions along each bezier.
        // Euler spiral (like spiro spline) wants its curvature k to be linear in curve length l
        // So treat l as an x-coord and k as a y-coord and fit a line to the data, then measure residuals from that line and minimise
        // Also if there are free variables, curvature should be as constant as possible ,
        // so minimise the gradient of the fitted curvature line
        // Also [optionally?] minimise line length
        let mutable errs = 0.
        let mutable curveNorm: (float * float) list = []
        let STEPS = 8

        for i in 0 .. _points.Length - 2 do
            let point1 = _points.[i]
            let point2 = _points.[i + 1]

            let bez =
                CubicBez(
                    [| point1.x
                       point1.y
                       point1.rpt().x
                       point1.rpt().y
                       point2.lpt().x
                       point2.lpt().y
                       point2.x
                       point2.y |]
                )

            curveNorm <-
                curveNorm
                @ [ for j in 0..STEPS do
                        let t0 = (-0.5 + float j) / (float STEPS)
                        let t1 = (float j) / (float STEPS)
                        let t2 = (0.5 + float j) / (float STEPS)
                        (10000. * bez.curvature t1, (bez.eval t2 - bez.eval t0).norm ()) ]

        let ks, segmentLengths = curveNorm |> List.toArray |> Array.unzip

        let cumm_dists, max_dist =
            Array.mapFold (fun sum dist -> let acc = sum + dist in (acc, acc)) 0. segmentLengths

        let m, c, residuals = linear_regression cumm_dists ks

        if
            Double.IsNaN m
            || Double.IsNaN c
            || Double.IsNaN residuals
            || Double.IsInfinity m
            || Double.IsInfinity c
            || Double.IsInfinity residuals
        then
            errs <- 1e9 // Large penalty for invalid configuration
        else
            errs <- errs + residuals + abs m * flatness //+ max_dist
        // errs <- errs + flatness * abs m //+ max_dist

        if this.debug then
            printfn
                "ks %A\nsegmentLengths %A\ndists %A"
                (Array.map int ks)
                (Array.map int segmentLengths)
                (Array.map int cumm_dists)

            printfn "m=%f c=%f res=%f max_dist=%f errs=%f" m c residuals max_dist errs

        if isClosed then
            // add error term for mismatch between end of previous bezier and start of this
            let segmentErr = ks.[ks.Length - 1] - ks.[0]
            errs <- errs + segmentErr * segmentErr

        assert not (isnan errs)

        if Double.IsNaN errs || Double.IsInfinity errs then
            if this.debug then
                printfn "computeErr returning NaN/Inf, replaced with penalty"

            1e12
        else
            errs

    member this.Solve(maxIter) =
        if maxIter > 0 then
            let mapping = ResizeArray()
            let initial: ResizeArray<float> = ResizeArray()

            for i in 0 .. _points.Length - 1 do
                for j in 0 .. BEZIER_ARGS - 1 do
                    if _points.[i].fit.[j] then
                        mapping.Add((i, j))
                        initial.Add(_points.[i].arr.[j])
#if FABLE_COMPILER
            let objectiveFunction (x: float[]) =
                for i in 0 .. x.Length - 1 do
                    let (index1, index2) = mapping.[i]
                    _points.[index1].arr.[index2] <- x[i]

                if isClosed then
                    _points.[_points.Length - 1].th <- _points.[0].th

                this.computeErr ()

            let param = createObj [ "maxIterations" ==> maxIter ]
            let best = nelderMead objectiveFunction (Array.ofSeq initial) param

            for i in 0 .. best.Length - 1 do
                let (index1, index2) = mapping.[i]
                _points.[index1].arr.[index2] <- best[i]
#else
            let minimiser: Optimization.NelderMeadSimplex =
                Optimization.NelderMeadSimplex(1e-5, maxIter)

            let objectiveFunction (x: Vector<float>) =
                assert (x.Count = mapping.Count)

                for i in 0 .. x.Count - 1 do
                    let (index1, index2) = mapping.[i]
                    _points.[index1].arr.[index2] <- x[i]

                this.computeErr ()

            let objModel = Optimization.ObjectiveFunction.Value(objectiveFunction)

            let best = minimiser.FindMinimum(objModel, DenseVector.ofArray (initial.ToArray()))

            let resultVec =
                if best.MinimizingPoint |> Seq.exists Double.IsNaN then
                    if this.debug then
                        printfn "Optimization returned NaNs! Falling back to initial."

                    DenseVector.ofArray (initial.ToArray())
                else
                    best.MinimizingPoint

            for i in 0 .. resultVec.Count - 1 do
                let (index1, index2) = mapping.[i]
                _points.[index1].arr.[index2] <- resultVec.[i]
#endif

// DSpline handles general sequence of lines & curves, including corners.
type DSpline(ctrlPts, isClosed) =
    member this.ctrlPts: DControlPoint array = ctrlPts
    member this.isClosed = isClosed



    member this.solveAndRenderTuple(maxIter, flatness, debug, showComb) =
        let length = ctrlPts.Length - if this.isClosed then 0 else 1

        // Implement LineToCurve and CurveToLine as Corners with fixed tangent theta
        // determined from the Line
        // for i in 0..length do
        //     let ptI = ctrlPts.[i % ctrlPts.Length]
        //     if ptI.ty = SplinePointType.LineToCurve || ptI.ty = SplinePointType.CurveToLine then
        //         assert (ptI.th.IsNone)
        //         let ptJ, dp =
        //             if ptI.ty = SplinePointType.LineToCurve then
        //                 (ctrlPts.[i-1]), ptI - (ctrlPts.[i-1])
        //             else
        //                 ctrlPts.[(i+1) % ctrlPts.Length], ctrlPts.[(i+1) % ctrlPts.Length] - ptI
        //         let th = atan2 dp.y dp.x
        //         ptI.th <- Some th
        //         ptJ.th <- Some th

        // First point
        let path = BezPath()
        let combPath = BezPath()
        let pt0 = ctrlPts.[0]
        path.moveto (pt0.x.Value, pt0.y.Value)

        // Process all segments between points
        let mutable i = 0

        while i < length do
            let ptI = ctrlPts.[i]
            let ptI1 = ctrlPts.[(i + 1) % ctrlPts.Length]

            // Skip if points are coincident
            if
                (ptI.x.IsSome
                 && ptI.y.IsSome
                 && ptI1.x.IsSome
                 && ptI1.y.IsSome
                 && ptI.x.Value = ptI1.x.Value
                 && ptI.y.Value = ptI1.y.Value)
            then
                i <- i + 1
            // Curve if either point is Smooth or Curve to/from Line , or if either has a tangent
            // Straight line if both points are corners and have no tangents
            else if
                (ptI.ty = SplinePointType.Corner
                 && ptI1.ty = SplinePointType.Corner
                 && ptI.th.IsNone
                 && ptI1.th.IsNone)
            // || ptI.ty = SplinePointType.CurveToLine
            // || ptI1.ty = SplinePointType.LineToCurve
            then
                path.lineto (ptI1.x.Value, ptI1.y.Value)
                i <- i + 1

            else
                // Find a curved section with Smooth points
                // i.e. stop if Corner or Curve to/from Line
                let mutable j = i + 1
                let mutable break_ = false

                let innerPts =
                    ptI
                    :: [ while j <= length && not break_ do
                             let ptJ = ctrlPts.[j % ctrlPts.Length]
                             yield (ptJ)
                             j <- j + 1

                             if ptJ.ty = SplinePointType.Corner then
                                 break_ <- true ]

                let solver =
                    Solver(Array.ofList innerPts, this.isClosed && innerPts.Length - 1 = length, flatness, debug)

                solver.initialise ()
                solver.Solve(maxIter)

                let bezPts = solver.points ()

                for k in 0 .. bezPts.Length - 2 do
                    let p1 = bezPts.[k]
                    let p2 = bezPts.[k + 1]
                    let cp1x, cp1y = p1.rpt().x, p1.rpt().y
                    let cp2x, cp2y = p2.lpt().x, p2.lpt().y
                    path.curveto (cp1x, cp1y, cp2x, cp2y, p2.x, p2.y)

                    if showComb then
                        // Render curvature comb
                        let bez = CubicBez([| p1.x; p1.y; cp1x; cp1y; cp2x; cp2y; p2.x; p2.y |])
                        let COMB_STEPS = 20
                        let SCALE = 2000.0 // Adjusted scale for visibility

                        for s in 0..COMB_STEPS do
                            let t = float s / float COMB_STEPS
                            let k = bez.curvature t
                            let pt = bez.eval t
                            let d = bez.deriv t
                            // Normal vector: (-dy, dx) normalized
                            let normal = { x = -d.y; y = d.x }
                            let nLen = normal.norm ()

                            if nLen > 1e-6 then
                                let n =
                                    { x = normal.x / nLen
                                      y = normal.y / nLen }
                                // Comb line from pt to pt + k * SCALE * n
                                let endPt =
                                    { x = pt.x + k * SCALE * n.x
                                      y = pt.y + k * SCALE * n.y }

                                path.moveto (p2.x, p2.y) // Move main path back to end point (redundant if curveto did it, but safe)

                                combPath.moveto (pt.x, pt.y)
                                combPath.lineto (endPt.x, endPt.y)

                if debug then
                    printfn "DSpline solved"

                    for p in solver.points () do
                        printfn "%s" (p.tostring ())

                i <- j - 1

        if this.isClosed then
            path.closepath ()

        (path.tostringlist (), combPath.tostringlist ())


// member this.renderSvg show_tangents =
//     let path = BezPath()
//     if ctrlPts.Length = 0 then "" else
//     let pt0 = ctrlPts.[0] in path.moveto(pt0.pt.x, pt0.pt.y)
//     let length = ctrlPts.Length - if this.isClosed then 0 else 1
//     for i in 0..length-1 do
//         path.mark i
//         let ptI = this.pt(i, 0)
//         let ptI1 = this.pt(i + 1, 0)
//         let dx = ptI1.pt.x - ptI.pt.x
//         let dy = ptI1.pt.y - ptI.pt.y
//         let chth = atan2 dy dx
//         let chord = hypot(dy, dx)
//         let th0 = mod2pi(ptI.rTh - chth)
//         let th1 = mod2pi(chth - ptI1.lTh)
//         // Apply curvature blending
//         let k0 = Option.map (fun k->k*chord) ptI.kBlend
//         let k1 = Option.map (fun k->k*chord) ptI1.kBlend
//         let render = this.curve.render4(th0, th1, k0, k1)
//         let c =
//             [|
//                 for j in 0..render.Length-1 do
//                     let pt = render.[j]
//                     yield ptI.pt.x + dx * pt.x - dy * pt.y
//                     yield ptI.pt.y + dy * pt.x + dx * pt.y
//                 yield ptI1.pt.x
//                 yield ptI1.pt.y
//             |]
//         for j in 0..6..c.Length-1 do
//             path.curveto(c.[j], c.[j + 1], c.[j + 2], c.[j + 3], c.[j + 4], c.[j + 5])
//     if this.isClosed then
//         path.closepath()

//     //terryspitz: also render tangents in SVG
//     if show_tangents then
//         for i in 1..length-1 do
//             path.mark i
//             let ptI = this.pt(i, 0)
//             let offset = 100.
//             // path.moveto(ptI.pt.x, ptI.pt.y)
//             // path.lineto(ptI.pt.x + offset*cos(-ptI1.lTh), ptI.pt.y + offset*sin(-ptI1.lTh))
//             path.moveto(ptI.pt.x, ptI.pt.y)
//             path.lineto(ptI.pt.x + offset*cos(ptI.lTh), ptI.pt.y + offset*sin(ptI.lTh))
//     path.tostring()
