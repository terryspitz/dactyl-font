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
      mutable th_in: float option   // optional incoming tangent angle (from previous segment)
      mutable th_out: float option  // optional outgoing tangent angle (toward next segment)
    }

    static member (-)(lhs, rhs) =
        { x = lhs.x.Value - rhs.x.Value
          y = lhs.y.Value - rhs.y.Value }

let dcp ty x y th =
    { ty = ty
      x = Some x
      y = Some y
      th_in = th
      th_out = th }


type BezierIndex =
    | X = 0
    | Y = 1
    | ThIn = 2
    | ThOut = 3
    | Ld = 4
    | Rd = 5

let BEZIER_ARGS = System.Enum.GetValues(typeof<BezierIndex>).Length

// Internal class for a knot on bezier curve including tangents and distances to control points
type BezierPoint() =
    //     x : float      (0)
    //     y : float      (1)
    //     th_in : float  (2) tangent from previous (left) segment
    //     th_out : float (3) tangent towards next (right) segment
    //     ld : float     (4) distance to left bezier control point
    //     rd : float     (5) distance to right bezier control point
    let _arr = Array.create BEZIER_ARGS nan
    let _fit = Array.create BEZIER_ARGS true

    member this.x
        with get () = _arr.[int BezierIndex.X]
        and set (v) = _arr.[int BezierIndex.X] <- v

    member this.y
        with get () = _arr.[int BezierIndex.Y]
        and set (v) = _arr.[int BezierIndex.Y] <- v

    member this.th_in
        with get () = _arr.[int BezierIndex.ThIn]
        and set (v) = _arr.[int BezierIndex.ThIn] <- v

    member this.th_out
        with get () = _arr.[int BezierIndex.ThOut]
        and set (v) = _arr.[int BezierIndex.ThOut] <- v

    member this.ld
        with get () = _arr.[int BezierIndex.Ld]
        and set (v) = _arr.[int BezierIndex.Ld] <- v

    member this.rd
        with get () = _arr.[int BezierIndex.Rd]
        and set (v) = _arr.[int BezierIndex.Rd] <- v

    member this.fit_x
        with get () = _fit.[int BezierIndex.X]
        and set (v) = _fit.[int BezierIndex.X] <- v

    member this.fit_y
        with get () = _fit.[int BezierIndex.Y]
        and set (v) = _fit.[int BezierIndex.Y] <- v

    member this.fit_th_in
        with get () = _fit.[int BezierIndex.ThIn]
        and set (v) = _fit.[int BezierIndex.ThIn] <- v

    member this.fit_th_out
        with get () = _fit.[int BezierIndex.ThOut]
        and set (v) = _fit.[int BezierIndex.ThOut] <- v

    member this.fit_ld
        with get () = _fit.[int BezierIndex.Ld]
        and set (v) = _fit.[int BezierIndex.Ld] <- v

    member this.fit_rd
        with get () = _fit.[int BezierIndex.Rd]
        and set (v) = _fit.[int BezierIndex.Rd] <- v

    member this.vec = { x = this.x; y = this.y }
    member this.arr = _arr
    member this.fit = _fit

    member this.lpt() =
        { x = this.x - this.ld * cos this.th_in
          y = this.y - this.ld * sin this.th_in }

    member this.rpt() =
        { x = this.x + this.rd * cos this.th_out
          y = this.y + this.rd * sin this.th_out }

    member this.tostring() =
        let fit_str f = if f then "~" else ""

        sprintf
            "BezPt x:%s%.2f y:%s%.2f th_in:%s%.2f th_out:%s%.2f ld:%s%.2f rd:%s%.2f"
            (fit_str this.fit_x)
            this.x
            (fit_str this.fit_y)
            this.y
            (fit_str this.fit_th_in)
            this.th_in
            (fit_str this.fit_th_out)
            this.th_out
            (fit_str this.fit_ld)
            this.ld
            (fit_str this.fit_rd)
            this.rd


// normalise angle to between PI and -PI
let norm th =
    if th > PI then th - PI * 2.0
    else if th < (-PI) then th + PI * 2.0
    else th

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
            printfn "dspline solver init, flatness: %f, pts: %A" this.flatness ctrlPts

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

            match ctrlPt.th_in, ctrlPt.th_out with
            | Some th_i, Some th_o ->
                point.th_in <- th_i
                point.th_out <- th_o
                point.fit_th_in <- false
                point.fit_th_out <- false
            | Some th_i, None ->
                point.th_in <- th_i
                point.fit_th_in <- false
                // Initialise th_out from geometry, leave fittable
                let lth = dpl.atan2 ()
                let rth = dpr.atan2 ()
                point.th_out <- if i < ctrlPts.Length - 1 then rth else lth
            | None, Some th_o ->
                point.th_out <- th_o
                point.fit_th_out <- false
                // Initialise th_in from geometry, leave fittable
                let lth = dpl.atan2 ()
                let rth = dpr.atan2 ()
                point.th_in <- if i > 0 then lth else rth
            | None, None ->
                let lth = dpl.atan2 ()
                let rth = dpr.atan2 ()

                let new_th =
                    if i = 0 then
                        if isClosed then
                            // For closed splines, the 'previous' point for the first point (i=0)
                            // is the point before the final duplicate point (at Length-2).
                            // e.g. [p0; p1; p2; p0] has Length=4, p3 is at index 2 (Length-2).
                            dpl <- point.vec - _points.[_points.Length - 2].vec
                            let lth = dpl.atan2 ()
                            averageAngles rth lth
                        elif ctrlPts.Length > 2 then
                            // Extrapolate tangent from first two segments
                            let dpr2 = _points.[2].vec - _points.[1].vec
                            let rth2 = dpr2.atan2 ()
                            norm (rth - 0.5 * norm (rth2 - rth))
                        else
                            rth
                    elif i = ctrlPts.Length - 1 then
                        if isClosed then
                            dpr <- _points.[1].vec - _points.[0].vec
                            let rth = dpr.atan2 ()
                            averageAngles rth lth
                        elif ctrlPts.Length > 2 then
                            let dpl2 = _points.[i - 1].vec - _points.[i - 2].vec
                            let lth2 = dpl2.atan2 ()
                            norm (lth + 0.5 * norm (lth - lth2))
                        else
                            lth
                    else
                        averageAngles rth lth

                point.th_in <- new_th
                point.th_out <- new_th

            point.ld <- dpl.norm () / 3.
            point.rd <- dpr.norm () / 3.

            if i = 0 then
                point.fit_ld <- false
            elif i = ctrlPts.Length - 1 then
                point.fit_rd <- false

        if this.debug then
            printfn "solver post init:"

            for p in _points do
                printfn "%s" (p.tostring ())

    member this.computeErr() =
        // Piecewise Euler spiral fitting
        // Fit a line to the curvature k(l) for EACH segment separately.
        // Then enforce continuity of k between segments.

        let mutable totalErr = 0.
        let mutable previousEndK = None
        let STEPS = 8

        let segmentFits = ResizeArray()

        for i in 0 .. _points.Length - 2 do
            let point1 = _points.[i]
            let point2 = _points.[i + 1]

            // Skip zero-length segments to avoid NaN/Inf errors
            // Use a small epsilon
            if (point1.vec - point2.vec).norm () > 1e-4 then
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

                let curveSamples =
                    [ for j in 0..STEPS do
                          let t0 = (-0.5 + float j) / (float STEPS)
                          let t1 = (float j) / (float STEPS)
                          let t2 = (0.5 + float j) / (float STEPS)
                          (10000. * bez.curvature t1, (bez.eval t2 - bez.eval t0).norm ()) ]

                let ks, segmentLengths = curveSamples |> List.toArray |> Array.unzip

                let cumm_dists, max_dist =
                    Array.mapFold (fun sum dist -> let acc = sum + dist in (acc, acc)) 0. segmentLengths

                // Fit line to this segment's curvature
                let m, c, residuals = linear_regression cumm_dists ks

                if
                    Double.IsNaN m
                    || Double.IsNaN c
                    || Double.IsNaN residuals
                    || Double.IsInfinity m
                    || Double.IsInfinity c
                    || Double.IsInfinity residuals
                then
                    totalErr <- totalErr + 1e9 // Penalty for invalid
                else
                    // 1. Residuals from being an Euler spiral
                    totalErr <- totalErr + residuals

                    // 2. Penalty for high variation in curvature (flatness)
                    totalErr <- totalErr + abs m * flatness

                    // Calculate start and end curvature for continuity
                    // k(s) = m*s + c. Start is s=0 (c), End is s=max_dist
                    let startK = c
                    let endK = m * max_dist + c

                    // Store start/end indices for continuity check
                    segmentFits.Add(startK, endK, i, i + 1)

        // 3. Continuity Penalties
        // Penalize jumps in curvature between segments
        for i in 0 .. segmentFits.Count - 1 do
            let startK, endK, startIdx, endIdx = segmentFits.[i]

            if i > 0 then
                let _, prevEndK, _, prevEndIdx = segmentFits.[i - 1]

                // Check if the connection point(s) allow discontinuity (Corner)
                // We check all points from prevEndIdx to startIdx (inclusive of boundary points logic)
                // Usually prevEndIdx == startIdx, but if we skipped segments, there might be a gap.
                // If ANY point in the gap (including ends) is a Corner, we break continuity.
                let mutable isJoinSmooth = true

                for k in prevEndIdx..startIdx do
                    let ty = ctrlPts.[k].ty

                    if ty = SplinePointType.Corner then
                        isJoinSmooth <- false

                if isJoinSmooth then
                    let gap = startK - prevEndK
                    // Weight this heavily so segments join smoothly in curvature
                    totalErr <- totalErr + gap * gap * 10.0

        if isClosed && segmentFits.Count > 0 then
            // Continuity between last and first
            let _, lastEndK, _, lastEndIdx = segmentFits.[segmentFits.Count - 1]
            let firstStartK, _, firstStartIdx, _ = segmentFits.[0]

            // Check boundary points for Corner (from lastEndIdx to end of array, and 0 to firstStartIdx)
            let mutable isJoinSmooth = true
            // Check wrap-around gap: lastEndIdx -> end, 0 -> firstStartIdx
            // Actually, logical path is lastEndIdx -> ... -> Length-1 -> loop -> 0 -> ... -> firstStartIdx

            for k in lastEndIdx .. _points.Length - 1 do
                let ty = ctrlPts.[k].ty

                if ty = SplinePointType.Corner then
                    isJoinSmooth <- false

            for k in 0..firstStartIdx do
                let ty = ctrlPts.[k].ty

                if ty = SplinePointType.Corner then
                    isJoinSmooth <- false

            if isJoinSmooth then
                let gap = firstStartK - lastEndK
                totalErr <- totalErr + gap * gap * 10.0

        if Double.IsNaN totalErr || Double.IsInfinity totalErr then
            if this.debug then
                printfn "computeErr returning NaN/Inf, replaced with penalty"

            1e12
        else
            totalErr

    member this.Solve(maxIter) =
        if maxIter > 0 then
            let mapping = ResizeArray()
            let initial: ResizeArray<float> = ResizeArray()

            for i in 0 .. _points.Length - 1 do
                for j in 0 .. BEZIER_ARGS - 1 do
                    if _points.[i].fit.[j] then
                        // For smooth points, th_in and th_out are synchronized.
                        // We only add th_in (index 2) to the mapping.
                        if j = int BezierIndex.ThOut && ctrlPts.[i].ty = SplinePointType.Smooth then
                            ()
                        else
                            mapping.Add((i, j))
                            initial.Add(_points.[i].arr.[j])
#if FABLE_COMPILER
            let objectiveFunction (x: float[]) =
                for i in 0 .. x.Length - 1 do
                    let (index1, index2) = mapping.[i]
                    _points.[index1].arr.[index2] <- x[i]

                    // Synchronize th_out for smooth points
                    if
                        index2 = int BezierIndex.ThIn
                        && ctrlPts.[index1].ty = SplinePointType.Smooth
                        && _points.[index1].fit_th_out
                    then
                        _points.[index1].th_out <- x[i]

                if isClosed then
                    _points.[_points.Length - 1].th_in <- _points.[0].th_in
                    _points.[_points.Length - 1].th_out <- _points.[0].th_out

                this.computeErr ()

            let param = createObj [ "maxIterations" ==> maxIter ]
            let best = nelderMead objectiveFunction (Array.ofSeq initial) param

            for i in 0 .. best.Length - 1 do
                let (index1, index2) = mapping.[i]
                _points.[index1].arr.[index2] <- best[i]

                if
                    index2 = int BezierIndex.ThIn
                    && ctrlPts.[index1].ty = SplinePointType.Smooth
                    && _points.[index1].fit_th_out
                then
                    _points.[index1].th_out <- best[i]
#else
            let minimiser: Optimization.NelderMeadSimplex =
                Optimization.NelderMeadSimplex(1e-5, maxIter)

            let objectiveFunction (x: Vector<float>) =
                assert (x.Count = mapping.Count)

                for i in 0 .. x.Count - 1 do
                    let (index1, index2) = mapping.[i]
                    _points.[index1].arr.[index2] <- x[i]

                    // Synchronize th_out for smooth points
                    if
                        index2 = int BezierIndex.ThIn
                        && ctrlPts.[index1].ty = SplinePointType.Smooth
                        && _points.[index1].fit_th_out
                    then
                        _points.[index1].th_out <- x[i]

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

                if
                    index2 = int BezierIndex.ThIn
                    && ctrlPts.[index1].ty = SplinePointType.Smooth
                    && _points.[index1].fit_th_out
                then
                    _points.[index1].th_out <- resultVec.[i]
#endif

// DactylSpline handles general sequence of lines & curves, including corners.
type DactylSpline(ctrlPts, isClosed) =
    member this.ctrlPts: DControlPoint array = ctrlPts
    member this.isClosed = isClosed

    /// Pre-process LineToCurve/CurveToLine points: fix their tangent to the line direction.
    /// Must be called before segment iteration in both solveAndGetPoints and solveAndRenderTuple.
    member this.preprocessLineTangents() =
        for i in 0 .. ctrlPts.Length - 1 do
            let ptI = ctrlPts.[i]

            if ptI.ty = SplinePointType.LineToCurve && (isClosed || i > 0) then
                let prevIdx = if i = 0 then ctrlPts.Length - 1 else i - 1
                let ptPrev = ctrlPts.[prevIdx]
                let dp = ptI - ptPrev
                let lineAngle = Some(atan2 dp.y dp.x)
                ptI.th_in <- lineAngle
                ptI.th_out <- lineAngle

            if ptI.ty = SplinePointType.CurveToLine && (isClosed || i < ctrlPts.Length - 1) then
                let nextIdx = (i + 1) % ctrlPts.Length
                let ptNext = ctrlPts.[nextIdx]
                let dp = ptNext - ptI
                let lineAngle = Some(atan2 dp.y dp.x)
                ptI.th_in <- lineAngle
                ptI.th_out <- lineAngle

    /// Predicate: true when segment i→i+1 is a straight line (no solver needed).
    member this.isLineSegment(ptI: DControlPoint, ptI1: DControlPoint) =
        (ptI.ty = SplinePointType.Corner
         && ptI1.ty = SplinePointType.Corner
         && ptI.th_out.IsNone
         && ptI1.th_in.IsNone)
        || ptI.ty = SplinePointType.CurveToLine
        || ptI1.ty = SplinePointType.LineToCurve

    /// Collect the run of smooth inner points starting at index i, returning the list and
    /// the exclusive end index j (i.e. the next segment to process starts at j-1).
    member this.collectCurveSection(i: int, length: int) =
        let ptI = ctrlPts.[i]
        let mutable j = i + 1
        let mutable break_ = false

        let innerPts =
            ptI
            :: [ while j <= length && not break_ do
                     let ptJ = ctrlPts.[j % ctrlPts.Length]
                     yield ptJ
                     j <- j + 1

                     if
                         ptJ.ty = SplinePointType.Corner
                         || ptJ.ty = SplinePointType.CurveToLine
                         || ptJ.ty = SplinePointType.LineToCurve
                     then
                         break_ <- true ]

        innerPts, j

    /// Construct and solve a Solver for the given inner points.
    /// If the optimizer hits its iteration limit the best-so-far state is kept (no exception).
    member this.solveSection(innerPts: DControlPoint list, length: int, maxIter, flatness, debug) =
        let solver =
            Solver(Array.ofList innerPts, isClosed && innerPts.Length - 1 = length, flatness, debug)

        solver.initialise ()

        try
            solver.Solve(maxIter)
        with _ ->
            () // keep whatever state the optimizer had when it stopped

        solver

    member this.solveAndGetPoints(maxIter, flatness, debug) : BezierPoint[] =
        /// Returns one BezierPoint per ctrlPts entry with solved x, y, th values.
        let length = ctrlPts.Length - if isClosed then 0 else 1

        this.preprocessLineTangents ()

        // Flip end tangent for open splines to match Spiro "handle direction" interpretation.
        // We do this here so the solver sees the flipped tangent in ctrlPt.th_in.
        if not isClosed && ctrlPts.Length > 1 then
            let lastPt = ctrlPts.[ctrlPts.Length - 1]

            match lastPt.th_in with
            | Some th ->
                lastPt.th_in <- Some(norm (th + PI))
            | None -> ()

            match lastPt.th_out with
            | Some th ->
                lastPt.th_out <- Some(norm (th + PI))
            | None -> ()

        // Result array: one BezierPoint per ctrlPts entry
        let result = Array.init ctrlPts.Length (fun _ -> BezierPoint())

        for i in 0 .. ctrlPts.Length - 1 do
            result.[i].x <- ctrlPts.[i].x |> Option.defaultValue Double.NaN
            result.[i].y <- ctrlPts.[i].y |> Option.defaultValue Double.NaN
            result.[i].th_in <- ctrlPts.[i].th_in |> Option.defaultValue Double.NaN
            result.[i].th_out <- ctrlPts.[i].th_out |> Option.defaultValue Double.NaN

        let mutable i = 0

        while i < length do
            let ptI = ctrlPts.[i]
            let ptI1 = ctrlPts.[(i + 1) % ctrlPts.Length]

            if this.isLineSegment (ptI, ptI1) then
                // Line segment — tangents point along the line
                let lineAngle = atan2 (ptI1.y.Value - ptI.y.Value) (ptI1.x.Value - ptI.x.Value)
                let nextIdx = (i + 1) % result.Length

                if Double.IsNaN result.[i].th_out then
                    result.[i].th_out <- lineAngle

                // For start of open curve, initialize th_in to lineAngle too
                if i = 0 && not isClosed && Double.IsNaN result.[i].th_in then
                    result.[i].th_in <- lineAngle

                result.[nextIdx].th_in <- lineAngle

                // For end of open curve, initialize th_out to lineAngle too
                if
                    i + 1 = ctrlPts.Length - 1
                    && not isClosed
                    && Double.IsNaN result.[nextIdx].th_out
                then
                    result.[nextIdx].th_out <- lineAngle

                result.[nextIdx].x <- ptI1.x.Value
                result.[nextIdx].y <- ptI1.y.Value
                i <- i + 1
            else
                let innerPts, j = this.collectCurveSection (i, length)
                let solver = this.solveSection (innerPts, length, maxIter, flatness, debug)
                let bezPts = solver.points ()

                // Copy solver results back.
                // For closed splines, we must copy the final point back to point 0 too.
                let copyCount = bezPts.Length

                for k in 0 .. copyCount - 1 do
                    let resIdx = (i + k) % ctrlPts.Length

                    if Double.IsNaN result.[resIdx].x then
                        result.[resIdx].x <- bezPts.[k].x
                        result.[resIdx].y <- bezPts.[k].y

                    // Selective copy: only update properties relevant for the segment sides this point is on.
                    // 'th_in' and 'ld' come from the segment END (incoming direction).
                    if k > 0 || (i = 0 && not isClosed) then
                        result.[resIdx].th_in <- bezPts.[k].th_in
                        result.[resIdx].ld <- bezPts.[k].ld

                    // 'th_out' and 'rd' come from the segment START (outgoing direction).
                    if k < bezPts.Length - 1 || (resIdx = ctrlPts.Length - 1 && not isClosed) then
                        result.[resIdx].th_out <- bezPts.[k].th_out
                        result.[resIdx].rd <- bezPts.[k].rd

                    // Legacy ensure non-NaN th_in/out for open ends (from user)
                    if resIdx = 0 && not isClosed && Double.IsNaN result.[resIdx].th_in then
                        result.[resIdx].th_in <- bezPts.[k].th_out

                    if
                        resIdx = ctrlPts.Length - 1
                        && not isClosed
                    then
                        result.[resIdx].th_out <- bezPts.[k].th_in

                i <- j - 1

        // Final pass to ensure all points have non-NaN ld/rd (defaulting to 0 for lines)
        for p in result do
            if Double.IsNaN p.ld then
                p.ld <- 0.

            if Double.IsNaN p.rd then
                p.rd <- 0.

        result

    member this.solveAndRenderSvg(maxIter, flatness, debug, showComb, showTangents) =
        let length = ctrlPts.Length - if isClosed then 0 else 1
        let bezPts = this.solveAndGetPoints (maxIter, flatness, debug)

        let path = BezPath()
        let combPath = BezPath()
        let tangentPath = BezPath()

        path.moveto (bezPts.[0].x, bezPts.[0].y)

        for i in 0 .. length - 1 do
            let p1 = bezPts.[i]
            let p2 = bezPts.[(i + 1) % bezPts.Length]
            let ptI = ctrlPts.[i]
            let ptI1 = ctrlPts.[(i + 1) % ctrlPts.Length]

            // Skip if points are coincident
            if
                not (
                    ptI.x.IsSome
                    && ptI.y.IsSome
                    && ptI1.x.IsSome
                    && ptI1.y.IsSome
                    && ptI.x.Value = ptI1.x.Value
                    && ptI.y.Value = ptI1.y.Value
                )
            then
                if this.isLineSegment (ptI, ptI1) then
                    path.lineto (p2.x, p2.y)

                    if showTangents then
                        let dx = p2.x - p1.x
                        let dy = p2.y - p1.y
                        let lineAngle = atan2 dy dx
                        let dist = sqrt (dx * dx + dy * dy) / 3.0
                        // Draw outgoing tangent from p1
                        tangentPath.moveto (p1.x, p1.y)
                        tangentPath.lineto (p1.x + dist * cos lineAngle, p1.y + dist * sin lineAngle)
                        // Draw incoming tangent to p2
                        tangentPath.moveto (p2.x, p2.y)
                        tangentPath.lineto (p2.x - dist * cos lineAngle, p2.y - dist * sin lineAngle)
                else
                    let cp1x, cp1y = p1.rpt().x, p1.rpt().y
                    let cp2x, cp2y = p2.lpt().x, p2.lpt().y
                    path.curveto (cp1x, cp1y, cp2x, cp2y, p2.x, p2.y)

                    if showComb then
                        // Render curvature comb
                        let bez = CubicBez([| p1.x; p1.y; cp1x; cp1y; cp2x; cp2y; p2.x; p2.y |])
                        let COMB_STEPS = 20
                        let SCALE = 2000.0

                        for s in 0..COMB_STEPS do
                            let t = float s / float COMB_STEPS
                            let kv = bez.curvature t
                            let pt = bez.eval t
                            let d = bez.deriv t
                            let normal = { x = -d.y; y = d.x }
                            let nLen = normal.norm ()

                            if nLen > 1e-6 then
                                let n =
                                    { x = normal.x / nLen
                                      y = normal.y / nLen }

                                let endPt =
                                    { x = pt.x + kv * SCALE * n.x
                                      y = pt.y + kv * SCALE * n.y }

                                combPath.moveto (pt.x, pt.y)
                                combPath.lineto (endPt.x, endPt.y)

                if showTangents && not (this.isLineSegment (ptI, ptI1)) then
                    // For curve segments, draw control point lines
                    if i > 0 || isClosed then
                        let lpt = p1.lpt ()
                        tangentPath.moveto (p1.x, p1.y)
                        tangentPath.lineto (lpt.x, lpt.y)

                    let rpt = p1.rpt ()
                    tangentPath.moveto (p1.x, p1.y)
                    tangentPath.lineto (rpt.x, rpt.y)

                    // Also handle p2's incoming tangent if it's the end of an open curve
                    if not isClosed && i + 1 = ctrlPts.Length - 1 then
                        let lpt2 = p2.lpt ()
                        tangentPath.moveto (p2.x, p2.y)
                        tangentPath.lineto (lpt2.x, lpt2.y)

        if isClosed then
            path.closepath ()

        (path.tostringlist (), combPath.tostringlist (), tangentPath.tostringlist ())


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
