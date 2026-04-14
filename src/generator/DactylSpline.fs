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


let linear_regression (xs: float array) (ys: float array) count =
    if count = 0 then
        0.0, 0.0, 0.0
    else
        let n = float count
        let mutable sum_x = 0.
        let mutable sum_y = 0.

        for i in 0 .. count - 1 do
            sum_x <- sum_x + xs.[i]
            sum_y <- sum_y + ys.[i]

        let mean_x = sum_x / n
        let mean_y = sum_y / n

        let mutable num = 0.
        let mutable den = 0.

        for i in 0 .. count - 1 do
            let dx = xs.[i] - mean_x
            let dy = ys.[i] - mean_y
            num <- num + dx * dy
            den <- den + dx * dx

        let m = num / den
        let c = mean_y - m * mean_x

        let mutable residuals = 0.

        for i in 0 .. count - 1 do
            let err = ys.[i] - (m * xs.[i] + c)
            residuals <- residuals + err * err

        m, c, residuals


let getCurvature (p0x, p0y) (p1x, p1y) (p2x, p2y) (p3x, p3y) t =
    let t1 = 1.0 - t
    // First derivative
    let c0 = 3.0 * t1 * t1
    let c1 = 6.0 * t1 * t
    let c2 = 3.0 * t * t
    let dx = c0 * (p1x - p0x) + c1 * (p2x - p1x) + c2 * (p3x - p2x)
    let dy = c0 * (p1y - p0y) + c1 * (p2y - p1y) + c2 * (p3y - p2y)

    // Second derivative
    let c0_2 = 6.0 * t1
    let c1_2 = 6.0 * t
    let d2x = c0_2 * (p2x - 2.0 * p1x + p0x) + c1_2 * (p3x - 2.0 * p2x + p1x)
    let d2y = c0_2 * (p2y - 2.0 * p1y + p0y) + c1_2 * (p3y - 2.0 * p2y + p1y)

    let den = dx * dx + dy * dy
    if den < 1e-9 then 0.0
    else (dx * d2y - dy * d2x) / (den ** 1.5)


let getBezPt (p0x, p0y) (p1x, p1y) (p2x, p2y) (p3x, p3y) t =
    let t1 = 1.0 - t
    let c0 = t1 * t1 * t1
    let c1 = 3.0 * t1 * t1 * t
    let c2 = 3.0 * t1 * t * t
    let c3 = t * t * t
    { x = c0 * p0x + c1 * p1x + c2 * p2x + c3 * p3x
      y = c0 * p0y + c1 * p1y + c2 * p2y + c3 * p3y }

type Solver(ctrlPts: DControlPoint array, isClosed: bool, constantCurvature: float, g3Smoothness: float, debug: bool) =
    let _points: BezierPoint array = Array.init ctrlPts.Length (fun _ -> BezierPoint())
    let STEPS = 8
    let _ks = Array.create (STEPS + 1) 0.0
    let _dists = Array.create (STEPS + 1) 0.0
    let _segmentStartK = Array.create ctrlPts.Length 0.0
    let _segmentEndK = Array.create ctrlPts.Length 0.0
    let _segmentStartIdx = Array.create ctrlPts.Length 0
    let _segmentEndIdx = Array.create ctrlPts.Length 0
    let _segmentM = Array.create ctrlPts.Length 0.0
    let _segmentMaxDist = Array.create ctrlPts.Length 0.0
    let _segmentResiduals = Array.create ctrlPts.Length 0.0
    let mutable _lastSegmentCount = 0

    member this.ctrlPts = ctrlPts
    member this.isClosed = isClosed
    member val startTh: float option = None with get, set
    member val endTh: float option = None with get, set
    member this.points() = _points
    member this.constantCurvature = constantCurvature
    member this.g3Smoothness = g3Smoothness
    member this.debug = debug
    member this.lastSegmentCount = _lastSegmentCount
    member this.segmentResiduals = _segmentResiduals
    member this.segmentMaxDist = _segmentMaxDist
    member this.segmentM = _segmentM
    member this.segmentStartIdx = _segmentStartIdx

    member this.initialise() =
        //initialise points
        if this.debug then
            printfn "dspline solver init, constantCurvature: %f, pts: %A" this.constantCurvature ctrlPts

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
            let dpl = point.vec - _points.[max (i - 1) 0].vec //pointing from previous point to this
            let dpr = _points.[min (i + 1) (_points.Length - 1)].vec - point.vec

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
                            let dpl_closed = point.vec - _points.[_points.Length - 2].vec
                            let lth_closed = dpl_closed.atan2 ()
                            averageAngles rth lth_closed
                        elif ctrlPts.Length > 2 then
                            // Extrapolate tangent from first two segments
                            let dpr2 = _points.[2].vec - _points.[1].vec
                            let rth2 = dpr2.atan2 ()
                            norm (rth - 0.5 * norm (rth2 - rth))
                        else
                            rth
                    elif i = ctrlPts.Length - 1 then
                        if isClosed then
                            let dpr_closed = _points.[1].vec - _points.[0].vec
                            let rth_closed = dpr_closed.atan2 ()
                            averageAngles rth_closed lth
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

        // Initialise handle lengths to chord/3 (simple, always positive, good convergence)
        for i in 0 .. _points.Length - 2 do
            let p1 = _points.[i]
            let p2 = _points.[i + 1]
            let chordLen = (p2.vec - p1.vec).norm ()
            p1.rd <- chordLen / 3.0
            p2.ld <- chordLen / 3.0

        for i in 0 .. ctrlPts.Length - 1 do
            let point = _points.[i]

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
        let mutable segmentCount = 0

        for i in 0 .. _points.Length - 2 do
            let point1 = _points.[i]
            let point2 = _points.[i + 1]

            // Skip zero-length segments to avoid NaN/Inf errors
            // Use a small epsilon
            let chordLen = (point1.vec - point2.vec).norm ()

            if chordLen > 1e-4 then
                let p0 = (point1.x, point1.y)
                let p1 = (point1.rpt().x, point1.rpt().y)
                let p2 = (point2.lpt().x, point2.lpt().y)
                let p3 = (point2.x, point2.y)

                let mutable cumm_dist = 0.

                for j in 0 .. STEPS do
                    let t0 = (float j - 0.5) / float STEPS
                    let t1 = float j / float STEPS
                    let t2 = (float j + 0.5) / float STEPS

                    let k = 10000. * getCurvature p0 p1 p2 p3 t1

                    // Sample segment length around t1
                    let segLen =
                        if j = 0 then
                            let p_start = getBezPt p0 p1 p2 p3 0.
                            let p_half = getBezPt p0 p1 p2 p3 (0.5 / float STEPS)
                            let dx = p_half.x - p_start.x
                            let dy = p_half.y - p_start.y
                            sqrt (dx * dx + dy * dy)
                        else
                            let p_low = getBezPt p0 p1 p2 p3 t0
                            let p_high = getBezPt p0 p1 p2 p3 t2
                            let dx = p_high.x - p_low.x
                            let dy = p_high.y - p_low.y
                            sqrt (dx * dx + dy * dy)

                    _ks.[j] <- k
                    _dists.[j] <- cumm_dist
                    cumm_dist <- cumm_dist + segLen

                let max_dist = cumm_dist

                // Fit line to this segment's curvature
                let m, c, residuals = linear_regression _dists _ks (STEPS + 1)

                if Double.IsNaN m || Double.IsNaN c || Double.IsNaN residuals || Double.IsInfinity m || Double.IsInfinity c || Double.IsInfinity residuals then
                    totalErr <- totalErr + 1e9 // Penalty for invalid
                else
                    // 1. Residuals from being an Euler spiral
                    totalErr <- totalErr + residuals

                    // 2. Penalty for high curvature rate (dk/ds), pulling toward circular arc
                    totalErr <- totalErr + abs m * constantCurvature

                    // Calculate start and end curvature for continuity
                    // k(s) = m*s + c. Start is s=0 (c), End is s=max_dist
                    let startK = c
                    let endK = m * max_dist + c

                    // Store start/end curvature and slope for continuity checks
                    _segmentStartK.[segmentCount] <- startK
                    _segmentEndK.[segmentCount] <- endK
                    _segmentM.[segmentCount] <- m
                    _segmentMaxDist.[segmentCount] <- max_dist
                    _segmentResiduals.[segmentCount] <- residuals
                    _segmentStartIdx.[segmentCount] <- i
                    _segmentEndIdx.[segmentCount] <- i + 1
                    segmentCount <- segmentCount + 1

        _lastSegmentCount <- segmentCount

        // 3. Continuity Penalties
        // Penalize jumps in curvature between segments
        for i in 0 .. segmentCount - 1 do
            if i > 0 then
                let startK = _segmentStartK.[i]
                let prevEndK = _segmentEndK.[i - 1]
                let startIdx = _segmentStartIdx.[i]
                let prevEndIdx = _segmentEndIdx.[i - 1]

                // Check if the connection point(s) allow discontinuity (Corner)
                let mutable isJoinSmooth = true

                for k in prevEndIdx .. startIdx do
                    let ty = ctrlPts.[k].ty

                    if ty = SplinePointType.Corner then
                        isJoinSmooth <- false

                if isJoinSmooth then
                    let gap = startK - prevEndK
                    // Weight this heavily so segments join smoothly in curvature
                    totalErr <- totalErr + gap * gap * 10.0

        if isClosed && segmentCount > 0 then
            // Continuity between last and first
            let lastEndK = _segmentEndK.[segmentCount - 1]
            let lastEndIdx = _segmentEndIdx.[segmentCount - 1]
            let firstStartK = _segmentStartK.[0]
            let firstStartIdx = _segmentStartIdx.[0]

            // Check boundary points for Corner
            let mutable isJoinSmooth = true

            for k in lastEndIdx .. _points.Length - 1 do
                let ty = ctrlPts.[k].ty

                if ty = SplinePointType.Corner then
                    isJoinSmooth <- false

            for k in 0 .. firstStartIdx do
                let ty = ctrlPts.[k].ty

                if ty = SplinePointType.Corner then
                    isJoinSmooth <- false

            if isJoinSmooth then
                let gap = firstStartK - lastEndK
                totalErr <- totalErr + gap * gap * 10.0

        // 4. m-consistency penalty: penalise jumps in curvature slope (m) at smooth joins.
        // m is the rate of curvature change with arc length; matching it across segments
        // gives G3-like smoothness (the curvature comb has no kinks at joins).
        // Scale by avgDist² to convert m (k/length) into Δk units, making the penalty
        // comparable in magnitude to the G2 continuity penalty (gap² × 10).
        if g3Smoothness > 0.0 then
            for i in 1 .. segmentCount - 1 do
                let startIdx = _segmentStartIdx.[i]
                let prevEndIdx = _segmentEndIdx.[i - 1]

                // Only apply at smooth joins (mirrors G2 continuity logic above)
                let mutable isJoinSmooth = true

                for k in prevEndIdx .. startIdx do
                    if ctrlPts.[k].ty = SplinePointType.Corner then
                        isJoinSmooth <- false

                if isJoinSmooth then
                    let mGap = _segmentM.[i] - _segmentM.[i - 1]
                    let avgDist = (_segmentMaxDist.[i] + _segmentMaxDist.[i - 1]) / 2.0
                    totalErr <- totalErr + mGap * mGap * avgDist * avgDist * g3Smoothness

            if isClosed && segmentCount > 1 then
                let lastEndIdx = _segmentEndIdx.[segmentCount - 1]
                let firstStartIdx = _segmentStartIdx.[0]
                let mutable isJoinSmooth = true

                for k in lastEndIdx .. _points.Length - 1 do
                    if ctrlPts.[k].ty = SplinePointType.Corner then
                        isJoinSmooth <- false

                for k in 0 .. firstStartIdx do
                    if ctrlPts.[k].ty = SplinePointType.Corner then
                        isJoinSmooth <- false

                if isJoinSmooth then
                    let mGap = _segmentM.[0] - _segmentM.[segmentCount - 1]
                    let avgDist = (_segmentMaxDist.[0] + _segmentMaxDist.[segmentCount - 1]) / 2.0
                    totalErr <- totalErr + mGap * mGap * avgDist * avgDist * g3Smoothness

        // 5. Free-endpoint curvature regularisation.
        // Corner endpoints have no G2 constraint, so the optimizer can settle into
        // different local minima (positive vs. negative start curvature) for nearly
        // identical inputs. Penalising (c * L / 10000)² ≈ (angle_turned)² is
        // scale-independent (units: rad²) and gently prefers lower curvature solutions,
        // making the landscape more unimodal near the free endpoint.
        // Weight 0.5 adds ~0.1-1 rad² of cost — enough to break sign symmetry without
        // over-constraining the shape.
        if segmentCount > 0 then
            let firstStartIdx = _segmentStartIdx.[0]
            if ctrlPts.[firstStartIdx].ty = SplinePointType.Corner then
                let angleTurned = _segmentStartK.[0] * _segmentMaxDist.[0] / 10000.0
                totalErr <- totalErr + angleTurned * angleTurned * 0.5

            let lastSeg = segmentCount - 1
            let lastEndIdx = _segmentEndIdx.[lastSeg]
            if ctrlPts.[lastEndIdx].ty = SplinePointType.Corner then
                let angleTurned = _segmentEndK.[lastSeg] * _segmentMaxDist.[lastSeg] / 10000.0
                totalErr <- totalErr + angleTurned * angleTurned * 0.5

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
        let isColinear th =
            match th with
            | Some angle ->
                match ptI.x, ptI.y, ptI1.x, ptI1.y with
                | Some x0, Some y0, Some x1, Some y1 ->
                    let lineAngle = atan2 (y1 - y0) (x1 - x0)
                    abs (norm (angle - lineAngle)) < 1e-4
                | _ -> false
            | None -> true

        (ptI.ty = SplinePointType.Corner
         && ptI1.ty = SplinePointType.Corner
         && isColinear ptI.th_out
         && isColinear ptI1.th_in)
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
    member this.solveSection(innerPts: DControlPoint list, length: int, maxIter, constantCurvature, g3Smoothness, debug) =
        let solver =
            Solver(Array.ofList innerPts, isClosed && innerPts.Length - 1 = length, constantCurvature, g3Smoothness, debug)

        solver.initialise ()

        try
            solver.Solve(maxIter)
        with _ ->
            () // keep whatever state the optimizer had when it stopped

        solver

    /// Solve a curve section with adaptive subdivision.
    /// If any segment has poor Euler-spiral fit (high normalised residuals), a smooth
    /// midpoint is inserted at the bezier midpoint (t=0.5) and the section is re-solved.
    ///
    /// Scale-independent criterion (analogous to SpiroFs' bend > 1.0):
    ///   sqrt(residuals_i) / (|m_i × dist_i| + ε) > 1.0
    /// Both numerator and denominator are in the same 10000-scaled curvature units,
    /// so the ratio is dimensionless and behaves the same at unit and font coordinate scales.
    /// A ratio > 1.0 means the RMS curvature deviation exceeds the curvature variation
    /// across the segment — a reliable sign that one Euler spiral is a poor fit.
    ///
    /// Returns the final Solver and origMap: origMap.[k] = index in final bezPts for
    /// the k-th position of the ORIGINAL (pre-expansion) innerPts list.
    member private this.solveSectionAdaptive
        (innerPts: DControlPoint list, isSectionClosed: bool, maxIter, cc, g3s, debug, depth)
        : Solver * int[] =
        let innerArr = Array.ofList innerPts

        let solver =
            let s = Solver(innerArr, isSectionClosed, cc, g3s, debug)
            s.initialise ()
            try s.Solve(maxIter) with _ -> ()
            s

        let identityMap = Array.init innerArr.Length id

        if depth >= 2 then
            solver, identityMap
        else
            let bezPts = solver.points ()
            let segCount = solver.lastSegmentCount

            // Find segments whose curvature deviates significantly from linear (Euler spiral).
            //
            // Scale-independent criterion (analogous to SpiroFs' bend > 1.0):
            //   sqrt(residuals) / (|m × d| + ε) > 1.0
            //
            // |m × d| is the total curvature change across the segment (in the same
            // 10000-scaled units as the residuals), so this ratio is dimensionless and
            // does not depend on coordinate scale.  A ratio > 1.0 means the RMS deviation
            // from linear curvature exceeds the curvature variation itself — a clear sign
            // that a single Euler-spiral segment is a poor fit.
            let insertAfter =
                [| 0 .. segCount - 1 |]
                |> Array.choose (fun s ->
                    let r = solver.segmentResiduals.[s]
                    let d = solver.segmentMaxDist.[s]
                    let m = solver.segmentM.[s]
                    let curvatureVariation = abs (m * d) + 1e-3  // ε avoids ÷0 on straight segs
                    if d > 0.0 && sqrt r / curvatureVariation > 1.0 then
                        Some solver.segmentStartIdx.[s]
                    else
                        None)
                |> Set.ofSeq

            if insertAfter.IsEmpty then
                solver, identityMap
            else
                if debug then
                    printfn "adaptive subdivision: inserting %d midpoints at depth %d" (Set.count insertAfter) depth

                let expanded = ResizeArray<DControlPoint>()
                let origToExpanded = Array.create innerArr.Length 0
                let mutable insertions = 0

                for k in 0 .. innerArr.Length - 1 do
                    origToExpanded.[k] <- k + insertions
                    expanded.Add(innerArr.[k])

                    // Insert midpoint after position k if segment k→k+1 needs subdivision
                    if k < innerArr.Length - 1 && insertAfter.Contains(k) then
                        let bp0 = bezPts.[k]
                        let bp1 = bezPts.[k + 1]
                        // Guard against degenerate bezier points
                        if not (Double.IsNaN bp0.x || Double.IsNaN bp1.x || Double.IsNaN bp0.rd || Double.IsNaN bp1.ld) then
                            let p0 = (bp0.x, bp0.y)
                            let p1c = (bp0.rpt().x, bp0.rpt().y)
                            let p2c = (bp1.lpt().x, bp1.lpt().y)
                            let p3 = (bp1.x, bp1.y)
                            let mid = getBezPt p0 p1c p2c p3 0.5
                            expanded.Add(
                                { ty = SplinePointType.Smooth
                                  x = Some mid.x
                                  y = Some mid.y
                                  th_in = None
                                  th_out = None }
                            )
                            insertions <- insertions + 1

                if insertions = 0 then
                    solver, identityMap
                else
                    let expandedList = List.ofSeq expanded
                    let finalSolver, innerToFinal =
                        this.solveSectionAdaptive(expandedList, isSectionClosed, maxIter, cc, g3s, debug, depth + 1)

                    // Compose: original k → origToExpanded[k] → innerToFinal[origToExpanded[k]]
                    let composedMap = Array.init innerArr.Length (fun k -> innerToFinal.[origToExpanded.[k]])
                    finalSolver, composedMap

    member this.solveAndGetPoints(maxIter, constantCurvature, g3Smoothness, debug) : BezierPoint[] =
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
                let dist = sqrt ((ptI1.x.Value - ptI.x.Value) ** 2.0 + (ptI1.y.Value - ptI.y.Value) ** 2.0)

                if Double.IsNaN result.[i].th_out then
                    result.[i].th_out <- lineAngle
                result.[i].rd <- dist / 3.0

                // For start of open curve, initialize th_in to lineAngle too
                if i = 0 && not isClosed && Double.IsNaN result.[i].th_in then
                    result.[i].th_in <- lineAngle

                result.[nextIdx].th_in <- lineAngle
                result.[nextIdx].ld <- dist / 3.0

                // For end of open curve, initialize th_out to lineAngle too

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
                let isSectionClosed = isClosed && innerPts.Length - 1 = length
                let solver, origMap =
                    this.solveSectionAdaptive (innerPts, isSectionClosed, maxIter, constantCurvature, g3Smoothness, debug, 0)
                let bezPts = solver.points ()

                // Copy solver results back.
                // copyCount is the original innerPts length; origMap.[k] maps to the
                // corresponding index in bezPts (which may be larger after subdivision).
                // For closed splines, we must copy the final point back to point 0 too.
                let copyCount = List.length innerPts

                for k in 0 .. copyCount - 1 do
                    let resIdx = (i + k) % ctrlPts.Length
                    let bk = origMap.[k]  // index into expanded bezPts

                    if Double.IsNaN result.[resIdx].x then
                        result.[resIdx].x <- bezPts.[bk].x
                        result.[resIdx].y <- bezPts.[bk].y

                    // Selective copy: only update properties relevant for the segment sides this point is on.
                    // 'th_in' and 'ld' come from the segment END (incoming direction).
                    if k > 0 || (i = 0 && not isClosed) then
                        result.[resIdx].th_in <- bezPts.[bk].th_in
                        result.[resIdx].ld <- bezPts.[bk].ld

                    // 'th_out' and 'rd' come from the segment START (outgoing direction).
                    if k < copyCount - 1 || (resIdx = ctrlPts.Length - 1 && not isClosed) then
                        result.[resIdx].th_out <- bezPts.[bk].th_out
                        result.[resIdx].rd <- bezPts.[bk].rd

                    // Legacy ensure non-NaN th_in/out for open ends (from user)
                    if resIdx = 0 && not isClosed && Double.IsNaN result.[resIdx].th_in then
                        result.[resIdx].th_in <- bezPts.[bk].th_out

                    if
                        resIdx = ctrlPts.Length - 1
                        && not isClosed
                    then
                        result.[resIdx].th_out <- bezPts.[bk].th_in

                i <- j - 1

        // Final pass to ensure all points have non-NaN ld/rd (defaulting to 0 for lines)
        for p in result do
            if Double.IsNaN p.ld then
                p.ld <- 0.

            if Double.IsNaN p.rd then
                p.rd <- 0.

        // Final pass to resolve any remaining NaN th_in/th_out using neighbour angles
        for i in 0 .. result.Length - 1 do
            let p = result.[i]

            if Double.IsNaN p.th_in || Double.IsNaN p.th_out then
                let prev = result.[(i - 1 + result.Length) % result.Length]
                let next = result.[(i + 1) % result.Length]
                // Only use neighbour coords if they are themselves valid
                let fallback =
                    if
                        not (Double.IsNaN prev.x)
                        && not (Double.IsNaN prev.y)
                        && not (Double.IsNaN next.x)
                        && not (Double.IsNaN next.y)
                    then
                        atan2 (next.y - prev.y) (next.x - prev.x)
                    else
                        0.0

                if Double.IsNaN p.th_in then
                    p.th_in <- fallback

                if Double.IsNaN p.th_out then
                    p.th_out <- fallback

        // Validate: throw if any coordinate is still NaN so the caller can handle it cleanly
        for i in 0 .. result.Length - 1 do
            let p = result.[i]

            if
                Double.IsNaN p.x
                || Double.IsNaN p.y
                || Double.IsNaN p.th_in
                || Double.IsNaN p.th_out
                || Double.IsNaN p.ld
                || Double.IsNaN p.rd
            then
                failwithf
                    "NaN in solved BezierPoint[%d]: %s"
                    i
                    (p.tostring ())

        result

    member this.renderFromPoints(bezPts: BezierPoint[], showComb, showTangents) =
        let length = ctrlPts.Length - if isClosed then 0 else 1

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

    member this.solveAndRenderSvg(maxIter, constantCurvature, g3Smoothness, debug, showComb, showTangents) =
        let bezPts = this.solveAndGetPoints (maxIter, constantCurvature, g3Smoothness, debug)
        this.renderFromPoints(bezPts, showComb, showTangents)

    member this.solveAndRenderFull(maxIter, constantCurvature, g3Smoothness, debug, showComb, showTangents) =
        let bezPts = this.solveAndGetPoints (maxIter, constantCurvature, g3Smoothness, debug)
        let pathSvg, combSvg, tangentSvg = this.renderFromPoints(bezPts, showComb, showTangents)
        (bezPts, pathSvg, combSvg, tangentSvg)

