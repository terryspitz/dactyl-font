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


/// Compute arm length for a cubic Bézier endpoint using the Bespoke Spline formula
/// from Raph Levien's paper (https://spline.technology/paper1.pdf).
/// Given tangent angles th0, th1 relative to the chord, returns the normalized arm length.
/// This derives from the `myCubic` function in curves.fs — the key insight being that
/// arm lengths are *determined* by tangent angles, not free variables.
let bespokeArmLength (th0: float) (th1: float) =
    let offset = 0.3 * sin (th1 * 2. - 0.4 * sin (th1 * 2.))
    let scale = 1.0 / (3. * 0.8)
    scale * (cos (th0 - offset) - 0.2 * cos (3. * (th0 - offset)))


/// Compute both arm lengths for a segment given tangent angles relative to the chord.
/// Returns (armLen0, armLen1) scaled by chord length.
let bespokeArmLengths (th_out: float) (th_in_next: float) (chordAngle: float) (chordLen: float) =
    let th0 = norm (th_out - chordAngle)   // tangent angle at start, relative to chord
    let th1 = norm (chordAngle - th_in_next)  // tangent angle at end, relative to chord
    let arm0 = bespokeArmLength th0 th1
    let arm1 = bespokeArmLength th1 th0
    (arm0 * chordLen, arm1 * chordLen)


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

type Solver(ctrlPts: DControlPoint array, isClosed: bool, flatness: float, debug: bool, ?useAnalyticalArms: bool) =
    let _points: BezierPoint array = Array.init ctrlPts.Length (fun _ -> BezierPoint())
    let STEPS = 8
    let _ks = Array.create (STEPS + 1) 0.0
    let _dists = Array.create (STEPS + 1) 0.0
    let _segmentStartK = Array.create ctrlPts.Length 0.0
    let _segmentEndK = Array.create ctrlPts.Length 0.0
    let _segmentStartIdx = Array.create ctrlPts.Length 0
    let _segmentEndIdx = Array.create ctrlPts.Length 0
    let _useAnalyticalArms = defaultArg useAnalyticalArms false

    member this.ctrlPts = ctrlPts
    member this.isClosed = isClosed
    member val startTh: float option = None with get, set
    member val endTh: float option = None with get, set
    member this.points() = _points
    member this.flatness = flatness
    member this.debug = debug
    member this.useAnalyticalArms = _useAnalyticalArms

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

    /// Recompute arm lengths (ld, rd) analytically from current tangent angles using
    /// Levien's Bespoke Spline formula. Called when useAnalyticalArms is enabled.
    member this.recomputeArmLengths() =
        for i in 0 .. _points.Length - 2 do
            let p1 = _points.[i]
            let p2 = _points.[i + 1]
            let dx = p2.x - p1.x
            let dy = p2.y - p1.y
            let chordLen = sqrt (dx * dx + dy * dy)

            if chordLen > 1e-4 then
                let chordAngle = atan2 dy dx
                let arm0, arm1 = bespokeArmLengths p1.th_out p2.th_in chordAngle chordLen
                // Clamp to [0, chordLen] — a negative arm would flip the CP to the wrong side of the
                // endpoint, giving a cusp/loop. Clamping to 0 produces a straight-ish segment which
                // is a safer fallback than either the bespoke negative or a stale previous value.
                let clampArm a = max 0.0 (min chordLen a)
                p1.rd <- clampArm arm0
                p2.ld <- clampArm arm1

    member this.computeErr() =
        // Piecewise Euler spiral fitting
        // Fit a line to the curvature k(l) for EACH segment separately.
        // Then enforce continuity of k between segments.

        // When using analytical arms, recompute ld/rd from current tangent angles
        // before evaluating error. This keeps arm lengths consistent with the
        // Bespoke Spline curve family rather than treating them as free variables.
        if _useAnalyticalArms then
            this.recomputeArmLengths()

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

                    // 2. Penalty for high variation in curvature (flatness)
                    totalErr <- totalErr + abs m * flatness

                    // Calculate start and end curvature for continuity
                    // k(s) = m*s + c. Start is s=0 (c), End is s=max_dist
                    let startK = c
                    let endK = m * max_dist + c

                    // Store start/end indices for continuity check
                    _segmentStartK.[segmentCount] <- startK
                    _segmentEndK.[segmentCount] <- endK
                    _segmentStartIdx.[segmentCount] <- i
                    _segmentEndIdx.[segmentCount] <- i + 1
                    segmentCount <- segmentCount + 1

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
                        // When using analytical arm lengths, exclude ld/rd from optimization.
                        // They are derived from tangent angles via bespokeArmLengths().
                        elif _useAnalyticalArms && (j = int BezierIndex.Ld || j = int BezierIndex.Rd) then
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

    /// Fast iterative solver inspired by Levien's Bespoke Spline approach.
    /// Instead of Nelder-Mead over all parameters, this adjusts tangent angles
    /// iteratively to achieve curvature continuity at joins. Arm lengths are
    /// computed analytically from tangent angles.
    /// This is O(n) per iteration vs O(n²) for Nelder-Mead, and converges in ~10-20 iterations.
    member this.SolveIterative(maxIter) =
        if _points.Length < 2 then () else

        let CONVERGED_ERR = 1e-6

        for iter in 0 .. maxIter - 1 do
            // 1. Recompute arm lengths from current tangent angles
            this.recomputeArmLengths()

            // 2. Compute endpoint curvatures for each segment
            let segCurvatures = Array.init (_points.Length - 1) (fun i ->
                let p1 = _points.[i]
                let p2 = _points.[i + 1]
                let chordLen = (p2.vec - p1.vec).norm()
                if chordLen < 1e-4 then (0.0, 0.0)
                else
                    let p0 = (p1.x, p1.y)
                    let cp1 = (p1.rpt().x, p1.rpt().y)
                    let cp2 = (p2.lpt().x, p2.lpt().y)
                    let p3 = (p2.x, p2.y)
                    let startK = getCurvature p0 cp1 cp2 p3 0.0
                    let endK = getCurvature p0 cp1 cp2 p3 1.0
                    (startK, endK))

            // 3. Adjust tangent angles at interior points to reduce curvature discontinuity
            let mutable totalGap = 0.0

            for i in 1 .. _points.Length - 2 do
                if ctrlPts.[i].ty = SplinePointType.Smooth && _points.[i].fit_th_in then
                    // Curvature gap at this point: end of segment (i-1) vs start of segment i
                    let (_, endK_prev) = segCurvatures.[i - 1]
                    let (startK_next, _) = segCurvatures.[i]
                    let gap = startK_next - endK_prev

                    // Estimate derivative of gap w.r.t. tangent angle via finite differences
                    let epsilon = 1e-4
                    let origTh = _points.[i].th_in

                    _points.[i].th_in <- origTh + epsilon
                    _points.[i].th_out <- origTh + epsilon
                    this.recomputeArmLengths()

                    let p1prev = _points.[i - 1]
                    let p1 = _points.[i]
                    let p2next = _points.[min (i + 1) (_points.Length - 1)]

                    let endK_prev_p =
                        let p0 = (p1prev.x, p1prev.y)
                        let cp1 = (p1prev.rpt().x, p1prev.rpt().y)
                        let cp2 = (p1.lpt().x, p1.lpt().y)
                        let p3 = (p1.x, p1.y)
                        getCurvature p0 cp1 cp2 p3 1.0

                    let startK_next_p =
                        let p0 = (p1.x, p1.y)
                        let cp1 = (p1.rpt().x, p1.rpt().y)
                        let cp2 = (p2next.lpt().x, p2next.lpt().y)
                        let p3 = (p2next.x, p2next.y)
                        getCurvature p0 cp1 cp2 p3 0.0

                    let gap_p = startK_next_p - endK_prev_p
                    let dgap = (gap_p - gap) / epsilon

                    // Restore original tangent
                    _points.[i].th_in <- origTh
                    _points.[i].th_out <- origTh

                    // Newton step with damping and clamping for stability. A very small dgap
                    // combined with a non-zero gap would otherwise send the tangent off to
                    // absurd values (which is what the "off by 90°" symptom likely is).
                    if abs dgap > 1e-10 then
                        let rawStep = gap / dgap
                        let MAX_STEP = 0.5  // radians per iter (~28.6°)
                        let step = max -MAX_STEP (min MAX_STEP rawStep)
                        let damping = min 1.0 (tanh (0.25 * float (iter + 1)))
                        _points.[i].th_in <- norm (origTh + damping * step)
                        _points.[i].th_out <- norm (origTh + damping * step)

                    totalGap <- totalGap + abs gap

            // 4. Recompute arm lengths after tangent adjustment
            this.recomputeArmLengths()

            if this.debug && iter % 5 = 0 then
                printfn "SolveIterative iter %d: totalGap=%f" iter totalGap

            if totalGap < CONVERGED_ERR then
                if this.debug then
                    printfn "SolveIterative converged at iter %d" iter
                () // break not available, but totalGap check means no more adjustments


/// Apply curvature blending at smooth joins with fixed tangents.
/// Based on Levien's Bespoke Spline paper: when a point has a fixed tangent,
/// endpoint curvatures from adjacent segments may not match. Blending adjusts
/// the arm lengths to smooth the curvature transition, using the harmonic mean
/// of the two endpoint curvatures (from Spline2.computeCurvatureBlending).
let applyCurvatureBlending (bezPts: BezierPoint array) (ctrlPts: DControlPoint array) =
    for i in 1 .. bezPts.Length - 2 do
        if ctrlPts.[i].ty = SplinePointType.Smooth && ctrlPts.[i].th_in.IsSome then
            // This is a smooth point with a fixed tangent — curvature blending applies
            let p_prev = bezPts.[i - 1]
            let p = bezPts.[i]
            let p_next = bezPts.[i + 1]

            // Compute endpoint curvatures from adjacent segments
            let endK_prev =
                let p0 = (p_prev.x, p_prev.y)
                let cp1 = (p_prev.rpt().x, p_prev.rpt().y)
                let cp2 = (p.lpt().x, p.lpt().y)
                let p3 = (p.x, p.y)
                getCurvature p0 cp1 cp2 p3 1.0

            let startK_next =
                let p0 = (p.x, p.y)
                let cp1 = (p.rpt().x, p.rpt().y)
                let cp2 = (p_next.lpt().x, p_next.lpt().y)
                let p3 = (p_next.x, p_next.y)
                getCurvature p0 cp1 cp2 p3 0.0

            // If curvatures have same sign and are non-trivial, blend via harmonic mean
            if sign endK_prev = sign startK_next && abs endK_prev > 1e-6 && abs startK_next > 1e-6 then
                let blendedK = 2.0 / (1.0 / endK_prev + 1.0 / startK_next)

                // Adjust arm lengths to better match the blended curvature.
                // Scale each arm by ratio of blended curvature to its original curvature.
                let ratioL = if abs endK_prev > 1e-10 then blendedK / endK_prev else 1.0
                let ratioR = if abs startK_next > 1e-10 then blendedK / startK_next else 1.0

                // Clamp ratio to avoid extreme distortions
                let clamp lo hi v = max lo (min hi v)
                let ratioL = clamp 0.5 2.0 ratioL
                let ratioR = clamp 0.5 2.0 ratioR

                // Arm length affects curvature inversely — scale by sqrt of ratio for gentler adjustment
                p.ld <- p.ld * sqrt ratioL
                p.rd <- p.rd * sqrt ratioR


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
    /// When useAnalyticalArms=true, arm lengths are derived from tangent angles using
    /// Levien's Bespoke Spline formula, reducing the optimization parameter space.
    /// When useIterativeSolver=true, uses the fast iterative solver instead of Nelder-Mead.
    member this.solveSection(innerPts: DControlPoint list, length: int, maxIter, flatness, debug,
                             ?useAnalyticalArms: bool, ?useIterativeSolver: bool) =
        let analyticalArms = defaultArg useAnalyticalArms false
        let iterativeSolver = defaultArg useIterativeSolver false
        let solver =
            Solver(Array.ofList innerPts, isClosed && innerPts.Length - 1 = length, flatness, debug, analyticalArms)

        solver.initialise ()

        // SolveIterative only adjusts tangent angles; it cannot optimise auto x/y coords.
        // If any control point has x or y set to None, fall back to Nelder-Mead (which
        // respects fit_x / fit_y) — still benefiting from analytical arm lengths when
        // analyticalArms=true (Solve excludes ld/rd from the parameter space in that case).
        let hasAutoXY =
            innerPts |> List.exists (fun p -> p.x.IsNone || p.y.IsNone)

        try
            if iterativeSolver && analyticalArms && not hasAutoXY then
                solver.SolveIterative(maxIter)
            else
                solver.Solve(maxIter)
        with _ ->
            () // keep whatever state the optimizer had when it stopped

        solver

    member this.solveAndGetPoints(maxIter, flatness, debug, ?useAnalyticalArms, ?useIterativeSolver) : BezierPoint[] =
        /// Returns one BezierPoint per ctrlPts entry with solved x, y, th values.
        let analyticalArms = defaultArg useAnalyticalArms false
        let iterativeSolver = defaultArg useIterativeSolver false
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
                let solver = this.solveSection (innerPts, length, maxIter, flatness, debug, analyticalArms, iterativeSolver)
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

        // Apply curvature blending at smooth joins with fixed tangents (Bespoke Spline paper)
        if analyticalArms then
            applyCurvatureBlending result ctrlPts

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

    member this.solveAndRenderSvg(maxIter, flatness, debug, showComb, showTangents,
                                   ?useAnalyticalArms, ?useIterativeSolver) =
        let bezPts = this.solveAndGetPoints (maxIter, flatness, debug,
                        defaultArg useAnalyticalArms false,
                        defaultArg useIterativeSolver false)
        this.renderFromPoints(bezPts, showComb, showTangents)

    member this.solveAndRenderFull(maxIter, flatness, debug, showComb, showTangents,
                                    ?useAnalyticalArms, ?useIterativeSolver) =
        let bezPts = this.solveAndGetPoints (maxIter, flatness, debug,
                        defaultArg useAnalyticalArms false,
                        defaultArg useIterativeSolver false)
        let pathSvg, combSvg, tangentSvg = this.renderFromPoints(bezPts, showComb, showTangents)
        (bezPts, pathSvg, combSvg, tangentSvg)

