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

//reuse spline-research Vec2, SplinePointType, SplineControlPoint, CubicBez
open Curves
open BezPath
#if FABLE_COMPILER
#else
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
#endif

type DControlPoint = {
    mutable ty : SplinePointType    //Continuity at this point: Smooth, Corner, LineToCurve or CurveToLine
    x : float option                // x coord or None to fit x to 'smoothest' curve
    y : float option                // same for y
    mutable th : float option       // optional tangent theta in direction of next point
    // member this.tostring() =
    //     sprintf "CP ty:%A x:%A y:%A th:%A" this.ty this.x this.y this.th
} with
    static member (-) (lhs, rhs) =
        {x=lhs.x.Value-rhs.x.Value; y=lhs.y.Value-rhs.y.Value}


type ControlPointOut(ty, x, y, lth, rth) = 
    member val ty : SplinePointType = ty with get, set
    member val x : float = x with get
    member val y : float = y with get
    member val th : float = lth with get, set  // angle of tangent towards next point
    // member val lth : float = lth with get, set  // angle of curve to previous point
    // member val rth : float = lth with get, set  // angle of curve to next point

// Internal class for a knot on bezier curve including tangents and distances to control points 
type BezierPoint() =
    //     x : float
    //     y : float
    //     lth : float  // tangent toward previous (left) point
    //     ld : float   // distance to left bezier control point
    //     rth : float  // tangent towards next (right) point
    //     rd : float   // distance to right bezier control point
    let _arr = Array.create 6 nan
    let _fit = Array.create 6 true
    member this.x   with get () = _arr.[0] and set (v) = _arr.[0] <- v
    member this.y   with get () = _arr.[1] and set (v) = _arr.[1] <- v
    member this.lth with get () = _arr.[2] and set (v) = _arr.[2] <- v
    member this.ld  with get () = _arr.[3] and set (v) = _arr.[3] <- v
    member this.rth with get () = _arr.[4] and set (v) = _arr.[4] <- v
    member this.rd  with get () = _arr.[5] and set (v) = _arr.[5] <- v
    member this.fit_x   with get () = _fit.[0] and set (v) = _fit.[0] <- v
    member this.fit_y   with get () = _fit.[1] and set (v) = _fit.[1] <- v
    member this.fit_lth with get () = _fit.[2] and set (v) = _fit.[2] <- v
    member this.fit_ld  with get () = _fit.[3] and set (v) = _fit.[3] <- v
    member this.fit_rth with get () = _fit.[4] and set (v) = _fit.[4] <- v
    member this.fit_rd  with get () = _fit.[5] and set (v) = _fit.[5] <- v

    member this.vec with get () = {x=this.x; y=this.y}
    member this.arr = _arr
    member this.fit = _fit
    member this.lpt() = {x=this.x + this.ld * cos this.lth; y=this.y + this.ld * sin this.lth}
    member this.rpt() =
        let th = if isnan this.lth then this.rth else this.lth
        {x=this.x + this.rd * cos this.rth; y=this.y + this.rd * sin this.rth}
    member this.tostring() =
        sprintf "BP x:%f y:%f lth:%f ld:%f rth:%f rd:%f" this.x this.y this.lth this.ld this.rth this.rd


type Solver(ctrlPts : DControlPoint array, isClosed : bool, debug: bool) =
    let _points : BezierPoint array = Array.zeroCreate ctrlPts.Length
    member this.ctrlPts = ctrlPts
    member this.isClosed = isClosed
    member val startTh : float option = None with get, set
    member val endTh : float option = None with get, set
    member this.points() = _points
    member this.debug = debug

    member this.initialise() =
        //initialise points
        if this.debug then
            printfn "solver init"
            printfn "%A" this.ctrlPts

        for i in 0..ctrlPts.Length - 1 do
            _points.[i] <- BezierPoint()
            let point = _points.[i]
            let ctrlPt = ctrlPts.[i]
            match ctrlPt.x with 
            | Some x -> point.x <- x; point.fit_x <- false
            // possibly: define fit_x/y separately from none, then use x/y for initialisation
            | None -> point.x <- if i=0 then ctrlPts.[i+1].x.Value
                                    elif i=ctrlPts.Length-1 then ctrlPts.[i-1].x.Value
                                    else (ctrlPts.[i-1].x.Value+ctrlPts.[i+1].x.Value)/2.
            //TODO: fit_y
            match ctrlPt.y with | Some y -> point.y <- y; point.fit_y <- false | None -> ()

        //initialise tangent angle/distance
        for i in 0..ctrlPts.Length - 1 do
            let point = _points.[i]
            let ctrlPt = ctrlPts.[i]
            let dpl = point.vec - _points.[max (i-1) 0].vec
            let dpr = _points.[min (i+1) (_points.Length-1)].vec - point.vec

            match ctrlPt.th with 
            | Some th ->
                point.lth <- th + PI; point.fit_lth <- false
                point.rth <- th; point.fit_rth <- false
            | None ->
                let th_est =
                    if i=0 then dpr.atan2()
                    elif i=ctrlPts.Length-1 then dpl.atan2()
                    // else mod2pi (dpl.atan2() + mod2pi (dpr.atan2() - dpl.atan2())/2.)  //average of angles
                    else (dpl.atan2() + dpr.atan2())/2.  //average of angles
                point.lth <- mod2pi (th_est + PI)
                point.rth <- th_est

            if i=0 then
                point.rd <- dpr.norm()/3.
                point.fit_lth <- false
                point.fit_ld <- false
            elif i=ctrlPts.Length-1 then
                point.ld <- dpl.norm()/3.
                point.fit_rth <- false
                point.fit_rd <- false
            else
                point.ld <- dpl.norm()/3.
                point.rd <- dpr.norm()/3.

        if this.debug then
            printfn "solver post init"
            printfn "%A" [|for p in _points do p.tostring()|]

    member this.computeErr() =
        // Sample the curvature and distance (arc length) along each bezier.
        // Euler spiral (like spiro spline) wants curvature k linear in curve length l
        // So treat l as an x-coord and k as a y-coord and fit a line to the data, then measure residuals from that line and minimise
        // Add error for curvature discontinuity between segments (tangents are consistent by construction)
        let mutable prev_end = nan
        let mutable errs = 0.
        for i in 0.._points.Length - 2 do
            let point1 = _points.[i]
            let point2 = _points.[i+1]
            let bez = CubicBez(
                [|  point1.x; point1.y; 
                    point1.rpt().x; point1.rpt().y;
                    point2.lpt().x; point2.lpt().y;
                    point2.x; point2.y
                |])
            let STEPS = 8
            let ks, dists =
                [|
                    for j in 0..STEPS do
                        let t0 = (-0.5 + float j) / (float STEPS)
                        let t1 = (float j) / (float STEPS)
                        let t2 = (0.5 + float j) / (float STEPS)
                        (bez.curvature(t1), (bez.eval(t2)-bez.eval(t0)).norm())
                |] |> Array.unzip
            let n = float (STEPS + 1)
            let cumm_dists, max_dist =
                Array.mapFold (fun sum dist -> let acc = sum+dist in (acc,acc)) 0. dists
            
            let linear_regression xs ys =
                let mean_x = (Array.sum xs) / n
                let sum_y = Array.sum ys
                let mean_y = sum_y / n
                // best fit k_i = m * dist_i + c using https://en.wikipedia.org/wiki/Simple_linear_regression
                let m = (Array.sumBy (fun (x,y)->(y-mean_y)*(x-mean_x)) (Array.zip xs ys))
                        / (Array.sumBy (fun x->(x-mean_x)*(x-mean_x)) xs)
                let c = (mean_y - m * mean_x)
                let residuals = (Array.zip ys xs |> Array.sumBy (fun (k,d)->let err = k-(m*d+c) in err*err))
                m, c, residuals

            let m, c, residuals = linear_regression cumm_dists ks
            errs <- errs + residuals
            // add error term for mismatch between end of previous bezier and start of this
            if i>0 then
                let err = c - prev_end
                errs <- errs + err*err
            prev_end <- m * cumm_dists.[STEPS] + c
        // assert not (isnan errs)
        errs
            
    /// Step towards a curvature continuous solution.
    /// Newton's method to minimise error: adjust each parameter by a small amount and see if error decreases
    member this.iter(iter) =
        let n = this.ctrlPts.Length
        
        //terryspitz: correction to match start/end tangents on closed curves
        if this.isClosed && this.startTh.IsNone && this.endTh.IsNone then
            let avgth = (_points.[0].lth + _points.[n - 1].rth) / 2.
            _points.[0].lth <- avgth
            _points.[n - 1].rth <- avgth

        // if points.Length < 3 then 0. else

        let mutable absErr = 0.
        let mutable newArr = [|for i in 0.._points.Length-1 do Array.create 6 nan|]
        let err = this.computeErr()
        let epsilon = 1e-5
        for i in 0.._points.Length-1 do
            for j in 0..5 do
                if _points.[i].fit.[j] then
                    let value = _points.[i].arr.[j]
                    _points.[i].arr.[j] <- value + epsilon
                    let errp = this.computeErr()
                    _points.[i].arr.[j] <- value
                    let derr = (errp - err) / epsilon
                    newArr.[i].[j] <- _points.[i].arr.[j] - err / derr  //newtons method, towards zero
        for i in 0.._points.Length-1 do
            for j in 0..5 do
                if _points.[i].fit.[j] then
                    _points.[i].arr.[j] <- newArr.[i].[j]
            if _points.[i].ld < 0.1 then _points.[i].ld <- 0.1
            if _points.[i].rd < 0.1 then _points.[i].rd <- 0.1

        if this.debug then
            printfn "%A" [|for p in _points do p.tostring()|]
            printfn "abs err %f at iter %d" err iter
        err

/// Spline handles more general cases, including corners.
type DSpline (ctrlPts, isClosed) =
    member this.ctrlPts : DControlPoint array = ctrlPts
    member this.isClosed = isClosed

    member this.solve(maxIter, debug) =
        let length = this.ctrlPts.Length - if this.isClosed then 0 else 1

        // Implement LineToCurve and CurveToLine as Corners with fixed tangent theta
        // determined from the Line
        for i in 0..length do
            let ptI = this.ctrlPts.[i % this.ctrlPts.Length]
            if ptI.ty = SplinePointType.LineToCurve || ptI.ty = SplinePointType.CurveToLine then
                assert (ptI.th.IsNone)
                let dp = 
                    if ptI.ty = SplinePointType.LineToCurve then 
                        ptI - (this.ctrlPts.[i-1])
                    else
                        this.ctrlPts.[(i+1) % this.ctrlPts.Length] - ptI
                ptI.ty <- SplinePointType.Corner
                ptI.th <- Some (atan2 dp.y dp.x)

        // First point
        let path = BezPath()
        let pt0 = this.ctrlPts.[0]
        path.moveto(pt0.x.Value, pt0.y.Value)

        // Process all segments between points
        let mutable i = 0
        while i < length do
            let ptI = this.ctrlPts.[i]
            let ptI1 = this.ctrlPts.[(i+1) % this.ctrlPts.Length]
            // Curve if either point is Smooth or Curve to/from Line , or if either has a tangent
            // Straight line if both points are corners and have no tangents
            if ptI.ty = SplinePointType.Corner 
                && ptI1.ty = SplinePointType.Corner 
                && ptI.th.IsNone && ptI1.th.IsNone then
                path.lineto(ptI1.x.Value, ptI1.y.Value)
                i <- i+1

            else
                // Find a curved section with Smooth points
                // i.e. stop if Corner or Curve to/from Line
                let mutable j = i + 1
                let mutable break_ = false
                let innerPts =
                    ptI ::
                    [
                        while j < length + 1 && not break_ do
                            let ptJ = this.ctrlPts.[j % this.ctrlPts.Length]
                            yield (ptJ)
                            j <- j + 1
                            if ptJ.ty <> SplinePointType.Smooth then
                                break_ <- true
                    ]
                let solver = Solver(
                                Array.ofList innerPts, 
                                this.isClosed && innerPts.Length-1 = length,
                                debug)
                solver.initialise()
                let CONVERGED_ERR = 1e-3
#if FABLE_COMPILER
                let mutable iter = 0
                while solver.iter iter > CONVERGED_ERR && iter < maxIter do
                    iter <- iter + 1
#else
                let minimiser = Optimization.LevenbergMarquardtMinimizer(
                    // CONVERGED_ERR,
                    // CONVERGED_ERR,
                    // CONVERGED_ERR,
                    maximumIterations=maxIter)
                let mapping = ResizeArray()
                let lowerBound: ResizeArray<float> = ResizeArray()
                let upperBound: ResizeArray<float> = ResizeArray()
                let initial: ResizeArray<float> = ResizeArray()
                for i in 0..solver.points().Length-1 do
                    for j in 0..5 do
                        if solver.points().[i].fit.[j] then
                            mapping.Add((i, j))
                            lowerBound.Add(0.0)
                            if j = 2 || j = 4 then
                                upperBound.Add(2.0 * PI)
                            else
                                upperBound.Add(1e10)
                            initial.Add(solver.points().[i].arr.[j])

                /// Adjust params to minimise least squares error for x and y, which we'll just set to 0, 0
                let objectiveFunction (param: Vector<float>) (x: Vector<float>) =
                        assert (param.Count = mapping.Count)
                        for i in 0..param.Count-1 do
                            let (index1, index2) = mapping.[i]
                            solver.points().[index1].arr.[index2] <- param[i]
                        DenseVector.ofArray [|solver.computeErr()|]
                let objModel = Optimization.ObjectiveFunction.NonlinearModel(
                    objectiveFunction,
                    DenseVector.ofArray [|0.|],
                    DenseVector.ofArray [|0.|])
                let best = minimiser.FindMinimum(
                    objModel,
                    DenseVector.ofArray (initial.ToArray())
                    // DenseVector.ofArray (lowerBound.ToArray()),
                    // DenseVector.ofArray (upperBound.ToArray())
                    )
                for i in 0..best.MinimizingPoint.Count-1 do
                    let (index1, index2) = mapping.[i]
                    solver.points().[index1].arr.[index2] <- best.MinimizingPoint[i]
#endif
                let c = solver.points()
                for k in 0..c.Length-2 do
                    let p1 = c.[k]
                    let p2 = c.[k+1]
                    path.curveto(p1.rpt().x, p1.rpt().y, p2.lpt().x, p2.lpt().y, p2.x, p2.y)

                i <- j - 1

        if this.isClosed then
            path.closepath()

        path.tostring()


        //terryspitz: fix start and end
        // if not this.isClosed then
        //     let firstPt = this.ctrlPts.[0]
        //     //firstPt.lTh <- firstPt.rTh
        //     let lastPt = this.ctrlPts.[this.ctrlPts.Length-1]
        //     assert (isnan lastPt.rTh)
        //     //lastPt.rTh <- lastPt.lTh



    // member this.renderSvg show_tangents =
    //     let path = BezPath()
    //     if this.ctrlPts.Length = 0 then "" else
    //     let pt0 = this.ctrlPts.[0] in path.moveto(pt0.pt.x, pt0.pt.y)
    //     let length = this.ctrlPts.Length - if this.isClosed then 0 else 1
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



