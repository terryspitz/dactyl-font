// Copyright 2018 Raph Levien

// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.


/// A library of primitives for curves and splines.
module Curves


open BezPath

let PI = System.Math.PI
let isnan = System.Double.IsNaN

/// <summary>
/// A simple container for 2-vectors
/// </summary>
type Vec2 = {
    x : float
    y : float
} with
    static member (-) (lhs, rhs) =
        {x=lhs.x-rhs.x; y=lhs.y-rhs.y}
    member this.norm() =
        sqrt (this.x * this.x + this.y * this.y)
    member this.dot(other) =
        this.x * other.x + this.y * other.y
    member this.cross(other) =
        this.x * other.y - this.y * other.x
    member this.atan2() =
        atan2 this.y this.x


type SplinePointType =
    | Corner = 0
    | Smooth = 1
    //terryspitz: add continuous tangents
    | LineToCurve = 2
    | CurveToLine = 3


/// ControlPoint is a lot like `Knot` but has no UI, is used for spline solving.
type SplineControlPoint(pt, ty, lth : float option, rth : float option) = 
    new(pt, ty) = SplineControlPoint(pt, ty, None, None)

    // input data
    member val pt : Vec2 = pt with get
    member val ty : SplinePointType = ty with get, set
    member val lth : float option = lth with get, set
    member val rth : float option = rth with get, set

    // fitted data
    member val kBlend : float option = lth with get, set
    //not the best naming convention, using one capital letter difference
    member val lTh : float = nan with get, set
    member val rTh : float = nan with get, set
    member val lAk : float = nan with get, set
    member val rAk : float = nan with get, set


/// Constructor argument is array of coordinate values [x0, y0, x1, y1, x2, y2, x3, y3].
type CubicBez (c : float []) =
    member this.c = c

    member this.weightsum(c0, c1, c2, c3) =
        let x = c0 * this.c.[0] + c1 * this.c.[2] + c2 * this.c.[4] + c3 * this.c.[6]
        let y = c0 * this.c.[1] + c1 * this.c.[3] + c2 * this.c.[5] + c3 * this.c.[7]
        {x=x; y=y}

    member this.eval t =
        let mt = 1. - t
        let c0 = mt * mt * mt
        let c1 = 3. * mt * mt * t
        let c2 = 3. * mt * t * t
        let c3 = t * t * t
        this.weightsum(c0, c1, c2, c3)

    member this.deriv(t) =
        let mt = 1. - t
        let c0 = -3. * mt * mt
        let c3 = 3. * t * t
        let c1 = -6. * t * mt - c0
        let c2 = 6. * t * mt - c3
        this.weightsum(c0, c1, c2, c3)

    member this.deriv2(t) =
        let mt = 1. - t
        let c0 = 6. * mt
        let c3 = 6. * t
        let c1 = 6. - 18. * mt
        let c2 = 6. - 18. * t
        this.weightsum(c0, c1, c2, c3)

    member this.curvature t =
        let d = this.deriv t
        let d2 = this.deriv2 t
        (d.cross d2) / d.norm() ** 3.

    member this.atanCurvature(t) =
        let d = this.deriv(t)
        let d2 = this.deriv2(t)
        atan2 (d.cross(d2)) (d.norm() ** 3.)

    // de Casteljau's algorithm
    member this.leftHalf() =
        let c : float [] = Array.zeroCreate 8
        c.[0] <- this.c.[0]
        c.[1] <- this.c.[1]
        c.[2] <- 0.5 * (this.c.[0] + this.c.[2])
        c.[3] <- 0.5 * (this.c.[1] + this.c.[3])
        c.[4] <- 0.25 * (this.c.[0] + 2. * this.c.[2] + this.c.[4])
        c.[5] <- 0.25 * (this.c.[1] + 2. * this.c.[3] + this.c.[5])
        c.[6] <- 0.125 * (this.c.[0] + 3. * (this.c.[2] + this.c.[4]) + this.c.[6])
        c.[7] <- 0.125 * (this.c.[1] + 3. * (this.c.[3] + this.c.[5]) + this.c.[7])
        CubicBez c

    member this.rightHalf() =
        let c : float [] = Array.zeroCreate 8
        c.[0] <- 0.125 * (this.c.[0] + 3. * (this.c.[2] + this.c.[4]) + this.c.[6])
        c.[1] <- 0.125 * (this.c.[1] + 3. * (this.c.[3] + this.c.[5]) + this.c.[7])
        c.[2] <- 0.25 * (this.c.[2] + 2. * this.c.[4] + this.c.[6])
        c.[3] <- 0.25 * (this.c.[3] + 2. * this.c.[5] + this.c.[7])
        c.[4] <- 0.5 * (this.c.[4] + this.c.[6])
        c.[5] <- 0.5 * (this.c.[5] + this.c.[7])
        c.[6] <- this.c.[6]
        c.[7] <- this.c.[7]
        CubicBez c


type Polynomial (c : float []) =
    member this.c = c

    member this.eval x =
        let mutable xi = 1.
        let mutable s = 0.
        for a in this.c do
            s <- s + a * xi
            xi <- xi * x
        s

    member this.deriv() =
        let d : float [] = Array.zeroCreate (this.c.Length - 1)
        for i in 0..this.c.Length-2 do
            d.[i] <- float (i + 1) * this.c.[i + 1]
        Polynomial d


let hermite5(x0, x1, v0, v1, a0, a1) =
    Polynomial([|
                x0
                v0
                0.5 * a0
                -10. * x0 + 10. * x1 - 6. * v0 - 4. * v1 - 1.5 * a0 + 0.5 * a1
                15. * x0 - 15. * x1 + 8. * v0 + 7. * v1 + 1.5 * a0 - a1
                -6. * x0 + 6. * x1 - 3. * v0 - 3. * v1 + -0.5 * a0 + 0.5 * a1|])

/// Solve tridiagonal matrix system. Destroys inputs, leaves output in x.
///
/// Solves a[i] * x[i - 1] + b[i] * x[i] + c[i] * x[i + 1] = d[i]
///
/// Inputs are array-like objects (typed arrays are good for performance).
///
/// Note: this is not necessarily the fastest, see:
/// https://en.wikibooks.org/wiki/Algorithm_Implementation/Linear_Algebra/Tridiagonal_matrix_algorithm
// let tridiag(a, b, c, d, x) =
//     let n = x.Length
//     for i in 1..n-1 do
//         let m = a.[i] / b.[i - 1]
//         b.[i] <- b.[i] - m * c.[i - 1]
//         d.[i] <- d.[i] - m * d.[i - 1]
//     x.[n - 1] <- d.[n - 1] / b.[n - 1]
//     for i in n - 2..-1..0 do
//         x.[i] <- (d.[i] - c.[i] * x.[i + 1]) / b.[i]
   

/// Create a smooth cubic bezier.
let myCubic th0 th1 =
    let myCubicLen th0 th1 =
        let offset = 0.3 * sin (th1 * 2. - 0.4 * sin (th1 * 2.))
        let newShape = true
        if newShape then
            let scale = 1.0 / (3. * 0.8)
            let len = scale * (cos (th0 - offset) - 0.2 * cos ((3. * (th0 - offset))))
            len
        else
            let drive = 2.0
            let scale = 1.0 / (3. * tanh drive)
            let len = scale * tanh (drive * cos (th0 - offset))
            len

    let coords : float [] = Array.zeroCreate 8
    let len0 = myCubicLen th0 th1
    coords.[2] <- cos th0 * len0
    coords.[3] <- sin th0 * len0
    let len1 = myCubicLen th1 th0
    coords.[4] <- 1. - cos th1 * len1
    coords.[5] <- sin th1 * len1
    coords.[6] <- 1.
    coords


type Ak = {
    ak0 : float
    ak1 : float
}

/// Base class for two parameter curve families
// type TwoParamCurve() =
    /// Render the curve, providing an array of _interior_ cubic bezier
    /// control points only. Return value is an array of 3n-1 Vec2's.
    // render(th0, th1)

    /// Compute curvature.
    ///
    /// Result is an object with ak0 and ak1 (arctan of curvature at endpoints).
    /// Quadrant is significant - a value outside -pi/2 to pi/2 means a reversal
    /// of direction.
    // computeCurvature(th0, th1)

    /// Get endpoint condition.
    ///
    /// Return tangent at endpoint given next-to-endpoint tangent.
    // endpointTangent(th)

    /// Compute curvature derivatives.
    ///
    /// Result is an object with dak0dth0 and friends.
    /// Default implementation is approximate through central differencing, but
    /// curves can override.
    // member this.computeCurvatureDerivs th0 th1 =
    //     let epsilon = 1e-6
    //     let scale = 2.0 / epsilon
    //     let k0plus = this.computeCurvature(th0 + epsilon, th1)
    //     let k0minus = this.computeCurvature(th0 - epsilon, th1)
    //     let dak0dth0 = scale * (k0plus.ak0 - k0minus.ak0)
    //     let dak1dth0 = scale * (k0plus.ak1 - k0minus.ak1)
    //     let k1plus = this.computeCurvature(th0, th1 + epsilon)
    //     let k1minus = this.computeCurvature(th0, th1 - epsilon)
    //     let dak0dth1 = scale * (k1plus.ak0 - k1minus.ak0)
    //     let dak1dth1 = scale * (k1plus.ak1 - k1minus.ak1)
    //     {dak0dth0: dak0dth0, dak1dth0: dak1dth0, dak0dth1: dak0dth1, dak1dth1: dak1dth1}


type MyCurve() = //extends TwoParamCurve =
    member this.render(th0, th1) =
        let c = myCubic th0 th1
        [| {x=c.[2]; y=c.[3]}; {x=c.[4]; y=c.[5]} |]

    /// Render as a 4-parameter curve with optional adjusted endpoint curvatures.
    member this.render4Quintic(th0, th1, k0, k1) =
        //let cb = new CubicBez(myCubic(th0, th1))
        let cb : CubicBez = this.convCubic(this.render4Cubic(th0, th1, k0, k1))
        // compute second deriv tweak to match curvature
        let curvAdjust(t, th, k : float option) =
            match k with
            | Some k ->
                let c = cos th
                let s = sin th
                let d2 = cb.deriv2(t)
                let d2cross = d2.y * c - d2.x * s
                let d = cb.deriv(t)
                let ddot = d.x * c + d.y * s
                // TODO: if ddot = 0, cusp, no adjustment
                let oldK = d2cross / (ddot * ddot)
                let kAdjust = k - oldK
                let aAdjust = kAdjust * (ddot * ddot)
                {x = -s * aAdjust; y = c * aAdjust}
            | None -> {x=0.; y=0.}
        let a0 = curvAdjust(0., th0, k0)
        let a1 = curvAdjust(1., -th1, k1)
        let hx = hermite5(0., 0., 0., 0., a0.x, a1.x)
        let hy = hermite5(0., 0., 0., 0., a0.y, a1.y)
        let hxd = hx.deriv()
        let hyd = hy.deriv()
        // This really would be cleaner if we had arbitrary deCasteljau...
        let c0 = cb.leftHalf()
        let c1 = cb.rightHalf()
        let cs = [|c0.leftHalf(); c0.rightHalf(); c1.leftHalf(); c1.rightHalf()|]
        let scale = 1./12.
        [| for i in 0..3 do
            let t = 0.25 * float i
            let t1 = t + 0.25
            let c = cs.[i].c
            let x0 = hx.eval(t)
            let y0 = hy.eval(t)
            let x1 = x0 + scale * hxd.eval(t)
            let y1 = y0 + scale * hyd.eval(t)
            let x3 = hx.eval(t1)
            let y3 = hy.eval(t1)
            let x2 = x3 - scale * hxd.eval(t1)
            let y2 = y3 - scale * hyd.eval(t1)
            if i <> 0 then
                yield {x=c.[0] + x0; y=c.[1] + y0}
            yield {x=c.[2] + x1; y=c.[3] + y1}
            yield {x=c.[4] + x2; y=c.[5] + y2}
        |]

    member this.convCubic(pts : Vec2 []) =
        let coords : float [] = Array.zeroCreate 8
        coords.[2] <- pts.[0].x
        coords.[3] <- pts.[0].y
        coords.[4] <- pts.[1].x
        coords.[5] <- pts.[1].y
        coords.[6] <- 1.
        CubicBez coords

    // Ultimately we want to exactly match the endpoint curvatures (probably breaking
    // into two cubic segments), but for now, just approximate...
    member this.render4Cubic(th0, th1, k0, k1) =
        let cb = CubicBez (myCubic th0 th1)

        let deriv_scale(t, th, k : float Option) =
            match k with
            | Some k ->
                let c = cos th
                let s = sin th
                let d = cb.deriv(t)
                let d2 = cb.deriv2(t)
                let d2cross = d2.y * c - d2.x * s
                let ddot = d.x * c + d.y * s
                let mutable oldK = d2cross / (ddot * ddot)
                // fudge to avoid divide-by-zero
                if abs oldK  < 1e-6 then oldK <- 1e-6
                let ratio = k / oldK
                // TODO: fine tune this dodgy formula
                //let scale = ratio < 1 ? 1/2 - ratio/6 : 1/(3*ratio)
                let scale = 1./(2. + ratio)
                scale
            | None -> 1./3.
        let scale0 = deriv_scale(0., th0, k0)
        let d0 = cb.deriv(0.)
        let d1 = cb.deriv(1.)
        let scale1 = deriv_scale(1., -th1, k1)
        [|  {x= d0.x * scale0; y=d0.y * scale0}
            {x= 1. - d1.x * scale1; y= -d1.y * scale1}|]

    member this.render4(th0, th1, k0, k1) =
        match (k0, k1) with
        | None, None ->
            this.render(th0, th1)
        | _ ->
            this.render4Quintic(th0, th1, k0, k1)

    member this.computeCurvature(th0, th1) =
        let cb = CubicBez (myCubic th0 th1)
        let curv(t, th) =
            let c = cos (th)
            let s = sin (th)
            let d2 = cb.deriv2(t)
            let d2cross = d2.y * c - d2.x * s
            let d = cb.deriv(t)
            let ddot = d.x * c + d.y * s
            atan2 d2cross (ddot * abs ddot)

        //let ak0 = cb.atanCurvature(0)
        //let ak1 = cb.atanCurvature(1)
        { ak0 = curv(0., th0); ak1 = curv(1., -th1) }

    member this.endpointTangent(th) =
        // Same value as parabola:
        //return atan(2 * tan(th)) - th
        0.5 * sin (2. * th)
        //terryspitz: https://www.google.com/search?q=atan%282+*+tan%28x%29%29+-+x%2C+0.5+*+sin+%282.+*+x%29

//! Global spline solver

// normalize theta to -pi..pi
let mod2pi(th) =
    let twopi = 2. * PI
    let frac = th / twopi
    twopi * (frac - round frac)


type ths = {
    th0 : float
    th1 : float
    chord : float
}

let hypot(x, y) =
    sqrt (x * x + y * y)

type TwoParamSpline(curve : MyCurve, ctrlPts : Vec2 array, isClosed : bool) =
    let _ths : float array = Array.zeroCreate ctrlPts.Length
    member this.curve : MyCurve = curve
    member this.ctrlPts : Vec2 array = ctrlPts
    member this.isClosed = isClosed
    // member this.startTh : float option = None
    // member this.endTh : float option = None
    member val startTh : float option = None with get, set
    member val endTh : float option = None with get, set
    member this.ths = _ths

    /// Determine initial tangent angles, given array of Vec2 control points.
    member this.initialThs() =
        for i in 1.._ths.Length - 2 do
            let dx0 = this.ctrlPts.[i].x - this.ctrlPts.[i - 1].x
            let dy0 = this.ctrlPts.[i].y - this.ctrlPts.[i - 1].y
            let l0 = hypot(dx0, dy0)
            let th0 = atan2 dy0 dx0
            let dx1 = this.ctrlPts.[i + 1].x - this.ctrlPts.[i].x
            let dy1 = this.ctrlPts.[i + 1].y - this.ctrlPts.[i].y
            let l1 = hypot(dx1, dy1)
            let th1 = atan2 dy1 dx1
            let bend = mod2pi (th1 - th0)
            let th = mod2pi (th0 + bend * l0 / (l0 + l1))
            _ths.[i] <- th
            if i = 1 then _ths.[0] <- th0
            if i = _ths.Length - 2 then _ths.[i + 1] <- th1

        match this.startTh with
        | Some th -> 
            _ths.[0] <- th
        | None -> ()
        match this.endTh with
        | Some th ->
            _ths.[_ths.Length - 1] <- th
        | None -> ()

    /// Get tangent angles relative to endpoints, and chord Length.
    member this.getThs(i) =
        let dx = this.ctrlPts.[i + 1].x - this.ctrlPts.[i].x
        let dy = this.ctrlPts.[i + 1].y - this.ctrlPts.[i].y
        let th =  atan2 dy dx
        let th0 = mod2pi(_ths.[i] - th)
        let th1 = mod2pi(th - _ths.[i + 1])
        let chord = hypot(dx, dy)
        {th0 = th0; th1 = th1; chord = chord}

    /// Crawl towards a curvature continuous solution.
    member this.iterDumb(iter) =
        let computeErr(ths0, ak0, ths1, ak1) =
            // rescale tangents by geometric mean of chordLengths
            let ch0 = sqrt(ths0.chord)
            let ch1 = sqrt(ths1.chord)
            let a0 = atan2 (sin (ak0.ak1) * ch1) (cos (ak0.ak1) * ch0)
            let a1 = atan2 (sin (ak1.ak0) * ch0) (cos (ak1.ak0) * ch1)
            a0 - a1
            // return ths1.chord * sin (ak0.ak1) * cos (ak1.ak0)
            //     - ths0.chord * sin (ak1.ak0) * cos (ak0.ak1)

        let n = this.ctrlPts.Length
        // Fix endpoint tangents; we rely on iteration for this to converge
        if this.startTh.IsNone then
            let startThs = this.getThs(0)
            _ths.[0] <- _ths.[0] + (this.curve.endpointTangent(startThs.th1) - startThs.th0)
        if this.endTh.IsNone then
            let endThs = this.getThs(n - 2)
            _ths.[n - 1] <- _ths.[n - 1] - (this.curve.endpointTangent(endThs.th0) - endThs.th1)
        
        //terryspitz: correction to match start/end tangents on closed curves
        if this.isClosed && this.startTh.IsNone && this.endTh.IsNone then
            let avgth = (_ths.[0] + _ths.[n - 1]) / 2.
            _ths.[0] <- avgth
            _ths.[n - 1] <- avgth

        if n < 3 then 0. else

        let mutable absErr = 0.
        let x : float [] = Array.zeroCreate (n - 2)
        let mutable ths0 = this.getThs(0)
        let mutable ak0 = this.curve.computeCurvature(ths0.th0, ths0.th1)
        for i in 0..n - 3 do
            let ths1 = this.getThs(i + 1)
            let ak1 = this.curve.computeCurvature(ths1.th0, ths1.th1)
            let err = computeErr(ths0, ak0, ths1, ak1)
            absErr <- absErr + abs err

            let epsilon = 1e-3
            let ak0p = this.curve.computeCurvature(ths0.th0, ths0.th1 + epsilon)
            let ak1p = this.curve.computeCurvature(ths1.th0 - epsilon, ths1.th1)
            let errp = computeErr(ths0, ak0p, ths1, ak1p)
            let derr = (errp - err) / epsilon
            x.[i] <- err / derr

            ths0 <- ths1
            ak0 <- ak1

        let scale = tanh (0.25 * (float iter + 1.))
        for i in 0..n - 3 do
            _ths.[i + 1] <- _ths.[i + 1] + scale * x.[i]

        // printfn "abs err %f at iter %d" absErr iter
        absErr

    /// Perform one step of a Newton solver.
    // Not yet implemented
    // iterate() {
    //     let n = this.ctrlPts.Length
    //     if (n < 3) return
    //     var a = Array.zeroCreate (n - 2)
    //     var b = Array.zeroCreate (n - 2)
    //     var c = Array.zeroCreate (n - 2)
    //     var d = Array.zeroCreate (n - 2)
    //     var x = Array.zeroCreate (n - 2)

    //     let ths0 = this.getThs(0)
    //     var last_ak = this.curve.computeCurvature ths0.th0 ths0.th1
    //     var last_dak = this.curve.computeCurvatureDerivs ths0.th0 ths0.th1
    //     var last_a = hypot(this.ctrlPts[1].x - this.ctrlPts[0].x,
    //                        this.ctrlPts[1].y - this.ctrlPts[0].y)
    //     for i in 0..n-3) {
    //         let ths = this.getThs(i + 1)
    //         let ak = this.curve.computeCurvature ths.th0 ths.th1
    //         let dak = this.curve.computeCurvatureDerivs ths.th0 ths.th1
    //         let a = hypot(this.ctrlPts.[i + 2].x - this.ctrlPts.[i + 1].x,
    //                          this.ctrlPts.[i + 2].y - this.ctrlPts.[i + 1].y)
    //         let c0 = cos (last_ak.ak1)
    //         let s0 = sin (last_ak.ak1)
    //         let c1 = cos (ak.ak0)
    //         let s1 = sin (ak.ak0)

    //         // TODO: fill in derivatives properly
    //         d.[i] = a * s0 * c1 - last_a * s1 * c0

    //         last_ak = ak
    //         last_dak = dak
    //         last_a = a
    //     }

    //     tridiag(a, b, c, d, x)
    //     for (var i = 0; i < n - 2; i++) {
    //         _ths.[i + 1] -= x.[i]
    //     }
    // }

    /// Return an SVG path string.
    // member this.renderSvg() =
    //     let c = this.ctrlPts
    //     if c.Length = 0 then [""] else
    //     [
    //         yield sprintf "M %s,%s" (Format c.[0].x) (Format c.[0].y) |> ignore
    //         let mutable cmd = " C"
    //         for i in 0..c.Length - 2 do
    //             let ths = this.getThs(i)
    //             let render = this.curve.render(ths.th0, ths.th1)
    //             let dx = c.[i + 1].x - c.[i].x
    //             let dy = c.[i + 1].y - c.[i].y
    //             for j in 0..render.Length-1 do
    //                 let pt = render[j]
    //                 let x = c.[i].x + dx * pt.x - dy * pt.y
    //                 let y = c.[i].y + dy * pt.x + dx * pt.y
    //                 yield sprintf "%s %s,%s" cmd (Format x) (Format y) |> ignore
    //                 cmd <- " "
    //             yield sprintf " %s,%s" (Format c.[i + 1].x) (Format c.[i + 1].y) |> ignore
    //     ]

/// Spline handles more general cases, including corners.
type Spline (ctrlPts, isClosed) =
    member this.ctrlPts : SplineControlPoint array = ctrlPts
    member this.isClosed = isClosed
    member this.curve = MyCurve()

    member this.pt(i, start) =
        let length = this.ctrlPts.Length
        this.ctrlPts.[(i + start + length) % length]

    member this.startIx() =
        if not this.isClosed then
            0
        else 
            match Array.tryFindIndex 
                (fun (pt : SplineControlPoint) -> pt.ty = SplinePointType.Corner || pt.lth.IsSome)
                this.ctrlPts with
            | Some i -> i
            | None -> 0 // Path is all-smooth and closed.

    member this.solve(maxIter) =
        let start = this.startIx()
        let length = this.ctrlPts.Length - if this.isClosed then 0 else 1

        //terryspitz: implement LineToCurve and CurveToLine as Corners with fixed tangent theta
        //determined from the Line
        for i in 0..length do
            let ptI = this.pt(i, start)
            if ptI.ty = SplinePointType.LineToCurve || ptI.ty = SplinePointType.CurveToLine then
                assert (ptI.lth.IsNone && ptI.rth.IsNone)
                let ptI1 = if ptI.ty = SplinePointType.LineToCurve then 
                                this.pt(i - 1, start)
                            else
                                this.pt(i + 1, start)
                assert (ptI1.ty <> SplinePointType.Smooth)
                let dx = ptI.pt.x - ptI1.pt.x
                let dy = ptI.pt.y - ptI1.pt.y
                let th = atan2 dy dx
                ptI.ty <- SplinePointType.Corner
                ptI.lth <- Some th
                ptI.rth <- Some th

        let mutable i = 0
        while i < length do
            let ptI = this.pt(i, start)
            let ptI1 = this.pt(i + 1, start)
            if (i + 1 = length || ptI1.ty = SplinePointType.Corner)
                && ptI.rth.IsNone && ptI1.lth.IsNone then
                let dx = ptI1.pt.x - ptI.pt.x
                let dy = ptI1.pt.y - ptI.pt.y
                let th = atan2 dy dx
                ptI.rTh <- th
                ptI1.lTh <- th
                i <- i+1
            else
                // We have a curve.
                let mutable j = i + 1
                let mutable break_ = false
                let innerPts =
                    ptI.pt ::
                    [
                        while j < length + 1 && not break_ do
                            let ptJ = this.pt(j, start)
                            yield (ptJ.pt)
                            j <- j + 1
                            if ptJ.ty = SplinePointType.Corner || ptJ.lth.IsSome then
                                break_ <- true
                    ]
                //console.log(innerPts)
                let inner = TwoParamSpline(this.curve, Array.ofList innerPts, 
                                this.isClosed && innerPts.Length-1 = length)
                inner.startTh <- this.pt(i, start).rth
                inner.endTh <- this.pt(j - 1, start).lth
                inner.initialThs()
                let CONVERGED_ERR = 1e-6
                let mutable iter = 0
                while inner.iterDumb iter > CONVERGED_ERR && iter < maxIter do
                    iter <- iter + 1
                for k in i..j-2 do
                    let ptK = this.pt(k, start)
                    let ptK1 = this.pt(k + 1, start)
                    //terryspitz: spline solve has undefined direction (same solution for tangents at 180) 
                    //so align thetas with chord direction to next point
                    let dx = ptK1.pt.x - ptK.pt.x
                    let dy = ptK1.pt.y - ptK.pt.y
                    let chordTh = atan2 dy dx
                    let alignTh th = 
                        if abs (mod2pi (th - chordTh)) < PI/2. then
                            th
                        else
                            mod2pi (th + PI)
                    ptK.rTh <- alignTh inner.ths.[k - i]
                    ptK1.lTh <- alignTh inner.ths.[k + 1 - i]
                    // Record curvatures (for blending, not all will be used)
                    let ths = inner.getThs(k - i)
                    let aks = this.curve.computeCurvature(ths.th0, ths.th1)
                    this.pt(k, start).rAk <- aks.ak0
                    this.pt(k + 1, start).lAk <- aks.ak1

                i <- j - 1

        //terryspitz: fix start and end
        if not this.isClosed then
            let firstPt = this.ctrlPts.[0]
            assert (isnan firstPt.lTh)
            //firstPt.lTh <- firstPt.rTh
            let lastPt = this.ctrlPts.[this.ctrlPts.Length-1]
            assert (isnan lastPt.rTh)
            //lastPt.rTh <- lastPt.lTh
        this.computeCurvatureBlending()


    member this.chordLen(i) =
        let ptI = this.pt(i, 0).pt
        let ptI1 = this.pt(i + 1, 0).pt
        hypot(ptI1.x - ptI.x, ptI1.y - ptI.y)

    // Determine whether a control point requires curvature blending, and if so,
    // the blended curvature. To be invoked after solving.
    member this.computeCurvatureBlending() =
        let myTan (th) =
            if (th > PI / 2.) then
                tan (PI - th)
            elif (th < -PI / 2.) then
                tan (-PI - th)
            else
                tan (th)
        for pt in this.ctrlPts do
            pt.kBlend <- None
        let length = this.ctrlPts.Length - if this.isClosed then 0 else 1
        for i in 0..length-1 do
            let pt = this.pt(i, 0)
            if pt.ty = SplinePointType.Smooth && pt.lth.IsSome then
                // let thresh = PI / 2. - 1e-6
                //if (abs(pt.rAk) > thresh || abs(pt.lAk) > thresh) {
                //    // Don't blend reversals. We might reconsider this, but punt for now.
                //    continue
                //}
                pt.kBlend <-
                    if isnan (pt.rAk) || isnan (pt.lAk) || sign (pt.rAk) <> sign (pt.lAk) then
                        Some 0.
                    else
                        let rK = myTan(pt.rAk) / this.chordLen(i - 1)
                        let lK = myTan(pt.lAk) / this.chordLen(i)
                        Some (2. / (1. / rK + 1. / lK))

    member this.renderSvg show_tangents =
        let path = BezPath()
        if this.ctrlPts.Length = 0 then "" else
        let pt0 = this.ctrlPts.[0] in path.moveto(pt0.pt.x, pt0.pt.y)
        let length = this.ctrlPts.Length - if this.isClosed then 0 else 1
        for i in 0..length-1 do
            path.mark i
            let ptI = this.pt(i, 0)
            let ptI1 = this.pt(i + 1, 0)
            let dx = ptI1.pt.x - ptI.pt.x
            let dy = ptI1.pt.y - ptI.pt.y
            let chth = atan2 dy dx
            let chord = hypot(dy, dx)
            let th0 = mod2pi(ptI.rTh - chth)
            let th1 = mod2pi(chth - ptI1.lTh)
            // Apply curvature blending
            let k0 = Option.map (fun k->k*chord) ptI.kBlend
            let k1 = Option.map (fun k->k*chord) ptI1.kBlend
            let render = this.curve.render4(th0, th1, k0, k1)
            let c =
                [|
                    for j in 0..render.Length-1 do
                        let pt = render.[j]
                        yield ptI.pt.x + dx * pt.x - dy * pt.y
                        yield ptI.pt.y + dy * pt.x + dx * pt.y
                    yield ptI1.pt.x
                    yield ptI1.pt.y
                |]
            for j in 0..6..c.Length-1 do
                path.curveto(c.[j], c.[j + 1], c.[j + 2], c.[j + 3], c.[j + 4], c.[j + 5])
        if this.isClosed then
            path.closepath()

        //terryspitz: also render tangents in SVG
        if show_tangents then
            for i in 1..length-1 do
                path.mark i
                let ptI = this.pt(i, 0)
                let offset = 100.
                // path.moveto(ptI.pt.x, ptI.pt.y)
                // path.lineto(ptI.pt.x + offset*cos(-ptI1.lTh), ptI.pt.y + offset*sin(-ptI1.lTh))
                path.moveto(ptI.pt.x, ptI.pt.y)
                path.lineto(ptI.pt.x + offset*cos(ptI.lTh), ptI.pt.y + offset*sin(ptI.lTh))
        path.tostring()
