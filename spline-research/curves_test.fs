module CurvesTest

open NUnit.Framework

open Curves


[<TestFixture>]
type TestClass() = 

    [<Test>]
    member this.CheckCurves() =
        let ctrlPts = [|
            SplineControlPoint({x=80.;y=738.},  SplinePointType.Smooth)
            SplineControlPoint({x=749.;y=540.}, SplinePointType.Smooth)
            SplineControlPoint({x=671.;y=309.}, SplinePointType.Smooth)
            SplineControlPoint({x=521.;y=396.}, SplinePointType.Smooth)
            SplineControlPoint({x=377.;y=333.}, SplinePointType.Smooth)
            SplineControlPoint({x=467.;y=231.}, SplinePointType.Smooth)
        |]
        // let pt = new ControlPoint(new Vec2(knot.x, knot.y), knot.ty, knot.lth, knot.rth);
        let spline = Spline(ctrlPts, false)
        spline.solve(10)
        let bezpath = spline.renderSvg(false)
        printfn "%A" bezpath
