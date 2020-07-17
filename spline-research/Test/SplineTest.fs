module SpiroTest

open NUnit.Framework

open Curves


[<TestFixture>]
type TestClass() = 

    [<Test>]
    member this.CheckCurves() =
        let ctrlPts = [|
            ControlPoint({x=80.;y=738.}, PointType.Smooth)
            ControlPoint({x=749.;y=540.}, PointType.Smooth)
            ControlPoint({x=671.;y=309.}, PointType.Smooth)
            ControlPoint({x=521.;y=396.}, PointType.Smooth)
            ControlPoint({x=377.;y=333.}, PointType.Smooth)
            ControlPoint({x=467.;y=231.}, PointType.Smooth)
        |]
        // let pt = new ControlPoint(new Vec2(knot.x, knot.y), knot.ty, knot.lth, knot.rth);
        let spline = Spline(ctrlPts, false)
        spline.solve()
        // Should this be bundled into solve?
        spline.computeCurvatureBlending()
        let bezpath = spline.render()
        printfn "%A" bezpath
