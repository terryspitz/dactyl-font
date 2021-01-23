module DactylSplineTest

open NUnit.Framework
open Curves
open DactylSpline

let PI = System.Math.PI

[<TestFixture>]
type TestClass() = 

    [<Test>]
    member this.CheckLine() =
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        printfn "%A" (spline.solve(5))

    [<Test>]
    member this.CheckTwoPointCurves() =
        let spline = Spline([|
            {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        printfn "%A" (spline.solve(5))
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Smooth; x=Some 1.; y=Some 0.; th=None};
        |], false)
        printfn "%A" (spline.solve(5))
        let spline = Spline([|
            {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Smooth; x=Some 1.; y=Some 0.; th=None};
        |], false)
        printfn "%A" (spline.solve(5))

    [<Test>]
    member this.CheckTwoPointCurvesWithTangents() =
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        printfn "%A" (spline.solve(5))
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        |], false)
        printfn "%A" (spline.solve(5))
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        |], false)
        printfn "%A" (spline.solve(5))
