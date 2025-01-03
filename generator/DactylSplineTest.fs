module DactylSplineTest

open NUnit.Framework
open Curves
open DactylSpline

let PI = System.Math.PI
let max_iter = 5
[<TestFixture>]
type TestClass() = 

    let solve_and_print_spline (spline : Spline) =
        let svg = (spline.solve max_iter).Replace("\r\n", " ").Trim()
        // printfn "%A" (spline.to_string())
        printfn "%A" svg
        svg

    [<Test>]
    member this.CheckLines() =
        let expectedLineTo = "M 0,0 L 1,0"
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.AreEqual(expectedLineTo, svg)
        let spline = Spline([|
            {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.AreEqual(expectedLineTo, svg)
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Smooth; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.AreEqual("M 0,0 C 0,0 0,0 1,0", svg)
        let spline = Spline([|
            {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Smooth; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.AreEqual("M 0,0 C 0,0 0,0 1,0", svg)

    [<Test>]
    member this.CheckTwoPointCurvesWithAlignedTangents() =
        let expectedLineWithCurveTo = "M 0,0 C 0,0 0,0 1,0"
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.AreEqual(expectedLineWithCurveTo, svg)
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.AreEqual(expectedLineWithCurveTo, svg)
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.AreEqual(expectedLineWithCurveTo, svg)

    [<Test>]
    member this.CheckTwoPointCurvesWithOtherTangents() =
        let spline = Spline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI/2.)};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (PI/2.)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.AreEqual("M 0,0 C 0,1 1,1 1,0", svg)
        // let spline = Spline([|
        //     {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
        //     {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        // |], false)
        // let svg = solve_and_print_spline spline
        // Assert.AreEqual("M 0,0 L 1,0", svg)
        // let spline = Spline([|
        //     {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
        //     {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        // |], false)
        // let svg = solve_and_print_spline spline
        // Assert.AreEqual("M 0,0 L 1,0", svg)
