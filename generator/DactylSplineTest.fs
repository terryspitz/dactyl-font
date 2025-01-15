module DactylSplineTest

open NUnit.Framework
open Curves
open DactylSpline

let PI = System.Math.PI
let max_iter = 5
[<TestFixture>]
type TestClass() = 

    let solve_and_print_spline (spline : DSpline) =
        let svg = spline.solve(max_iter, false)
        let svg = svg.Replace("\r\n", " ").Trim()
        let svg = svg.Replace("\n", " ").Trim()
        // printfn "%A" (spline.to_string())
        let output = "OUTPUT: " + svg
        printfn "%A" output
        output

    [<Test>]
    member this.CheckLines() =
        let expectedLineTo = "M 0,0 L 1.000,0"
        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo(expectedLineTo))

        let spline = DSpline([|
            {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Is.EqualTo(expectedLineTo))

        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Smooth; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo("M 0,0 C 0.333,0 0.667,0 1.000,0"))

        let spline = DSpline([|
            {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Smooth; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo("M 0,0 C 0.333,0 0.667,0 1.000,0"))

    [<Test>]
    member this.CheckTwoPointCurvesWithAlignedTangents() =
        let expectedLineWithCurveTo = "M 0,0 C 0.333,0 0.667,0 1.000,0"
        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo(expectedLineWithCurveTo))

        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo(expectedLineWithCurveTo))

        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo(expectedLineWithCurveTo))

    [<Test>]
    member this.CheckTwoPointCurvesWithOtherTangents() =
        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI/2.)};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 1.; th=Some (-PI)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo("M 0,0 C 0,0.520 0.482,1.000 1.000,1.000"))

        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI/2.)};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (PI/2.)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo("M 0,0 C 0,0.646 1.000,0.799 1.000,0"))

        // let spline = Spline([|
        //     {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
        //     {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        // |], false)
        // let svg = solve_and_print_spline spline
        // Assert.That(svg, Is.EqualTo("M 0,0 L 1,0"))

        // let spline = Spline([|
        //     {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
        //     {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        // |], false)
        // let svg = solve_and_print_spline spline
        // Assert.That(svg, Is.EqualTo("M 0,0 L 1.0,0"))
