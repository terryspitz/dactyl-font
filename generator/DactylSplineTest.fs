module DactylSplineTest

open System
open NUnit.Framework
open Curves
open DactylSpline

let PI = System.Math.PI
let max_iter = 5
[<TestFixture>]
type TestClass() = 

    let solve_and_print_spline (spline : DSpline) =
        let svg = spline.solve(max_iter, false)
        let svg = (String.Join(" ", svg))
        printfn "%A" svg
        svg

    [<Test>]
    member this.CheckLines() =
        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg, Is.EqualTo("M 0,0 L 1,0"))

        let spline = DSpline([|
            {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Does.StartWith("M 0,0 "))
        Assert.That(svg,Does.EndWith("1,0"))

        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Smooth; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Does.StartWith("M 0,0 "))
        Assert.That(svg,Does.EndWith("1,0"))

        let spline = DSpline([|
            {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Smooth; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Does.StartWith("M 0,0 "))
        Assert.That(svg,Does.EndWith("1,0"))

    [<Test>]
    member this.CheckTwoPointCurvesWithAlignedTangents() =
        let expectedLineWithCurveTo = "M 0,0 C 0.333,0 0.667,0 1.000,0"
        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Does.StartWith("M 0,0 "))
        Assert.That(svg,Does.EndWith("1,0"))

        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Does.StartWith("M 0,0 "))
        Assert.That(svg,Does.EndWith("1,0"))

        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some 0.};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (-PI)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Does.StartWith("M 0,0 "))
        Assert.That(svg,Does.EndWith("1,0"))

    [<Test>]
    member this.CheckTwoPointCurvesWithOtherTangents() =
        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI/2.)};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (PI)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Does.StartWith("M 0,0 "))
        Assert.That(svg,Does.EndWith("1,0"))

        let spline = DSpline([|
            {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI/2.)};
            {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (PI/2.)};
        |], false)
        let svg = solve_and_print_spline spline
        Assert.That(svg,Does.StartWith("M 0,0 "))
        Assert.That(svg,Does.EndWith("1,0"))

