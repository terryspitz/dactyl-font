module DactylSplineTest

open System
open NUnit.Framework
open Curves
open DactylSpline

let PI = System.Math.PI
let max_iter = 500

[<TestFixture>]
type TestClass() =

    let solve_and_print_spline (spline: DSpline) =
        let svg = fst (spline.solveAndRenderTuple (max_iter, 1.0, false, false))
        let svg = (String.Join(" ", svg))
        printfn "%A" svg
        svg.Trim()

    [<Test>]
    member this.CheckLinesCornerCorner() =
        let spline =
            DSpline(
                [| { ty = SplinePointType.Corner
                     x = Some 0.
                     y = Some 0.
                     th = None }
                   { ty = SplinePointType.Corner
                     x = Some 1.
                     y = Some 0.
                     th = None } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckLinesCornerSmooth() =
        let spline =
            DSpline(
                [| { ty = SplinePointType.Smooth
                     x = Some 0.
                     y = Some 0.
                     th = None }
                   { ty = SplinePointType.Corner
                     x = Some 1.
                     y = Some 0.
                     th = None } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckLinesSmoothCorner() =
        let spline =
            DSpline(
                [| { ty = SplinePointType.Corner
                     x = Some 0.
                     y = Some 0.
                     th = None }
                   { ty = SplinePointType.Smooth
                     x = Some 1.
                     y = Some 0.
                     th = None } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckLinesSmoothSmooth() =
        let spline =
            DSpline(
                [| { ty = SplinePointType.Smooth
                     x = Some 0.
                     y = Some 0.
                     th = None }
                   { ty = SplinePointType.Smooth
                     x = Some 1.
                     y = Some 0.
                     th = None } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckTwoPointCurvesWithAlignedTangents() =
        let expectedLineWithCurveTo = "M 0,0C 0.333,0 0.667,0 1.000,0"

        let spline =
            DSpline(
                [| { ty = SplinePointType.Corner
                     x = Some 0.
                     y = Some 0.
                     th = Some 0. }
                   { ty = SplinePointType.Corner
                     x = Some 1.
                     y = Some 0.
                     th = None } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

        let spline =
            DSpline(
                [| { ty = SplinePointType.Corner
                     x = Some 0.
                     y = Some 0.
                     th = None }
                   { ty = SplinePointType.Corner
                     x = Some 1.
                     y = Some 0.
                     th = Some(-PI) } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

        let spline =
            DSpline(
                [| { ty = SplinePointType.Corner
                     x = Some 0.
                     y = Some 0.
                     th = Some 0. }
                   { ty = SplinePointType.Corner
                     x = Some 1.
                     y = Some 0.
                     th = Some(-PI) } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckTwoPointCurvesWithOtherTangents() =
        let spline =
            DSpline(
                [| { ty = SplinePointType.Corner
                     x = Some 0.
                     y = Some 0.
                     th = Some(PI / 2.) }
                   { ty = SplinePointType.Corner
                     x = Some 1.
                     y = Some 0.
                     th = Some(PI) } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

        let spline =
            DSpline(
                [| { ty = SplinePointType.Corner
                     x = Some 0.
                     y = Some 0.
                     th = Some(PI / 2.) }
                   { ty = SplinePointType.Corner
                     x = Some 1.
                     y = Some 0.
                     th = Some(PI / 2.) } |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckFlatnessParam() =
        let spline =
            DSpline(
                [| { ty = SplinePointType.Corner
                     x = Some 0.
                     y = Some 0.
                     th = None }
                   { ty = SplinePointType.Smooth
                     x = Some 0.5
                     y = Some 1.0
                     th = None }
                   { ty = SplinePointType.Corner
                     x = Some 1.
                     y = Some 0.
                     th = None } |],
                false
            )

        let svgFlat0 =
            fst (spline.solveAndRenderTuple (5000, 0.0, false, false)) |> String.concat " "

        let svgFlat10 =
            fst (spline.solveAndRenderTuple (5000, 10.0, false, false)) |> String.concat " "

        printfn "Flatness 0.0: %s" svgFlat0
        printfn "Flatness 10.0: %s" svgFlat10

        Assert.That(svgFlat0, Is.Not.EqualTo(svgFlat10))

[<TestFixture>]
type LinearRegressionTests() =
    [<Test>]
    member this.PerfectLine() =
        // y = 2x + 1
        let xs = [| 0.0; 1.0; 2.0; 3.0 |]
        let ys = [| 1.0; 3.0; 5.0; 7.0 |]
        let m, c, res = linear_regression xs ys
        Assert.That(m, Is.EqualTo(2.0).Within(1e-10))
        Assert.That(c, Is.EqualTo(1.0).Within(1e-10))
        Assert.That(res, Is.EqualTo(0.0).Within(1e-10))

    [<Test>]
    member this.HorizontalLine() =
        // y = 5
        let xs = [| 0.0; 10.0; 20.0 |]
        let ys = [| 5.0; 5.0; 5.0 |]
        let m, c, res = linear_regression xs ys
        Assert.That(m, Is.EqualTo(0.0).Within(1e-10))
        Assert.That(c, Is.EqualTo(5.0).Within(1e-10))
        Assert.That(res, Is.EqualTo(0.0).Within(1e-10))

    [<Test>]
    member this.NoisyData() =
        // Points roughly on y = x
        // (0, 0.1), (1, 0.9), (2, 2.1)
        // Mean x = 1, Mean y = 1.0333...
        // This is just a regression test to ensure it runs and returns non-zero residuals
        let xs = [| 0.0; 1.0; 2.0 |]
        let ys = [| 0.1; 0.9; 2.1 |]
        let m, c, res = linear_regression xs ys
        Assert.That(m, Is.EqualTo(1.0).Within(0.1))
        Assert.That(res, Is.GreaterThan(0.0))

    [<Test>]
    member this.MismatchedLengths() =
        let xs = [| 0.0; 1.0 |]
        let ys = [| 0.0 |]

        Assert.Throws<System.Exception>(fun () -> linear_regression xs ys |> ignore)
        |> ignore

    [<Test>]
    member this.SinglePoint() =
        // With a single point, variance of x is 0, so division by zero occurs for slope m.
        // In this implementation:
        // sum((x - mean_x)^2) will be 0.
        // m will be Infinity or NaN.
        let xs = [| 1.0 |]
        let ys = [| 1.0 |]
        let m, c, res = linear_regression xs ys
        // Depending on F# / .NET float behavior, this might be NaN or Infinity.
        // Let's just check it doesn't crash unpredictably.
        // float 0.0 / float 0.0 is NaN
        Assert.That(Double.IsNaN(m), Is.True, "Slope should be NaN for single point")

[<TestFixture>]
type SolverTests() =
    [<Test>]
    member this.StraightLineZeroError() =
        let ctrlPts =
            [| { ty = SplinePointType.Smooth
                 x = Some 0.
                 y = Some 0.
                 th = None }
               { ty = SplinePointType.Smooth
                 x = Some 1.
                 y = Some 0.
                 th = None }
               { ty = SplinePointType.Smooth
                 x = Some 2.
                 y = Some 0.
                 th = None } |]

        let solver = Solver(ctrlPts, false, 1.0, false)
        solver.initialise ()
        let err = solver.computeErr ()
        Assert.That(err, Is.EqualTo(0.0).Within(1e-9))

    [<Test>]
    member this.CurvedLineNonZeroError() =
        let ctrlPts =
            [| { ty = SplinePointType.Smooth
                 x = Some 0.
                 y = Some 0.
                 th = None }
               { ty = SplinePointType.Smooth
                 x = Some 1.
                 y = Some 1.
                 th = None }
               { ty = SplinePointType.Smooth
                 x = Some 2.
                 y = Some 0.
                 th = None } |]

        let solver = Solver(ctrlPts, false, 1.0, false)
        solver.initialise ()
        let err = solver.computeErr ()
        Assert.That(err, Is.GreaterThan(0.0))

[<TestFixture>]
type VariablePointTests() =
    [<Test>]
    member this.VariableY_Collinear() =
        // 0,0 - 1,? - 2,0
        // Expect y to be 0 for perfect line
        let ctrlPts: DControlPoint array =
            [| { ty = SplinePointType.Smooth
                 x = Some 0.
                 y = Some 0.
                 th = None }
               { ty = SplinePointType.Smooth
                 x = Some 1.
                 y = None
                 th = None }
               { ty = SplinePointType.Smooth
                 x = Some 2.
                 y = Some 0.
                 th = None } |]

        let solver = Solver(ctrlPts, false, 0.0, false)
        solver.initialise ()

        let initialPts = solver.points ()
        Assert.That(initialPts.[1].y, Is.Not.NaN, "Initial y should not be NaN")

        solver.Solve(5000)
        let pts = solver.points ()
        Assert.That(pts.[1].y, Is.EqualTo(0.0).Within(1e-4))

    [<Test>]
    member this.VariableX_Symmetric() =
        // 0,0 - ?,1 - 2,0
        // Expect x to be 1 due to symmetry
        let ctrlPts: DControlPoint array =
            [| { ty = SplinePointType.Smooth
                 x = Some 0.
                 y = Some 0.
                 th = None }
               { ty = SplinePointType.Smooth
                 x = None
                 y = Some 1.
                 th = None }
               { ty = SplinePointType.Smooth
                 x = Some 2.
                 y = Some 0.
                 th = None } |]

        let solver = Solver(ctrlPts, false, 0.0, false)
        solver.initialise ()
        solver.Solve(5000)
        let pts = solver.points ()
        Assert.That(pts.[1].x, Is.EqualTo(1.0).Within(0.05))

    [<Test>]
    member this.VariableXY_Collinear() =
        // 0,0 - ?,? - 2,0
        // Initialized to midpoint (1,0). Since that's optimal (0 error), it should stay there.
        let ctrlPts: DControlPoint array =
            [| { ty = SplinePointType.Smooth
                 x = Some 0.
                 y = Some 0.
                 th = None }
               { ty = SplinePointType.Smooth
                 x = None
                 y = None
                 th = None }
               { ty = SplinePointType.Smooth
                 x = Some 2.
                 y = Some 0.
                 th = None } |]

        let solver = Solver(ctrlPts, false, 0.0, false)
        solver.initialise ()
        solver.Solve(5000)
        let pts = solver.points ()
        Assert.That(pts.[1].x, Is.EqualTo(1.0).Within(1e-4))
        Assert.That(pts.[1].y, Is.EqualTo(0.0).Within(1e-4))
