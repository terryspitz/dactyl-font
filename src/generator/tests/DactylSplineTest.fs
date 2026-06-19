module DactylSplineTest

open System
open NUnit.Framework
open Curves
open DactylSpline
open GlyphStringDefs
open GeneratorTypes
open Axes

let PI = System.Math.PI
let max_iter = 500
let dcp = DactylSpline.dcp

let pointToDcp (p: Point) =
    let x_opt = if p.x_fit then None else Some p.x
    let y_opt = if p.y_fit then None else Some p.y

    { ty = SplinePointType.Smooth
      x = x_opt
      y = y_opt
      th_in = None
      th_out = None }

[<TestFixture>]
type TestClass() =

    let solve_and_print_spline (spline: DactylSpline) =
        let svg, _, _ = spline.solveAndRenderSvg(max_iter, 1.0, 10.0, false, false, false)
        let svg = (String.Join(" ", svg))
        printfn "%A" svg
        svg.Trim()

    [<Test>]
    member this.CheckLinesCornerCorner() =
        let spline =
            DactylSpline(
                [| dcp SplinePointType.Corner 0. 0. None
                   dcp SplinePointType.Corner 1. 0. None |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckLinesCornerSmooth() =
        let spline =
            DactylSpline(
                [| dcp SplinePointType.Smooth 0. 0. None
                   dcp SplinePointType.Corner 1. 0. None |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckLinesSmoothCorner() =
        let spline =
            DactylSpline(
                [| dcp SplinePointType.Corner 0. 0. None
                   dcp SplinePointType.Smooth 1. 0. None |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckLinesSmoothSmooth() =
        let spline =
            DactylSpline(
                [| dcp SplinePointType.Smooth 0. 0. None
                   dcp SplinePointType.Smooth 1. 0. None |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckTwoPointCurvesWithAlignedTangents() =
        let expectedLineWithCurveTo = "M 0,0C 0.333,0 0.667,0 1.000,0"

        let spline =
            DactylSpline(
                [| dcp SplinePointType.Corner 0. 0. (Some 0.)
                   dcp SplinePointType.Corner 1. 0. None |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

        let spline =
            DactylSpline(
                [| dcp SplinePointType.Corner 0. 0. None
                   dcp SplinePointType.Corner 1. 0. (Some(-PI)) |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

        let spline =
            DactylSpline(
                [| dcp SplinePointType.Corner 0. 0. (Some 0.)
                   dcp SplinePointType.Corner 1. 0. (Some(-PI)) |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckTwoPointCurvesWithOtherTangents() =
        let spline =
            DactylSpline(
                [| dcp SplinePointType.Corner 0. 0. (Some(PI / 2.))
                   dcp SplinePointType.Corner 1. 0. (Some(PI)) |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

        let spline =
            DactylSpline(
                [| dcp SplinePointType.Corner 0. 0. (Some(PI / 2.))
                   dcp SplinePointType.Corner 1. 0. (Some(PI / 2.)) |],
                false
            )

        let svg = solve_and_print_spline spline
        Assert.That(svg, Does.StartWith("M 0,0"))
        Assert.That(svg, Does.EndWith("1,0"))

    [<Test>]
    member this.CheckFlatnessParam() =
        let spline =
            DactylSpline(
                [| dcp SplinePointType.Corner 0. 0. None
                   dcp SplinePointType.Smooth 0.5 1.0 None
                   dcp SplinePointType.Corner 1. 0. None |],
                false
            )

        let svgFlat0 =
            let svg, _, _ = spline.solveAndRenderSvg(5000, 0.0, 10.0, false, false, false) in svg |> String.concat " "

        let svgFlat10 =
            let svg, _, _ = spline.solveAndRenderSvg(5000, 1000.0, 10.0, false, false, false) in svg |> String.concat " "

        printfn "Flatness 0.0: %s" svgFlat0
        printfn "Flatness 10.0: %s" svgFlat10

        Assert.That(svgFlat0, Is.Not.EqualTo(svgFlat10))
    
    [<Test>]
    member this.EndTangentReversal() =
        // Horizontal line from 0,0 to 1,0.
        // User specifies tangent West (PI) at the end.
        // Spiro behavior (which we now match) expects this to be flipped to East (0) internally
        // so that the incoming control point (lpt) is to the West of the end point.
        let ctrlPts =
            [| dcp SplinePointType.Corner 0. 0. None
               dcp SplinePointType.Corner 1. 0. (Some PI) |]

        let spline = DactylSpline(ctrlPts, false)
        let bezPts = spline.solveAndGetPoints(max_iter, 1.0, 10.0, false)
        
        // check that th_in at the end is 0 (East), after being flipped from PI (West)
        Assert.That(bezPts.[1].th_in, Is.EqualTo(0.0).Within(1e-10))
        
        // check that lpt is West of the end point (1,0)
        let lpt = bezPts.[1].lpt()
        Assert.That(lpt.x, Is.LessThan(1.0))
        Assert.That(lpt.y, Is.EqualTo(0.0).Within(1e-10))

        // Conversely, a tangent specified at the START should NOT be flipped.
        let ctrlPtsStart =
            [| dcp SplinePointType.Corner 0. 0. (Some 0.0) // East
               dcp SplinePointType.Corner 1. 0. None |]

        let splineStart = DactylSpline(ctrlPtsStart, false)
        let bezPtsStart = splineStart.solveAndGetPoints(max_iter, 1.0, 10.0, false)
        
        Assert.That(bezPtsStart.[0].th_out, Is.EqualTo(0.0).Within(1e-10))
        let rpt = bezPtsStart.[0].rpt()
        Assert.That(rpt.x, Is.GreaterThan(0.0))
        Assert.That(rpt.y, Is.EqualTo(0.0).Within(1e-10))

    [<Test>]
    member this.TestF_StemToHookTransition() =
        // Points from user's bug report:
        // xtllc: x=80, y=510, th=PI/2 (North)
        // tcrW: x=255, y=630, th=PI (West)
        let ctrlPts =
            [| dcp SplinePointType.LineToCurve 80. 510. (Some(PI / 2.0))
               dcp SplinePointType.Corner 255. 630. (Some PI) |]

        let spline = DactylSpline(ctrlPts, false)
        let bezPts = spline.solveAndGetPoints(500, 1.0, 10.0, true)
        
        // Point 0 (xtllc) should NOT be flipped. It should point North.
        Assert.That(bezPts.[0].th_out, Is.EqualTo(PI / 2.0).Within(1e-10), "xtllc should point North")
        
        // Point 1 (tcrW) SHOULD be flipped to East (0).
        Assert.That(bezPts.[1].th_in, Is.EqualTo(0.0).Within(1e-10), "tcrW should be flipped to East")

    [<Test>]
    member this.TestF_StemToHookTransition_Smooth() =
        // Same coordinates but as a 3-point spline (matching "bllc-xtllc~tcrW")
        let ctrlPts =
            [| dcp SplinePointType.Corner 80. 0. None
               dcp SplinePointType.LineToCurve 80. 510. None // tangent set by preprocess
               dcp SplinePointType.Corner 255. 630. (Some PI) |]

        let spline = DactylSpline(ctrlPts, false)
        let bezPts = spline.solveAndGetPoints(500, 1.0, 10.0, true)
        
        // xtllc (pt 1) should point North (from stem line)
        Assert.That(bezPts.[1].th_in, Is.EqualTo(PI / 2.0).Within(1e-10), "xtllc in should be North")
        Assert.That(bezPts.[1].th_out, Is.EqualTo(PI / 2.0).Within(1e-10), "xtllc out should be North")
        
        // tcrW (pt 2) should be flipped to East
        Assert.That(bezPts.[2].th_in, Is.EqualTo(0.0).Within(1e-10), "tcrW should be flipped to East")



[<TestFixture>]
type LinearRegressionTests() =
    [<Test>]
    member this.PerfectLine() =
        // y = 2x + 1
        let xs = [| 0.0; 1.0; 2.0; 3.0 |]
        let ys = [| 1.0; 3.0; 5.0; 7.0 |]
        let m, c, res = linear_regression xs ys xs.Length
        Assert.That(m, Is.EqualTo(2.0).Within(1e-10))
        Assert.That(c, Is.EqualTo(1.0).Within(1e-10))
        Assert.That(res, Is.EqualTo(0.0).Within(1e-10))

    [<Test>]
    member this.HorizontalLine() =
        // y = 5
        let xs = [| 0.0; 10.0; 20.0 |]
        let ys = [| 5.0; 5.0; 5.0 |]
        let m, c, res = linear_regression xs ys xs.Length
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
        let m, c, res = linear_regression xs ys xs.Length
        Assert.That(m, Is.EqualTo(1.0).Within(0.1))
        Assert.That(res, Is.GreaterThan(0.0))

    [<Test>]
    member this.MismatchedLengths() =
        let xs = [| 0.0; 1.0 |]
        let ys = [| 0.0 |]

        Assert.Throws<System.IndexOutOfRangeException>(fun () -> linear_regression xs ys xs.Length |> ignore)
        |> ignore

    [<Test>]
    member this.SinglePoint() =
        // With a single point, variance of x is 0, so division by zero occurs for slope m.
        // In this implementation:
        // sum((x - mean_x)^2) will be 0.
        // m will be Infinity or NaN.
        let xs = [| 1.0 |]
        let ys = [| 1.0 |]
        let m, c, res = linear_regression xs ys xs.Length
        // Depending on F# / .NET float behavior, this might be NaN or Infinity.
        // Let's just check it doesn't crash unpredictably.
        // float 0.0 / float 0.0 is NaN
        Assert.That(Double.IsNaN(m), Is.True, "Slope should be NaN for single point")

[<TestFixture>]
type SolverTests() =
    [<Test>]
    member this.StraightLineZeroError() =
        let ctrlPts =
            [| dcp SplinePointType.Smooth 0. 0. None
               dcp SplinePointType.Smooth 1. 0. None
               dcp SplinePointType.Smooth 2. 0. None |]

        let solver = Solver(ctrlPts, false, 1.0, 10.0, false)
        solver.initialise ()
        let err = solver.computeErr ()
        Assert.That(err, Is.EqualTo(0.0).Within(1e-9))

    [<Test>]
    member this.CurvedLineNonZeroError() =
        let ctrlPts =
            [| dcp SplinePointType.Smooth 0. 0. None
               dcp SplinePointType.Smooth 1. 1. None
               dcp SplinePointType.Smooth 2. 0. None |]

        let solver = Solver(ctrlPts, false, 1.0, 10.0, false)
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
            [| dcp SplinePointType.Smooth 0. 0. None
               { ty = SplinePointType.Smooth
                 x = Some 1.
                 y = None
                 th_in = None
                 th_out = None }
               dcp SplinePointType.Smooth 2. 0. None |]

        let solver = Solver(ctrlPts, false, 0.0, 10.0, false)
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
            [| dcp SplinePointType.Smooth 0. 0. None
               { ty = SplinePointType.Smooth
                 x = None
                 y = Some 1.
                 th_in = None
                 th_out = None }
               dcp SplinePointType.Smooth 2. 0. None |]

        let solver = Solver(ctrlPts, false, 0.0, 10.0, false)
        solver.initialise ()
        solver.Solve 5000
        let pts = solver.points ()
        Assert.That(pts.[1].x, Is.EqualTo(1.0).Within(0.1))

    [<Test>]
    member this.VariableXY_Collinear() =
        // 0,0 - ?,? - 2,0
        // Initialized to midpoint (1,0). Since that's optimal (0 error), it should stay there.
        let ctrlPts: DControlPoint array =
            [| dcp SplinePointType.Smooth 0. 0. None
               { ty = SplinePointType.Smooth
                 x = None
                 y = None
                 th_in = None
                 th_out = None }
               dcp SplinePointType.Smooth 2. 0. None |]

        let solver = Solver(ctrlPts, false, 0.0, 10.0, false)
        solver.initialise ()
        solver.Solve(5000)
        let pts = solver.points ()
        Assert.That(pts.[1].x, Is.EqualTo(1.0).Within(1e-4))
        Assert.That(pts.[1].y, Is.EqualTo(0.0).Within(1e-4))

[<TestFixture>]
type AdvancedGeometricTests() =
    let PI = System.Math.PI

    [<Test>]
    member this.QuarterCircle() =
        // 3 points on unit circle: 0, 45, 90 degrees
        // (1,0), (0.7071, 0.7071), (0,1)
        let s45 = sin (PI / 4.0) // 0.7071...

        let ctrlPts =
            [| dcp SplinePointType.Smooth 1. 0. (Some(PI / 2.0)) // Tangent up
               dcp SplinePointType.Smooth s45 s45 None
               dcp SplinePointType.Smooth 0. 1. (Some(PI)) |] // Tangent left

        let solver = Solver(ctrlPts, false, 0.0, 10.0, false)
        solver.initialise ()
        solver.Solve(5000)

        let err = solver.computeErr ()
        // Error is sum of squared residuals of curvature * 10000.
        // 500 ~ 0.05% deviation.
        Assert.That(err, Is.LessThan(1000.0), "Error too high for quarter circle")

        let pts = solver.points ()
        // Tangent is stored in pts.[1].th_in (BezierPoint.th_in is float, not option)
        let th = pts.[1].th_in
        // 135 degrees is 3*PI/4 = 2.35619...
        Assert.That(th, Is.EqualTo(3.0 * PI / 4.0).Within(0.1))

    [<Test>]
    member this.InflectionSCurve() =
        // (-1, -1) -> (0,0) -> (1,1)
        // Tangents equal at ends (0.0 horizontal)
        let ctrlPts =
            [| { ty = SplinePointType.Smooth
                 x = Some -1.0
                 y = Some -1.0
                 th_in = Some 0.0
                 th_out = Some 0.0 }
               { ty = SplinePointType.Smooth
                 x = Some 0.0
                 y = Some 0.0
                 th_in = None
                 th_out = None }
               { ty = SplinePointType.Smooth
                 x = Some 1.0
                 y = Some 1.0
                 th_in = Some 0.0
                 th_out = Some 0.0 } |]

        let solver = Solver(ctrlPts, false, 0.0, 10.0, false)
        solver.initialise ()
        solver.Solve(5000)

        let err = solver.computeErr ()
        // S-curve fitting with Beziers is harder, expect higher residual on curvature linearity
        Assert.That(err, Is.LessThan(2.0e7), "Error too high for S-curve")

        let pts = solver.points ()
        let th = pts.[1].th_in
        Assert.That(th, Is.GreaterThan(0.0))
        // Should be steep but not vertical?
        Assert.That(th, Is.LessThan(PI / 2.0))

    [<Test>]
    member this.FitVsFixedCoordinate() =
        // V shape: (-1, 1) -> (0, 0) -> (1, 1).
        // If middle point is fixed at (0,0), it stays there.
        // If middle point y is optional, a smoother curve will lift it up to reduce curvature.

        // 1. Fixed case
        let ctrlPtsFixed =
            [| dcp SplinePointType.Smooth -1. 1. None
               dcp SplinePointType.Smooth 0. 0. None
               dcp SplinePointType.Smooth 1. 1. None |]

        let solverFixed = Solver(ctrlPtsFixed, false, 1.0, 10.0, false)
        solverFixed.initialise ()
        solverFixed.Solve(5000)
        let ptsFixed = solverFixed.points ()
        let yFixed = ptsFixed.[1].y

        // 2. Optional case (y is None)
        let ctrlPtsOpt =
            [| dcp SplinePointType.Smooth -1. 1. None
               { ty = SplinePointType.Smooth
                 x = Some 0.
                 y = None
                 th_in = None
                 th_out = None }
               dcp SplinePointType.Smooth 1. 1. None |]

        let solverOpt = Solver(ctrlPtsOpt, false, 1.0, 10.0, false)
        solverOpt.initialise ()
        solverOpt.Solve(500) // Lower iter for faster test, should be enough to move
        let ptsOpt = solverOpt.points ()
        let yOpt = ptsOpt.[1].y

        printfn "Fixed y: %f, Fitted y: %f" yFixed yOpt

        // Verify they are different
        Assert.That(yFixed, Is.EqualTo(0.0).Within(1e-5), "Fixed point should stay at 0.0")
        Assert.That(yOpt, Is.Not.EqualTo(yFixed).Within(0.1), "Fitted point should move away from 0.0")
        // In this case, since y=1 is a straight line (zero curvature), we expect yOpt to approach 1.0
        Assert.That(yOpt, Is.GreaterThan(0.1), "Fitted point should move up towards linear fit")

[<TestFixture>]
type LineToCurveTests() =
    let solve_and_print_spline (spline: DactylSpline) =
        let svg, _, _ = spline.solveAndRenderSvg(max_iter, 1.0, 10.0, false, false, false)
        let svg = (String.Join(" ", svg))
        printfn "%A" svg
        svg.Trim()

    [<Test>]
    member this.LineToCurve_FirstSegmentShouldBeLine() =
        let ctrlPts =
            [| dcp SplinePointType.Corner 0. 0. None
               dcp SplinePointType.LineToCurve 1. 0. None
               dcp SplinePointType.Corner 2. 1. None |]

        let spline = DactylSpline(ctrlPts, false)
        let svg = solve_and_print_spline spline
        // Expected: M 0,0 L 1,0 C ...
        // We look for "L 1,0" or similar float representation
        Assert.That(svg, Does.Match("M 0,0.*L 1(\.0+)?,0.*C"), "First segment should be a line")

    [<Test>]
    member this.CurveToLine_SecondSegmentShouldBeLine() =
        let ctrlPts =
            [| dcp SplinePointType.Corner 0. 0. None
               dcp SplinePointType.CurveToLine 1. 1. None
               dcp SplinePointType.Corner 2. 1. None |]

        let spline = DactylSpline(ctrlPts, false)
        let svg = solve_and_print_spline spline
        // Expected: M 0,0 C ... L 2,1
        Assert.That(svg, Does.Match("M 0,0.*C.*L 2,1"), "Second segment should be a line")

    // Spline grid failing cases

    [<Test>]
    member this.LineToCurve_G2_Open_ShouldRenderValidCurve() =
        // C/LC/G2 open: line then curve, no arm explosion
        let ctrlPts =
            [| dcp SplinePointType.Corner 50. 50. None
               dcp SplinePointType.LineToCurve 250. 50. None
               dcp SplinePointType.Smooth 150. 200. None |]

        let spline = DactylSpline(ctrlPts, false)
        let svg = solve_and_print_spline spline
        let bezPts, _, _, _ = spline.solveAndRenderFull(max_iter, 1.0, 10.0, false, false, false)
        printfn "bezPts: %A" (bezPts |> Array.map (fun bp -> sprintf "ld=%.2f rd=%.2f" bp.ld bp.rd))
        // First segment should be a line (horizontal)
        Assert.That(svg, Does.Match("L 250(\.0+)?,50(\.0+)?"), "First segment Corner→LineToCurve should be a line")
        // No arm should blow up
        let ok = bezPts |> Array.forall (fun bp -> abs bp.ld < 1e5 && abs bp.rd < 1e5)
        Assert.That(ok, Is.True, "Arm lengths should stay within bounds (no solver divergence)")

    [<Test>]
    member this.LineToCurve_CurveToLine_Closed_ShouldRenderValidCurve() =
        // C/LC/CL closed: corner + LC + CL, closed triangle — both penalties must not fight
        let ctrlPts =
            [| dcp SplinePointType.Corner 50. 50. None
               dcp SplinePointType.LineToCurve 250. 50. None
               dcp SplinePointType.CurveToLine 150. 200. None |]

        let spline = DactylSpline(ctrlPts, true)
        let svg = solve_and_print_spline spline
        let bezPts, _, _, _ = spline.solveAndRenderFull(max_iter, 1.0, 10.0, false, false, false)
        printfn "bezPts: %A" (bezPts |> Array.map (fun bp -> sprintf "ld=%.2f rd=%.2f" bp.ld bp.rd))
        // First segment should be a line (horizontal)
        Assert.That(svg, Does.Match("L 250(\.0+)?,50(\.0+)?"), "First segment Corner→LineToCurve should be a line")
        // No arm should blow up
        let ok = bezPts |> Array.forall (fun bp -> abs bp.ld < 1e5 && abs bp.rd < 1e5)
        Assert.That(ok, Is.True, "Arm lengths should stay within bounds (no solver divergence)")

    [<Test>]
    member this.SplineGrid_AllCombinations_NoArmDivergence() =
        // Mirror the Api.solveSplineGrid logic: all 4^3 type combos × open/closed.
        // Every cell must produce arm lengths < 1e5 (otherwise shows as X in the UI).
        // For closed paths: also check G1 smoothness at P0 when P0=Smooth and P0 is
        // not at the start of a section that follows a line from P2 (P2≠CL).
        let pointTypes =
            [| SplinePointType.Corner
               SplinePointType.Smooth
               SplinePointType.LineToCurve
               SplinePointType.CurveToLine |]
        let typeNames = [| "Corner"; "Smooth"; "LC"; "CL" |]
        let tri = [| (50.0, 50.0); (250.0, 50.0); (150.0, 200.0) |]
        let failures = System.Collections.Generic.List<string>()

        for isClosed in [| false; true |] do
            for withTangent in [| false; true |] do
                for it0 in 0 .. 3 do
                    for it1 in 0 .. 3 do
                        for it2 in 0 .. 3 do
                        let types = [| pointTypes.[it0]; pointTypes.[it1]; pointTypes.[it2] |]
                        let pts =
                            Array.init 3 (fun i ->
                                let (x, y) = tri.[i]
                                let th =
                                    if withTangent && i = 2 then
                                        Some (if isClosed then System.Math.PI else 0.0)
                                    else None
                                dcp types.[i] x y th)
                        try
                            let spline = DactylSpline(pts, isClosed)
                            let bezPts, _, _, _ = spline.solveAndRenderFull(200, 1.0, 10.0, false, false, false)

                            // Arm divergence check
                            let armOk = bezPts |> Array.forall (fun bp -> abs bp.ld < 1e5 && abs bp.rd < 1e5)
                            if not armOk then
                                let maxArm = bezPts |> Array.map (fun bp -> max (abs bp.ld) (abs bp.rd)) |> Array.max
                                failures.Add(sprintf "[%s,%s,%s] %s arm=%.0f"
                                    typeNames.[it0] typeNames.[it1] typeNames.[it2]
                                    (if isClosed then "closed" else "open") maxArm)

                            // G1 smoothness at P0: only for closed paths where P0=Smooth
                            // and P0 does NOT follow a line from P2 (i.e. P2≠CL).
                            // When P2=CL the segment P2→P0 is a straight line and P0 is
                            // the start of a new section; a kink there is expected because
                            // the user did not declare P0 as LC.
                            if isClosed
                               && types.[0] = SplinePointType.Smooth
                               && types.[2] <> SplinePointType.CurveToLine then
                                let p0 = bezPts.[0]
                                let diff = abs (norm (p0.th_in - p0.th_out))
                                if diff > 0.1 then
                                    failures.Add(sprintf "[%s,%s,%s] closed P0 not smooth: th_in=%.3f th_out=%.3f diff=%.3f"
                                        typeNames.[it0] typeNames.[it1] typeNames.[it2]
                                        p0.th_in p0.th_out diff)
                        with ex ->
                            failures.Add(sprintf "[%s,%s,%s] %s EXCEPTION: %s"
                                typeNames.[it0] typeNames.[it1] typeNames.[it2]
                                (if isClosed then "closed" else "open") ex.Message)

        if failures.Count > 0 then
            printfn "Failing grid cells:"
            for f in failures do printfn "  %s" f

        Assert.That(failures.Count, Is.EqualTo(0), sprintf "%d grid cells failed:\n%s" failures.Count (String.concat "\n" failures))

    [<Test>]
    member this.Closed_G2_G2_LC_SmoothAtP0() =
        // Regression: [G2,G2,LC] closed was not smooth at P0.
        // The section collector used to start at i=0 (Smooth), splitting P0 between two
        // independent solvers that each optimised its tangent separately.  The fix starts
        // the loop at the first hard-boundary (P2=LC) so P0 is always interior to one section.
        let ctrlPts =
            [| dcp SplinePointType.Smooth 50. 50. None
               dcp SplinePointType.Smooth 250. 50. None
               dcp SplinePointType.LineToCurve 150. 200. None |]
        let spline = DactylSpline(ctrlPts, true)
        let bezPts, _, _, _ = spline.solveAndRenderFull(500, 1.0, 10.0, false, false, false)
        // P0 must be G1: th_in = th_out (same tangent, no kink)
        let p0 = bezPts.[0]
        let angleDiff = abs (norm (p0.th_in - p0.th_out))
        printfn "P0 th_in=%.4f th_out=%.4f diff=%.4f" p0.th_in p0.th_out angleDiff
        Assert.That(angleDiff, Is.LessThan(1e-3), "P0 must be smooth (th_in ≈ th_out) in [G2,G2,LC] closed")

    [<Test>]
    member this.Closed_G2_G2_CL_KinkAtP0() =
        // [G2,G2,CL] closed: the line is P2→P0.  P0 is Smooth (not LC), so the
        // user has NOT declared a smooth line-to-curve join at P0.  The expected
        // behaviour is a kink at P0 (th_in from line ≠ th_out toward P1).
        // The fix must NOT force LC behaviour on P0 in this case.
        let ctrlPts =
            [| dcp SplinePointType.Smooth 50. 50. None
               dcp SplinePointType.Smooth 250. 50. None
               dcp SplinePointType.CurveToLine 150. 200. None |]
        let spline = DactylSpline(ctrlPts, true)
        let bezPts, _, _, _ = spline.solveAndRenderFull(500, 1.0, 10.0, false, false, false)
        // Arms must not blow up (solver must be stable)
        let p0 = bezPts.[0]
        let maxArm = bezPts |> Array.map (fun bp -> max (abs bp.ld) (abs bp.rd)) |> Array.max
        printfn "P0 th_in=%.4f th_out=%.4f diff=%.4f  maxArm=%.1f" p0.th_in p0.th_out (abs (norm (p0.th_in - p0.th_out))) maxArm
        Assert.That(maxArm, Is.LessThan(1e5), "arm lengths should stay within bounds for [G2,G2,CL] closed")

[<TestFixture>]
type IntegrationTests() =
    [<Test>]
    member this.BracketSyntaxIntegration() =
        // Verify that parsing string inputs with brackets leads to fitted points.
        let axes =
            { Axes.Axes.DefaultAxes with
                width = 1000
                height = 1000 }

        let glyphDefs = FontMetrics(axes)

        // Define V shape: TL -> B(L) -> TR
        // TL=(0,1000), TR=(1000,1000), BL= (0,0) (nominal) with y=0
        // B(L) means x=0 is optional.

        // Parsing
        let p1, _, _, _ = parse_point glyphDefs "tl"
        let p2, _, _, _ = parse_point glyphDefs "b(l)"
        let p3, _, _, _ = parse_point glyphDefs "tr"

        let cp1 = pointToDcp p1
        let cp2 = pointToDcp p2
        let cp3 = pointToDcp p3

        // Verify parsing result for p2 flags
        let yfit, xfit = p2.y_fit, p2.x_fit
        Assert.That(yfit, Is.False, "Parser should not set y_fit for b")
        Assert.That(xfit, Is.True, "Parser should set x_fit for (l)")

        // Verify DControlPoint conversion (x should be None)
        Assert.That(cp2.x, Is.EqualTo(None), "DControlPoint x should be None")
        Assert.That(cp2.y, Is.Not.EqualTo(None), "DControlPoint y should be distinct Some")

        // Solve
        // Fixed points at (0,1000) and (1000,1000) y.
        // Middle point nominal x=0 (left), y=0 (bottom).
        // Solver should move x towards 500 (center) to minimize curvature of (0,1000)->(x,0)->(1000,1000).
        let solver = Solver([| cp1; cp2; cp3 |], false, 1.0, 10.0, false)
        solver.initialise ()

        solver.Solve(2000)
        let finalX = solver.points().[1].x

        printfn "Final X: %f (Left is 0, Right is 1000)" finalX
        Assert.That(finalX, Is.GreaterThan(100.0), "Point should have moved right significantly from 0")

    [<Test>]
    member this.TestAsymmetricFit() =
        // tl -> hl -> b(c) -> tr
        // Verify that adding points on the left 'pulls' the fit that way.
        let axes =
            { Axes.Axes.DefaultAxes with
                width = 1000
                height = 1000 }

        let glyphDefs = FontMetrics(axes)

        let p1, _, _, _ = parse_point glyphDefs "tl"
        let p2, _, _, _ = parse_point glyphDefs "hl" // (0, 500)
        let p3, _, _, _ = parse_point glyphDefs "b(c)" // (500, 0) optional x
        let p4, _, _, _ = parse_point glyphDefs "tr"

        let cp1 = pointToDcp p1
        let cp2 = pointToDcp p2
        let cp3 = pointToDcp p3
        let cp4 = pointToDcp p4

        // Check initial assumptions
        Assert.That(cp3.x, Is.EqualTo(None), "Middle point x should be optional")

        let solver = Solver([| cp1; cp2; cp3; cp4 |], false, 1.0, 10.0, false)
        solver.initialise ()

        // Solve; use best-so-far if max iterations reached (mirrors solveSection behaviour)
        try solver.Solve(5000) with _ -> ()
        let finalX = solver.points().[2].x

        printfn "Final X: %f (Center is 500)" finalX
        // Solver moves point RIGHT to minimize curvature of the turn from the vertical HL segment.
        // A wider turn (larger X) reduces energy compared to a tight turn (small X).
        Assert.That(
            finalX,
            Is.GreaterThan(500.0),
            "Point should move RIGHT of center to allow wider turn from vertical left edge"
        )

    [<Test>]
    member this.TestAsymmetricFit_U_Shape() =
        // tl-tbbl~b(c)~tbr-tr
        // Curve is from (0,333) -> (x,0) -> (1000,500).
        // Left drop: 333 units (shorter arc → stiffer curvature per unit length).
        // Right drop: 500 units (longer arc → gentler curvature per unit length).
        // Stiffer left side pushes the optimal x to the RIGHT → fitted x > 500.

        let axes =
            { Axes.Axes.DefaultAxes with
                width = 1000
                height = 1000 }

        let glyphDefs = FontMetrics(axes)

        let p1, _, _, _ = parse_point glyphDefs "tl"
        let p2, _, _, _ = parse_point glyphDefs "tbbl"
        let p3, _, _, _ = parse_point glyphDefs "b(c)"
        let p4, _, _, _ = parse_point glyphDefs "tbr"
        let p5, _, _, _ = parse_point glyphDefs "tr"

        printfn "P1: %A" p1
        printfn "P2: %A" p2
        printfn "P3: %A" p3
        printfn "P4: %A" p4
        printfn "P5: %A" p5

        // Manually set types AND tangents for line transitions, as Solver doesn't do this automatically
        // (DSpline.solveAndRenderTuple usually handles this).
        let cp1 =
            { pointToDcp p1 with
                ty = SplinePointType.Corner }

        // tl(0,1000) -> tbbl(0,333). Downwards (-PI/2).
        let cp2 =
            { pointToDcp p2 with
                ty = SplinePointType.LineToCurve
                th_in = Some(-System.Math.PI / 2.0)
                th_out = Some(-System.Math.PI / 2.0) }

        let cp3 =
            { pointToDcp p3 with
                ty = SplinePointType.Smooth }

        // tbr(1000,500) -> tr(1000,1000). Upwards (PI/2).
        let cp4 =
            { pointToDcp p4 with
                ty = SplinePointType.CurveToLine
                th_in = Some(System.Math.PI / 2.0)
                th_out = Some(System.Math.PI / 2.0) }

        let cp5 =
            { pointToDcp p5 with
                ty = SplinePointType.Corner }

        let solver = Solver([| cp1; cp2; cp3; cp4; cp5 |], false, 1.0, 10.0, false)
        solver.initialise ()
        // Use best-so-far if max iterations reached (mirrors solveSection behaviour)
        try solver.Solve(10000) with _ -> ()

        let finalX = solver.points().[2].x

        // With corrected arc-length sampling (no end-extrapolation), the stiffer LEFT
        // side (shorter 333-unit drop) pushes the bottom point rightward — away from
        // the stiff side — giving x > 500.
        Assert.That(
            finalX,
            Is.GreaterThan(500.0),
            "fitted x of the bottom point should be to the right of centre (stiffer left side pushes right)"
        )

    [<Test>]
    member this.TestAsymmetricFit_3PointProductionPath() =
        // Same geometry as TestAsymmetricFit_U_Shape but using the PRODUCTION 3-point
        // inner solver [LC, Smooth, CL] exactly as DactylSpline.solveSection would call it.
        // LC at (0,333) going downward, CL at (1000,500) going upward, Smooth x free.
        //
        // Arc-length equalization predicts x ≈ 570:
        //   left arc ≈ sqrt(x² + 333²), right arc ≈ sqrt((1000-x)² + 500²)
        //   equal when x² + 110889 = (1000-x)² + 250000  →  x ≈ 570 > 500
        //
        // The right endpoint is farther from the bottom (500 > 333), so the
        // equal-arc-length point sits RIGHT of centre.
        let ctrlPts3 =
            [| { ty = SplinePointType.LineToCurve
                 x = Some 0.; y = Some 333.
                 th_in = Some(-System.Math.PI / 2.0); th_out = Some(-System.Math.PI / 2.0) }
               { ty = SplinePointType.Smooth
                 x = None; y = Some 0.
                 th_in = None; th_out = None }
               { ty = SplinePointType.CurveToLine
                 x = Some 1000.; y = Some 500.
                 th_in = Some(System.Math.PI / 2.0); th_out = Some(System.Math.PI / 2.0) } |]

        let solver3 = Solver(ctrlPts3, false, 1.0, 10.0, false)
        solver3.initialise ()
        try solver3.Solve(5000) with _ -> ()
        let x3 = solver3.points().[1].x
        printfn "3-point production solver x = %.2f  (center=500, arc-len prediction≈570)" x3

        // The right junction is higher (500 vs 333), so equal arc lengths put x right of centre.
        Assert.That(x3, Is.GreaterThan(500.0),
            "right junction is higher → equal-arc-length optimum is right of centre")

[<TestFixture>]
type BracketFittingTests() =
    let axes = Axes.DefaultAxes
    let metrics = FontMetrics(axes)

    // Mirrors Font.toDactylSplineControlPoints for G2/Smooth-only open curves.
    let knotsToDcps (knots: Knot list) : DControlPoint array =
        knots
        |> List.map (fun k ->
            // Use equality on the enum values imported from GeneratorTypes (G2, LineToCurve, CurveToLine)
            let ty =
                if k.ty = G2 || k.ty = SpiroPointType.SpiroPointType.G4 then SplinePointType.Smooth
                elif k.ty = LineToCurve then SplinePointType.LineToCurve
                elif k.ty = CurveToLine then SplinePointType.CurveToLine
                else SplinePointType.Corner
            { ty = ty
              x = if k.pt.x_fit then None else Some k.pt.x
              y = if k.pt.y_fit then None else Some k.pt.y
              th_in = k.th_in
              th_out = k.th_out })
        |> Array.ofList

    let solveGlyphCurve (def: string) : BezierPoint array =
        match GlyphStringDefs.parse_curve metrics def false with
        | Curve(knots, isClosed) ->
            let ctrlPts = knotsToDcps knots
            let spline = DactylSpline(ctrlPts, isClosed)
            spline.solveAndGetPoints(max_iter, 1.0, 10.0, false)
        | _ -> failwith "expected Curve element"

    [<Test>]
    member _.BracketX_ValueInsideBracketsIsIgnored() =
        // x(c) and x(cr) both have x_fit=true → DControlPoint.x = None (free variable).
        // Initialization comes from neighbor average, not the parsed value, so both
        // produce identical solver inputs and must produce identical solved results.
        let pts1 = solveGlyphCurve "xor~x(c)~(xb)l~b(c)~bor"
        let pts2 = solveGlyphCurve "xor~x(cr)~(xb)l~b(c)~bor"
        // Top point (index 1) x must be the same regardless of c vs cr inside brackets.
        Assert.That(pts2.[1].x, Is.EqualTo(pts1.[1].x).Within(1e-6),
            "x(c) and x(cr) should produce identical top-point x — parsed value inside brackets is discarded")

    [<Test>]
    member _.BracketX_FreesCoordinateFromFixed() =
        // A free x (None) is optimized; a fixed x stays at its parsed value.
        // So after solving, "xc" (fixed) should have the top-point x equal to C,
        // while "x(c)" (free) may differ.
        let ptsFixed = solveGlyphCurve "xor~xc~(xb)l~bc~bor"
        let ptsFree  = solveGlyphCurve "xor~x(c)~(xb)l~b(c)~bor"
        Assert.That(ptsFixed.[1].x, Is.EqualTo(metrics.C).Within(1e-6),
            "Fixed x=C must stay at C after solving")
        // The free top-point x must be finite (optimizer ran without error)
        Assert.That(Double.IsFinite(ptsFree.[1].x), Is.True,
            "Free top-point x should be finite after solving")

[<TestFixture>]
type FlatnessTests() =
    let axes = Axes.DefaultAxes
    let metrics = FontMetrics(axes)
    let max_iter = 500

    let knotsToDcps (knots: Knot list) : DControlPoint array =
        knots
        |> List.map (fun k ->
            let ty =
                if k.ty = G2 || k.ty = SpiroPointType.SpiroPointType.G4 then SplinePointType.Smooth
                elif k.ty = LineToCurve then SplinePointType.LineToCurve
                elif k.ty = CurveToLine then SplinePointType.CurveToLine
                else SplinePointType.Corner
            { ty = ty
              x = if k.pt.x_fit then None else Some k.pt.x
              y = if k.pt.y_fit then None else Some k.pt.y
              th_in = k.th_in
              th_out = k.th_out })
        |> Array.ofList

    let solveCWithFlatnessAndIter (flatness: float) (iters: int) =
        match GlyphStringDefs.parse_curve metrics "xor~x(c)~(xb)l~b(c)~bor" false with
        | Curve(knots, isClosed) ->
            let ctrlPts = knotsToDcps knots
            let spline = DactylSpline(ctrlPts, isClosed)
            spline.solveAndGetPoints(iters, flatness, 10.0, false)
        | _ -> failwith "expected Curve"

    let solveCWithFlatness flatness = solveCWithFlatnessAndIter flatness 500

    [<Test; Explicit("Diagnostic: print top-x and per-segment curvature of c across flatness values")>]
    member _.PrintFlatnessEffect() =
        printfn "=== Iteration count sweep (flatness=1.0) ==="
        for iters in [| 50; 100; 200; 500; 1000 |] do
            let pts = solveCWithFlatnessAndIter 1.0 iters
            printfn "iters=%-4d  top-x=%.2f" iters pts.[1].x
        printfn ""
        printfn "=== Flatness sweep (500 iters) ==="
        for f in [| 0.0; 0.5; 1.0; 5.0; 20.0; 100.0 |] do
            let pts = solveCWithFlatness f
            let curveData = computeCurvatureData pts false
            printfn "flatness=%-6g  top-x=%.2f" f pts.[1].x
            for si in 0 .. curveData.segments.Length - 1 do
                let seg = curveData.segments.[si]
                let actualStartK = seg.samples.[0].curvature * 10000.0
                let actualEndK   = seg.samples.[seg.samples.Length-1].curvature * 10000.0
                printfn "  seg%d  startK(actual)=%.1f  endK(actual)=%.1f  gap=%.1f"
                    si actualStartK actualEndK (actualEndK - actualStartK)

    [<Test; Explicit("Diagnostic: render c outline SVG for visual inspection")>]
    member _.PrintCOutlineSvg() =
        let axes = { Axes.DefaultAxes with max_spline_iter = 500 }
        let font = Font.Font(axes)
        let outline = font.CharToOutline 'c'
        let svg, _, _ = font.elementToSvg outline
        printfn "=== 'c' outline SVG (font rendering, 500 iters) ==="
        for line in svg do printfn "%s" line
        // Also print solved backbone points
        printfn "=== 'c' backbone bezier points ==="
        let pts = solveCWithFlatness 1.0
        for i in 0 .. pts.Length - 1 do
            printfn "  pt%d  x=%.1f  y=%.1f  th_in=%.3f  th_out=%.3f  ld=%.1f  rd=%.1f"
                i pts.[i].x pts.[i].y pts.[i].th_in pts.[i].th_out pts.[i].ld pts.[i].rd

    [<Test>]
    member _.FlatnessZeroVsHighGivesDifferentTopX() =
        let ptsLow  = solveCWithFlatness 0.0
        let ptsHigh = solveCWithFlatness 100.0
        let xLow  = ptsLow.[1].x
        let xHigh = ptsHigh.[1].x
        printfn "flatness=0.0   top-x=%.2f" xLow
        printfn "flatness=100.0 top-x=%.2f" xHigh
        Assert.That(abs (xLow - xHigh), Is.GreaterThan(5.0),
            sprintf "Flatness should move top-x by >5 units (got %.2f vs %.2f)" xLow xHigh)

    [<Test>]
    member _.SolveIsTranslationInvariant() =
        // The same curve solved at two different absolute positions must produce
        // the same SHAPE. charToElem translates glyphs by (thickness,thickness)
        // before solving, so any position-dependence makes the Font tab diverge
        // from the Splines tab.
        match GlyphStringDefs.parse_curve metrics "xor~x(c)~(xb)l~b(c)~bor" false with
        | Curve(knots, isClosed) ->
            let solveAt dx dy =
                let shifted =
                    knots
                    |> List.map (fun k ->
                        { k with pt = { k.pt with x = k.pt.x + dx; y = k.pt.y + dy } })
                let ctrlPts = knotsToDcps shifted
                let spline = DactylSpline(ctrlPts, isClosed)
                spline.solveAndGetPoints(500, 0.5, 10.0, false)
            let basePts = solveAt 0.0 0.0
            let shiftPts = solveAt 30.0 30.0
            // Compare free top-x relative to the left edge (pt2.x), which is fixed.
            let baseTopRel  = basePts.[1].x  - basePts.[2].x
            let shiftTopRel = shiftPts.[1].x - shiftPts.[2].x
            printfn "untranslated  top-x relative = %.2f" baseTopRel
            printfn "translated+30 top-x relative = %.2f" shiftTopRel
            // Tolerance 5.0: solver is approximately translation-invariant (Nelder-Mead is not
            // exactly deterministic across positions). 5.0 still catches large regressions.
            Assert.That(abs (baseTopRel - shiftTopRel), Is.LessThan(5.0),
                sprintf "Solve must be translation-invariant: relative top-x differs by %.1f"
                    (abs (baseTopRel - shiftTopRel)))
        | _ -> failwith "expected Curve"

    [<Test; Explicit("Diagnostic: print backbone points for symmetry glyphs across iteration counts")>]
    member _.PrintSymmetryBackbones() =
        for iters in [| 500; 1000; 2000; 5000 |] do
            let font = Font.Font({ Axes.DefaultAxes with dactyl_spline = true; outline = true; max_spline_iter = iters })
            printfn "=== max_spline_iter = %d ===" iters
            for ch in [ 'C'; 'O' ] do
                let pts = font.charToSolvedBackbonePoints ch
                printfn "  '%c': %s" ch
                    (pts |> List.map (fun (x, y) -> sprintf "(%.1f,%.1f)" x y) |> String.concat " ")

    [<Test; Explicit("Diagnostic: render 'c' from Splines-tab path and Font-tab path to SVG files")>]
    member _.RenderBothPathsToSvg() =
        let wrap (color: string) (fill: string) (body: string) =
            sprintf "<svg xmlns='http://www.w3.org/2000/svg' viewBox='-80 -80 520 580'>\n\
                     <g transform='matrix(1 0 0 -1 0 440)'>\n\
                     <path d='%s' fill='%s' stroke='%s' stroke-width='3'/>\n\
                     </g></svg>" body fill color

        // --- Splines-tab path: solveSplineEditor — backbone only, flatness=1.0, 1000 iters ---
        match GlyphStringDefs.parse_curve metrics "xor~x(c)~(xb)l~b(c)~bor" false with
        | Curve(knots, isClosed) ->
            let ctrlPts = knotsToDcps knots
            let spline = DactylSpline(ctrlPts, isClosed)
            let _, pathSvg, _, _ = spline.solveAndRenderFull(1000, 1.0, 10.0, false, false, false)
            let body = String.concat " " pathSvg
            System.IO.File.WriteAllText("/tmp/c_splines_backbone.svg", wrap "blue" "none" body)
            printfn "Splines backbone path: %s" body
        | _ -> failwith "expected Curve"

        // --- Font-tab path: CharToOutline — full stroked outline, axes.flatness, max_spline_iter ---
        let axes = { Axes.DefaultAxes with max_spline_iter = 500 }
        let font = Font.Font(axes)
        let outline = font.CharToOutline 'c'
        let svg, _, _ = font.elementToSvg outline
        let body = String.concat " " svg
        System.IO.File.WriteAllText("/tmp/c_font_outline.svg", wrap "black" "#00000030" body)
        printfn "Font outline path: %s" body

        // --- Splines-tab outline (getSplineOutlinePath equivalent): same as Font path? ---
        match GlyphStringDefs.parse_curve metrics "xor~x(c)~(xb)l~b(c)~bor" false with
        | Curve(knots, isClosed) ->
            let outlineFont = Font.Font({ axes with outline = true; filled = true; dactyl_spline = true })
            let outline2 = outlineFont.getDactylSansOutlines (Curve(knots, isClosed))
            let svg2, _, _ = outlineFont.elementToSvg outline2
            let body2 = String.concat " " svg2
            System.IO.File.WriteAllText("/tmp/c_splines_outline.svg", wrap "green" "#00800030" body2)
            printfn "Splines outline path: %s" body2
        | _ -> failwith "expected Curve"

[<TestFixture>]
type CurvatureBalanceTests() =

    [<Test>]
    member _.ClusterMergesNearbyAndDropsSingletons() =
        // Three magnitudes near 0.010 (within 15% of each other) should merge into
        // one attractor; a lone 0.050 has no siblings and must be dropped.
        let samples =
            [| (0.0100, 1.0); (0.0105, 1.0); (0.0098, 1.0); (0.0500, 1.0) |]
        let attractors = clusterCurvatures samples 0.15 0.001 2
        Assert.That(attractors.Length, Is.EqualTo(1),
            "expected a single merged cluster, lone value dropped")
        Assert.That(attractors.[0], Is.EqualTo(0.0101).Within(0.0005),
            "attractor should be the weighted mean of the merged cluster")

    [<Test>]
    member _.ClusterIsSignAndStraightAware() =
        // Opposite-sign equal-magnitude arcs cluster together (matched radius);
        // near-straight magnitudes below minMag are excluded.
        let samples =
            [| (0.0100, 1.0); (-0.0102, 1.0); (0.0001, 5.0); (-0.0001, 5.0) |]
        let attractors = clusterCurvatures samples 0.15 0.001 2
        Assert.That(attractors.Length, Is.EqualTo(1),
            "mirror arcs should form one magnitude cluster; near-straight excluded")
        Assert.That(attractors.[0], Is.EqualTo(0.0101).Within(0.0005))

    [<Test>]
    member _.DisabledAxisYieldsNoAttractors() =
        let font = Font.Font(Axes.DefaultAxes)
        Assert.That(font.curvatureBalance.weight, Is.EqualTo(0.0),
            "default axis (0) leaves balancing disabled")
        Assert.That(font.curvatureBalance.attractors.Length, Is.EqualTo(0))

    [<Test>]
    member _.EnabledAxisProducesAttractors() =
        let font = Font.Font({ Axes.DefaultAxes with curvature_balance = 5.0 })
        let balance = font.curvatureBalance
        Assert.That(balance.weight, Is.EqualTo(5.0))
        Assert.That(balance.attractors.Length, Is.GreaterThan(0),
            "font-wide measuring pass should find shared curvature levels")
        Assert.That(balance.attractors |> Array.forall (fun a -> a > 0.0), Is.True,
            "attractors are positive magnitudes")

    [<Test>]
    member _.BalancingChangesSomeGlyphOutline() =
        // Enabling balancing should perturb at least one curved glyph's outline,
        // while leaving overall rendering finite and valid.
        let axesOff = { Axes.DefaultAxes with max_spline_iter = 500 }
        let axesOn = { axesOff with curvature_balance = 8.0 }
        let fontOff = Font.Font(axesOff)
        let fontOn = Font.Font(axesOn)

        let render (font: Font.Font) ch =
            let svg, _, _ = font.elementToSvg (font.CharToOutline ch)
            String.concat " " svg

        let anyDifferent =
            [ 'o'; 'b'; 'd'; 'n'; 'e'; 'c'; 'a' ]
            |> List.exists (fun ch -> render fontOff ch <> render fontOn ch)

        Assert.That(anyDifferent, Is.True,
            "expected balancing to alter at least one glyph's outline")

    [<Test; Explicit("Diagnostic: print font-wide attractors and count of glyphs changed by balancing")>]
    member _.PrintBalanceSummary() =
        for weight in [ 5.0; 20.0 ] do
            let axesOff = { Axes.DefaultAxes with max_spline_iter = 500 }
            let axesOn = { axesOff with curvature_balance = weight }
            let fontOff = Font.Font(axesOff)
            let fontOn = Font.Font(axesOn)
            let balance = fontOn.curvatureBalance
            printfn "=== curvature_balance = %g ===" weight
            printfn "  attractors (radius units): %s"
                (balance.attractors
                 |> Array.map (fun a -> sprintf "%.0f" (10000.0 / a))
                 |> String.concat ", ")

            let render (font: Font.Font) ch =
                let svg, _, _ = font.elementToSvg (font.CharToOutline ch)
                String.concat " " svg

            let chars =
                [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ]
            let changed =
                chars |> List.filter (fun ch -> render fontOff ch <> render fontOn ch)
            printfn "  changed %d / %d glyphs: %s"
                changed.Length chars.Length (System.String(List.toArray changed))

            // Measure actual geometric displacement (max backbone point shift), not just
            // string inequality, so we know whether the effect is visible or sub-pixel.
            let backbone (font: Font.Font) ch = font.charToSolvedBackbonePoints ch
            let maxShift ch =
                let a = backbone fontOff ch
                let b = backbone fontOn ch
                if a.Length <> b.Length || a.IsEmpty then nan
                else
                    List.zip a b
                    |> List.map (fun ((x1, y1), (x2, y2)) -> sqrt ((x2 - x1) ** 2.0 + (y2 - y1) ** 2.0))
                    |> List.max
            for ch in [ 'o'; 'b'; 'd'; 'e'; 'c'; 'a'; 'n'; 'O'; 'C'; 'S'; '8' ] do
                printfn "    '%c' max backbone shift = %.2f units" ch (maxShift ch)

