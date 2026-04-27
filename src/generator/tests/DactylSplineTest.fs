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
        let svg, _, _ = spline.solveAndRenderSvg (max_iter, 1.0, false, false, false)
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
            let svg, _, _ = spline.solveAndRenderSvg (5000, 0.0, false, false, false) in svg |> String.concat " "

        let svgFlat10 =
            let svg, _, _ = spline.solveAndRenderSvg (5000, 1000.0, false, false, false) in svg |> String.concat " "

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
        let bezPts = spline.solveAndGetPoints(max_iter, 1.0, false)
        
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
        let bezPtsStart = splineStart.solveAndGetPoints(max_iter, 1.0, false)
        
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
        let bezPts = spline.solveAndGetPoints(500, 1.0, true)
        
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
        let bezPts = spline.solveAndGetPoints(500, 1.0, true)
        
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

        let solver = Solver(ctrlPts, false, 1.0, false)
        solver.initialise ()
        let err = solver.computeErr ()
        Assert.That(err, Is.EqualTo(0.0).Within(1e-9))

    [<Test>]
    member this.CurvedLineNonZeroError() =
        let ctrlPts =
            [| dcp SplinePointType.Smooth 0. 0. None
               dcp SplinePointType.Smooth 1. 1. None
               dcp SplinePointType.Smooth 2. 0. None |]

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
            [| dcp SplinePointType.Smooth 0. 0. None
               { ty = SplinePointType.Smooth
                 x = Some 1.
                 y = None
                 th_in = None
                 th_out = None }
               dcp SplinePointType.Smooth 2. 0. None |]

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
            [| dcp SplinePointType.Smooth 0. 0. None
               { ty = SplinePointType.Smooth
                 x = None
                 y = Some 1.
                 th_in = None
                 th_out = None }
               dcp SplinePointType.Smooth 2. 0. None |]

        let solver = Solver(ctrlPts, false, 0.0, false)
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

        let solver = Solver(ctrlPts, false, 0.0, false)
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

        let solver = Solver(ctrlPts, false, 0.0, false)
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

        let solver = Solver(ctrlPts, false, 0.0, false)
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

        let solverFixed = Solver(ctrlPtsFixed, false, 1.0, false)
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

        let solverOpt = Solver(ctrlPtsOpt, false, 1.0, false)
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
        let svg, _, _ = spline.solveAndRenderSvg (max_iter, 1.0, false, false, false)
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
        let bezPts, _, _, _ = spline.solveAndRenderFull(max_iter, 1.0, false, false, false)
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
        let bezPts, _, _, _ = spline.solveAndRenderFull(max_iter, 1.0, false, false, false)
        printfn "bezPts: %A" (bezPts |> Array.map (fun bp -> sprintf "ld=%.2f rd=%.2f" bp.ld bp.rd))
        // First segment should be a line (horizontal)
        Assert.That(svg, Does.Match("L 250(\.0+)?,50(\.0+)?"), "First segment Corner→LineToCurve should be a line")
        // No arm should blow up
        let ok = bezPts |> Array.forall (fun bp -> abs bp.ld < 1e5 && abs bp.rd < 1e5)
        Assert.That(ok, Is.True, "Arm lengths should stay within bounds (no solver divergence)")

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
        let solver = Solver([| cp1; cp2; cp3 |], false, 1.0, false)
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

        let solver = Solver([| cp1; cp2; cp3; cp4 |], false, 1.0, false)
        solver.initialise ()

        // Solve
        solver.Solve(2000)
        let finalX = solver.points().[2].x

        printfn "Final X: %f (Center is 500)" finalX
        // Solver moves point RIGHT (542) to minimize curvature of the turn from the vertical HL segment.
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
        // Left side drop: 333. Right side drop: 500.
        // Requirement: fitted x < 500. (Shifted Left).

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

        let solver = Solver([| cp1; cp2; cp3; cp4; cp5 |], false, 1.0, false)
        solver.initialise ()
        solver.Solve(5000)

        let finalX = solver.points().[2].x

        Assert.That(
            finalX,
            Is.LessThan(500.0),
            "fitted x of t  he bottom point should be to the left of centre (stiffer right side pulls left)"
        )
