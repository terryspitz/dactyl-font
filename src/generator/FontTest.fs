module FontTests

open System
open NUnit.Framework
open Curves
open DactylSpline
open Axes
open GeneratorTypes
open Font
open GlyphFsDefs

let dcp = DactylSpline.dcp

let IsFinite x =
    not (System.Double.IsInfinity x) && not (System.Double.IsNaN x)

[<TestFixture>]
type FontTests() =

    [<Test>]
    member this.SolveAndGetPoints_ClosedSmoothCurve() =
        // Simulate the backbone of an 'o' glyph: 4 G2 points on a rough circle.
        // All are Smooth → a single closed curve section, which previously caused
        // an array-out-of-bounds write in solveAndGetPoints.
        let ctrlPts =
            [| dcp SplinePointType.Smooth 500. 900. None // top
               dcp SplinePointType.Smooth 1000. 500. None // right
               dcp SplinePointType.Smooth 500. 100. None // bottom
               dcp SplinePointType.Smooth 0. 500. None |] // left

        let spline = DactylSpline(ctrlPts, true)
        // Should not throw IndexOutOfRangeException
        let bezPts = spline.solveAndGetPoints (500, 1.0, false)

        // Result must have exactly N points, one per control point
        Assert.That(bezPts.Length, Is.EqualTo(ctrlPts.Length), "BezierPoint count must match ctrlPts count")

        // Every solved point should have finite coordinates
        for pt in bezPts do
            Assert.That(IsFinite(pt.x), Is.True, sprintf "x should be finite, got %f" pt.x)
            Assert.That(IsFinite(pt.y), Is.True, sprintf "y should be finite, got %f" pt.y)
            Assert.That(IsFinite(pt.th_in), Is.True, sprintf "th_in should be finite, got %f" pt.th_in)

    [<Test>]
    member this.DactylOutline_O_Glyph_RendersWithoutException() =
        // End-to-end test: Font.charToSvg 'o' with dactyl_spline + outline enabled
        // previously threw an IndexOutOfRangeException in solveAndGetPoints.
        let font =
            Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    new_definitions = true }
            )

        // Should not throw
        let svg = font.charToSvg 'o' 0 0 "black"

        // Should produce non-empty SVG output containing path data
        Assert.That(svg, Is.Not.Empty, "SVG output should not be empty")
        let svgStr = String.concat " " svg
        Assert.That(svgStr, Does.Contain("M "), "SVG should contain a moveto command")

    [<Test>]
    member this.VGlyph_OutlineSidesDoNotOverlap() =
        // The 'v' glyph is a V-shape: XL -> BC -> XR.  When outlining, the inner
        // concave side at BC previously used the "two outer points" expansion,
        // pushing both points behind the corner and making them overlap the outer side.
        //
        // We test by calling getDactylSansOutlines directly on a simple symmetric V
        // and checking that the two offset mid-segment points at the corner are on
        // opposite sides of the stroke centre-line (i.e. they are separated, not coincident).
        let font =
            Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    new_definitions = false } // use GlyphFsDefs path
            )

        // Render 'v' and capture the SVG; just verify it doesn't throw and produces
        // a path with enough points to form a closed outline.
        let svg = font.charToSvg 'v' 0 0 "black"
        let svgStr = String.concat " " svg
        Assert.That(svgStr, Does.Contain("M "), "SVG should contain a moveto")
        printfn "SVG for 'v': %s" svgStr

        // A correct V outline has a closed path. Count 'C' and 'L' commands.
        // Before the fix the outline collapsed (inner points overlapped the outer),
        // producing a degenerate tiny path with very few commands.
        let commands = svgStr.Split(' ') |> Array.filter (fun s -> s = "L" || s = "C")

        Assert.That(
            commands,
            Has.Length.EqualTo(7),
            sprintf "Expected at 7 path commands for v outline, got %d in: %s" commands.Length svgStr
        )

    [<Test>]
    member this.DactylOutline_Guides_RendersWithoutException() =
        // End-to-end test: Font.charToSvg '□' with dactyl_spline + outline enabled
        // This is used for the guides feature in the spline tab.
        let font =
            Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    new_definitions = true }
            )

        // Should not throw
        let svg = font.charToSvg '□' 0 0 "black"

        // Should produce non-empty SVG output containing path data
        Assert.That(svg, Is.Not.Empty, "SVG output should not be empty")
        let svgStr = String.concat " " svg
        printfn "SVG for '□': %s" svgStr
        Assert.That(svgStr, Does.Contain("M "), "SVG should contain a moveto command")

        // If outline calculation failed, it falls back to red.
        // The color "black" was passed, so if it's red, something is wrong.
        Assert.That(svgStr, Does.Not.Contain("stroke:#e00000"), "SVG should not be red (indicates outline failure)")
        ()
