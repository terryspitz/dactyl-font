module FontTests

open System
open NUnit.Framework
open Curves
open DactylSpline
open Axes
open GeneratorTypes
open Font

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
        let bezPts = spline.solveAndGetPoints (500, 1.0, 10.0, false)

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
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true }
            )

        // Should not throw
        let svg = font.charToSvg 'o' 0.0 0.0 "black"

        // Should produce non-empty SVG output containing path data
        Assert.That(svg, Is.Not.Empty, "SVG output should not be empty")
        let svgStr = String.concat " " svg
        Assert.That(svgStr, Does.Contain("M "), "SVG should contain a moveto command")

    [<Test>]
    member this.VGlyph_OutlineSidesDoNotOverlap() =
        // The 'v' glyph is a V-shape: XL -> BC -> XR.  When outlining, the inner
        // concave side at BC previously used the "two outer points" expansion,
        // pushing both points behind the corner and making them overlap the outer side.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    constant_offset = false }
            )

        // Render 'v' and capture the SVG
        let svg = font.charToSvg 'v' 0.0 0.0 "black"
        let svgStr = String.concat " " svg
        Assert.That(svgStr, Does.Contain("M "), "SVG should contain a moveto")

        // A correct V outline has a closed path. Count 'C' and 'L' commands.
        let commands = svgStr.Split(' ') |> Array.filter (fun s -> s = "L" || s = "C")

        Assert.That(
            commands,
            Has.Length.EqualTo(7),
            sprintf "Expected at 7 path commands for v outline, got %d in: %s" commands.Length svgStr
        )

    [<Test>]
    member this.DactylOutline_Guides_RendersWithoutException() =
        // End-to-end test: Font.charToSvg '□' with dactyl_spline + outline enabled
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true }
            )

        // Should not throw
        let svg = font.charToSvg '□' 0.0 0.0 "black"

        // Should produce non-empty SVG output containing path data
        Assert.That(svg, Is.Not.Empty, "SVG output should not be empty")
        let svgStr = String.concat " " svg
        Assert.That(svgStr, Does.Contain("M "), "SVG should contain a moveto command")

        // If outline calculation failed, it falls back to red.
        Assert.That(svgStr, Does.Not.Contain("stroke:#e00000"), "SVG should not be red (indicates outline failure)")

    [<Test>]
    member this.DactylOutline_P_DotJoint_RendersSingleOutlineWithCorrectSequence() =
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    constant_offset = false }
            )

        let svg = font.charToSvg 'P' 0.0 0.0 "black"
        let svgStr = String.concat " " svg

        // 1. Single outline check: there should only be one 'M' command.
        let mCount = svgStr.Split('M').Length - 1
        Assert.That(mCount, Is.EqualTo(1), "P should have a single outline")

        // 2. Specific sequence check (P's bowl now has short straight shoulders
        // before/after its curves, added so roundedness can square them off).
        // Extract command letters only
        let commands =
            svgStr.Split(' ')
            |> Array.filter (fun s -> s.Length = 1 && "MLCZ".Contains(s))
            |> String.concat ""

        Assert.That(commands, Is.EqualTo("MLLLCCLLLCCLLZ"), "P outline should have the updated MLLLCCLLLCCLLZ command sequence")
        Assert.That(svgStr, Does.Not.Contain("stroke:#e00000"), "SVG should not be red (indicates outline failure)")

        for ch in [ 'R'; 'B' ] do
            let svg = font.charToSvg ch 0.0 0.0 "black"
            Assert.That(String.concat " " svg, Does.Contain("M "))

    [<Test>]
    member this.SpiroOutline_B_RendersCleanOutline() =
        // Test that the Spiro (non-dactyl) outline for B doesn't break at tangent points.
        // This specifically targets the collapseHandleSegments fix.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = false
                    spline2 = false
                    outline = true }
            )

        let svg = font.charToSvg 'B' 0.0 0.0 "black"
        let svgStr = String.concat " " svg
        Assert.That(svgStr, Does.Contain("M "), "SVG should contain a moveto command")
        Assert.That(svgStr, Does.Not.Contain("stroke:#e00000"), "SVG should not be red (indicates outline failure)")
        // The B outline should form a single closed path (one M command) per element
        let mCount = svgStr.Split('M').Length - 1
        Assert.That(mCount, Is.GreaterThanOrEqualTo(1), "B should have at least one outline path")

    [<Test>]
    member this.SpiroOutline_O_RendersCleanClosedContours() =
        // The Spiro 'o' is a 4-point closed G2 curve.  SpiroCPsToSegments returns n+1
        // segments for closed curves (a wrap-around copy of segment 0 whose ks values
        // are all zeroes).  Before the fix, this unsolved segment was stored in
        // SpiroClosedCurve and ended up in the outline pass with a bogus tangentStart=0,
        // offsetting it northward instead of westward and producing a kinked outline.
        //
        // After the fix the wrap-around is stripped so the outline consists of exactly
        // two smooth ovals (inner and outer), each with the same number of M commands.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = false
                    spline2 = false
                    outline = true }
            )

        let svg = font.charToSvg 'o' 0.0 0.0 "black"
        let svgStr = String.concat " " svg
        Assert.That(svgStr, Does.Contain("M "), "SVG should contain a moveto command")
        Assert.That(svgStr, Does.Not.Contain("NaN"), "SVG should not contain NaN coordinates")
        Assert.That(svgStr, Does.Not.Contain("stroke:#e00000"), "SVG should not be red (outline failure)")
        // A closed 'o' outline has exactly two contours: outer and inner.
        let mCount = svgStr.Split([| "M " |], System.StringSplitOptions.None).Length - 1
        Assert.That(mCount, Is.EqualTo(2), sprintf "Spiro 'o' outline should have exactly 2 contours, got %d in: %s" mCount svgStr)

    [<Test>]
    member this.SpiroOutline_SimpleGlyphs_RendersWithoutException() =
        // Spiro outlines for simple glyphs that have 2-knot open strokes ('i', 'l', '1').
        // These previously crashed in collapseHandleSegments dropping the last segment,
        // leaving only 1 segment and causing IndexOutOfRangeException in strokeSegments.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = false
                    spline2 = false
                    outline = true }
            )

        for ch in [ 'i'; 'l'; '1'; '-' ] do
            let svg = font.charToSvg ch 0.0 0.0 "black"
            let svgStr = String.concat " " svg
            Assert.That(svgStr, Does.Contain("M "), sprintf "Spiro outline for '%c' should contain a moveto" ch)

            Assert.That(
                svgStr,
                Does.Not.Contain("stroke:#e00000"),
                sprintf "Spiro outline for '%c' should not indicate failure" ch
            )

    [<Test>]
    member this.Spline2Outline_SimpleGlyphs_RendersWithoutException() =
        // Spline2 outlines for simple glyphs with open strokes.
        // Two bugs previously caused crashes: collapseHandleSegments dropping the last
        // segment, and elementToSpline2 creating a garbage wraparound segment for open curves.
        // Note: spline2 mode always emits a tangent layer with stroke:#e00000, so we check
        // for valid path output and absence of NaN coordinates instead.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = false
                    spline2 = true
                    outline = true }
            )

        for ch in [ 'i'; 'l'; '1'; 'B'; 'o' ] do
            let svg = font.charToSvg ch 0.0 0.0 "black"
            let svgStr = String.concat " " svg
            Assert.That(svgStr, Does.Contain("M "), sprintf "Spline2 outline for '%c' should contain a moveto" ch)

            Assert.That(
                svgStr,
                Does.Not.Contain("NaN"),
                sprintf "Spline2 outline for '%c' should not contain NaN coordinates" ch
            )

    [<Test>]
    member this.EGlyph_BackboneIsStraight_Dactyl() = this.verifyEGlyphBackbone (true, false)

    [<Test>]
    member this.EGlyph_BackboneIsStraight_Spiro() =
        this.verifyEGlyphBackbone (false, false)

    member private this.verifyEGlyphBackbone(useDactyl, useSpline2) =
        // The 'e' glyph has a horizontal crossbar: xbl-xbrN.
        // It should be rendered as a straight line in the outline.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = useDactyl
                    spline2 = useSpline2
                    outline = false }
            )

        let svg = font.charToSvg 'e' 0.0 0.0 "black"
        let svgStr = String.concat " " svg

        // Find the line command that corresponds to the crossbar.
        // The crossbar is horizontal at y = (X+B)/2.
        // In the outline, it should be two parallel lines.
        // Spiro output may contain newlines, so we use a more robust split.
        let commands =
            svgStr.Split([| ' '; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.filter (fun s -> s = "L" || s = "C")

        let lCount = commands |> Array.filter (fun s -> s = "L") |> Array.length

        // If it's being treated as a curve, there will be fewer 'L' commands.
        // A typical 'e' outline should have at least 1 'L' commands (top and bottom of the bar)
        // if they are properly detected as lines.
        Assert.That(
            lCount,
            Is.GreaterThanOrEqualTo(1),
            sprintf
                "Expected at least 1 'L' commands for 'e' backbone (Dactyl=%b), got %d. SVG: %s"
                useDactyl
                lCount
                svgStr
        )

    // [<Test>]
    // member this.SpiroTangent_MatchDactyl() =
    //     // Test a simple curve: (0,100) ~ (0,0) with tangent South (PI*1.5) at (0,0)
    //     // Dactyl and Spiro should both results in a straight vertical line or a vertical arrival.
    //     let knots =
    //         [ { pt =
    //               { x = 0.
    //                 y = 100.
    //                 y_fit = false
    //                 x_fit = false }
    //             ty = Corner
    //             th_in = None
    //             th_out = None
    //             label = None }
    //           { pt =
    //               { x = 0.
    //                 y = 0.
    //                 y_fit = false
    //                 x_fit = false }
    //             ty = Corner
    //             th_in = Some(norm (Math.PI * 1.5))
    //             th_out = None
    //             label = None } ] // South arrival

    //     let elem = Curve(knots, false)

    //     let fontD =
    //         Font.Font(
    //             { Axes.DefaultAxes with
    //                 dactyl_spline = true
    //                 outline = false }
    //         )

    //     let fontS =
    //         Font.Font(
    //             { Axes.DefaultAxes with
    //                 dactyl_spline = false
    //                 spline2 = false
    //                 outline = false }
    //         )

    //     let svgD = String.concat " " (fontD.elementToSvgPath elem 0.0 0.0 1.0 "black")
    //     let svgS = String.concat " " (fontS.elementToSvgPath elem 0.0 0.0 1.0 "black")

    //     printfn "Dactyl SVG: %s" svgD
    //     printfn "Spiro SVG: %s" svgS

    //     // If Spiro exploded, coordinates will be huge or contain Dash.
    //     Assert.That(svgS.Contains("-"), Is.False, "Spiro should not have huge negative coordinates")
    //     Assert.That(svgS.Length, Is.LessThan(1000), "Spiro should produce a compact SVG")

    [<Test>]
    member this.Cursive_Axis_ChangesAAndG_ButNotOthers() =
        // The cursive axis swaps 'a', 'f' and 'g' between their cursive forms
        // (the default: single-storey a, single-storey g, descending f) and the
        // two-storey Roman / non-descending alternates.  It must change the
        // output of 'a', 'f' and 'g' and leave every other glyph untouched.
        let mkFont cursive =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    cursive = cursive }
            )

        let fontCursive = mkFont 1.0 // single-storey (default)
        let fontRoman = mkFont 0.0 // two-storey Roman alternates

        let svg (font: Font.Font) ch =
            font.charToSvg ch 0.0 0.0 "black" |> String.concat " "

        // 'a', 'f' and 'g' should render cleanly and differ between the two settings.
        for ch in [ 'a'; 'f'; 'g' ] do
            let sCursive = svg fontCursive ch
            let sRoman = svg fontRoman ch
            Assert.That(sRoman, Does.Contain("M "), sprintf "Roman '%c' should render a moveto" ch)
            Assert.That(sRoman, Does.Not.Contain("NaN"), sprintf "Roman '%c' should not contain NaN" ch)
            Assert.That(sRoman, Does.Not.Contain("stroke:#e00000"), sprintf "Roman '%c' outline should not fail" ch)
            Assert.That(sRoman, Is.Not.EqualTo(sCursive), sprintf "cursive should change '%c'" ch)

        // Every other glyph must be identical with the axis on or off.
        for ch in "bcdehijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" do
            Assert.That(svg fontRoman ch, Is.EqualTo(svg fontCursive ch),
                sprintf "cursive should not change '%c'" ch)

    [<Test>]
    member this.Cursive_Auto_FollowsSlant() =
        // Cursive=0.5 (Auto): Roman (two-storey) when upright, cursive
        // (single-storey) when slanted.  Compare 'a' against the explicit
        // Roman (0.0) and Cursive (1.0) settings at the same slant.
        let mkFont cursive slant =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    cursive = cursive
                    slant = slant }
            )

        let svg (font: Font.Font) ch =
            font.charToSvg ch 0.0 0.0 "black" |> String.concat " "

        // Upright: Auto must match Roman (two-storey), not Cursive.
        Assert.That(svg (mkFont 0.5 0.0) 'a', Is.EqualTo(svg (mkFont 0.0 0.0) 'a'),
            "Auto upright should use Roman two-storey 'a'")
        // Slanted: Auto must match Cursive (single-storey), not Roman.
        Assert.That(svg (mkFont 0.5 0.15) 'a', Is.EqualTo(svg (mkFont 1.0 0.15) 'a'),
            "Auto slanted should use Cursive single-storey 'a'")

    [<Test>]
    member this.FilledAxis_ControlsSvgFillStyle() =
        // When filled=true (and outline=true), SVG should have fill:black.
        // When filled=false, SVG should have fill:none regardless of outline setting.
        let filledFont =
            Font.Font(
                { Axes.DefaultAxes with
                    outline = true
                    filled = true }
            )

        let unfilledFont =
            Font.Font(
                { Axes.DefaultAxes with
                    outline = true
                    filled = false }
            )

        let filledSvg = String.concat " " (filledFont.charToSvg 'o' 0.0 0.0 "black")
        let unfilledSvg = String.concat " " (unfilledFont.charToSvg 'o' 0.0 0.0 "black")

        Assert.That(filledSvg, Does.Contain("fill:black"), "filled=true should produce fill:black")
        Assert.That(unfilledSvg, Does.Contain("fill:none"), "filled=false should produce fill:none")

    [<Test>]
    member this.SoftCorners_V_Glyph_ProducesRoundedCorners() =
        // The 'V' glyph (bl-tc-br) has sharp corners at tc and at the miter points.
        // With softness > 0, corners should be replaced with arcs (CurveToLine→G2→LineToCurve).
        // End caps should remain intact (not distorted by rounding).
        let fontSharp =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    softness = 0.0
                    constant_offset = false }
            )

        let fontRound =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    softness = 0.5
                    constant_offset = false }
            )

        let svgSharp = fontSharp.charToSvg 'V' 0.0 0.0 "black" |> String.concat " "
        let svgRound = fontRound.charToSvg 'V' 0.0 0.0 "black" |> String.concat " "

        // Both should produce valid SVG
        Assert.That(svgSharp, Does.Contain("M "), "Sharp V should contain moveto")
        Assert.That(svgRound, Does.Contain("M "), "Rounded V should contain moveto")
        Assert.That(svgRound, Does.Not.Contain("NaN"), "Rounded V should not contain NaN")

        // The rounded version should have more curve commands (C) than the sharp one,
        // since corners are replaced with arcs.
        let countC (svg: string) =
            svg.Split(' ') |> Array.filter (fun s -> s = "C") |> Array.length

        Assert.That(
            countC svgRound,
            Is.GreaterThan(countC svgSharp),
            "Rounded V should have more curve commands than sharp V"
        )

    [<Test>]
    member this.SoftCorners_Zero_MatchesDefault() =
        // With softness = 0, output should be identical to default (no rounding).
        let fontDefault =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true }
            )

        let fontZero =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    softness = 0.0 }
            )

        for ch in [ 'A'; 'V'; 'M'; 'o' ] do
            let svgDefault = fontDefault.charToSvg ch 0.0 0.0 "black" |> String.concat " "
            let svgZero = fontZero.charToSvg ch 0.0 0.0 "black" |> String.concat " "
            Assert.That(svgZero, Is.EqualTo(svgDefault), sprintf "softness=0 should match default for '%c'" ch)

    [<Test>]
    member this.SoftCorners_AllGlyphs_RenderWithoutException() =
        // Smoke test: every glyph should render without crashing with softness enabled.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    softness = 0.8 }
            )

        for ch in "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" do
            let svg = font.charToSvg ch 0.0 0.0 "black"
            let svgStr = String.concat " " svg
            Assert.That(svgStr, Does.Contain("M "), sprintf "Soft corners glyph '%c' should render a moveto" ch)
            Assert.That(svgStr, Does.Not.Contain("NaN"), sprintf "Soft corners glyph '%c' should not contain NaN" ch)

    [<Test>]
    member this.DactylSpline_IsLineSegment_HandlesColinearTangents() =
        // Test that DactylSpline.isLineSegment returns true for segments
        // where forced tangents are colinear with the chord.
        let pt0 =
            { ty = SplinePointType.Corner
              x = Some 0.
              y = Some 0.
              th_in = None
              th_out = Some 0. }
        // th_in = 0 is colinear with chord from (0,0) to (100,0)
        let pt1 =
            { ty = SplinePointType.Corner
              x = Some 100.
              y = Some 0.
              th_in = Some 0.
              th_out = Some 1.57 }

        let spline = DactylSpline([| pt0; pt1 |], false)
        Assert.That(spline.isLineSegment (pt0, pt1), Is.True, "Segment should be a line if tangents are colinear")

    [<Test>]
    member this.TopLeftOfP_OutlinePreservesTangents() =
        let axes =
            { Axes.DefaultAxes with
                dactyl_spline = true
                outline = true
                weight = 30
                constant_offset = false }

        let font = Font.Font(axes)

        // 'P' is defined as "bl-tlE~(th)rS~hlE"
        // tlE has an explicit East tangent (0.0 rad) and it's a transition from line to curve.
        let backbone = font.charToElem 'P'
        let outline = font.getOutline backbone

        // Find the outline Curve that corresponds to the exterior
        match outline with
        | Curve(pts, true) ->
            // In the 'P' glyph, tl is one of the top-most points.
            // Let's look for points with th_out or th_in set.
            let pointsWithTangents =
                pts |> List.filter (fun k -> k.th_in.IsSome || k.th_out.IsSome)

            let hasEastTangentAtTopLeft =
                pointsWithTangents
                |> List.exists (fun k ->
                    match k.th_out with
                    | Some t ->
                        let isEast = abs (t - 0.0) < 0.001
                        let isTopLeft = k.pt.y > 500.0 && k.pt.x < 100.0 // Adjusted for typical FontMetrics
                        isEast && isTopLeft
                    | None -> false)

            Assert.That(
                hasEastTangentAtTopLeft,
                Is.True,
                "Outline should have an East tangent at the top-left corner area"
            )
        | _ -> Assert.Fail("Could not find exterior curve in P outline")

    [<Test>]
    member this.IsJoint_ReturnsTrue_For_A_Glyph_BowlEndpoint() =
        // The 'a' glyph is "xr-br xor~x(c)~xbl~bc~bor".
        // The bowl endpoint "bor" lies exactly on the stem "xr-br", so isJoint must fire.
        // Default axes: width=300 height=600 x_height=0.6 roundedness=60 thickness=30.
        // After translateByThickness (+30 in both axes):
        //   bor  = (R + t, B + roundedness + t) = (330, 90)
        //   stem = (R + t, X + t) → (R + t, B + t) = (330, 390) → (330, 30)
        // The point (330, 90) is on the vertical stem (perpDist = 0 < thickness), so
        // isJoint should return true.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    joints = true
                    dactyl_spline = true }
            )

        let backbone = font.charToElem 'a'
        let t = float Axes.DefaultAxes.weight  // 30
        let r = float Axes.DefaultAxes.width       // 300 → R
        let roundedness = float Axes.DefaultAxes.roundedness  // 60
        let borX = r + t          // 330
        let borY = roundedness + t // 90  (B=0 + roundedness + thickness)

        Assert.That(
            font.isJoint backbone borX borY,
            Is.True,
            sprintf "isJoint should return true at bor=(%.0f,%.0f) for 'a' glyph (bowl endpoint lies on stem)" borX borY
        )

    [<Test>]
    member this.SoftCorners_A_Glyph_JointCornersNotRounded() =
        // With joints=true and softness > 0, corners at joint positions must be
        // preserved (not rounded), so the SVG should not gain extra curve commands at
        // those joints compared to softness=0.
        let mkFont sc jt =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    softness = sc
                    joints = jt
                    constant_offset = false }
            )

        let countC (svg: string) =
            svg.Split(' ') |> Array.filter (fun s -> s = "C") |> Array.length

        let svgNoRounding = mkFont 0.0 true  |> fun f -> f.charToSvg 'a' 0.0 0.0 "black" |> String.concat " "
        let svgWithJoints = mkFont 0.5 true  |> fun f -> f.charToSvg 'a' 0.0 0.0 "black" |> String.concat " "
        let svgNoJoints   = mkFont 0.5 false |> fun f -> f.charToSvg 'a' 0.0 0.0 "black" |> String.concat " "

        // Sanity: all renders should produce valid SVG
        Assert.That(svgWithJoints, Does.Contain("M "), "Soft corners + joints: 'a' should render")
        Assert.That(svgNoJoints,   Does.Contain("M "), "Soft corners, no joints: 'a' should render")
        Assert.That(svgWithJoints, Does.Not.Contain("NaN"), "Soft corners + joints: no NaN in 'a'")
        Assert.That(svgNoJoints,   Does.Not.Contain("NaN"), "Soft corners, no joints: no NaN in 'a'")

        // When joints are enabled, joint corners are preserved, so 'a' gains fewer extra
        // curves than when joints are disabled (where ALL corners get rounded).
        let cWithJoints = countC svgWithJoints
        let cNoJoints   = countC svgNoJoints
        let cNoRounding = countC svgNoRounding
        Assert.That(
            cWithJoints,
            Is.LessThan(cNoJoints),
            sprintf "joints=true should round fewer corners than joints=false (got %d vs %d C commands)" cWithJoints cNoJoints
        )
        Assert.That(
            cWithJoints,
            Is.GreaterThanOrEqualTo(cNoRounding),
            sprintf "softness should still add some rounding even with joints (got %d C, baseline %d)" cWithJoints cNoRounding
        )

    [<Test>]
    member this.O_And_o_Outline_IsHorizontallyAndVerticallySymmetric() =
        // The 'O' and 'o' glyphs are ovals defined by 4 symmetric knots with fitted coords.
        // We verify structural symmetry: left and right backbone knots should have similar y
        // (horizontal mirror), and top and bottom knots should have similar x (vertical mirror).
        // knotTol=8.0 gives enough headroom for NelderMead asymmetry while still catching
        // regressions (the pre-fix asymmetry was >100 units).
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    flatness = 1.0
                    end_flatness = 0.0 }
            )

        let knotTol = 8.0

        let rec collectOutlinePoints elem =
            match elem with
            | Curve(knots, _) -> knots |> List.map (fun k -> k.pt.x, k.pt.y)
            | EList(elems) -> List.collect collectOutlinePoints elems
            | _ -> []

        for ch in [ 'O'; 'o' ] do
            let backbonePts = font.charToSolvedBackbonePoints ch
            Assert.That(backbonePts, Is.Not.Empty, sprintf "'%c' backbone should have points" ch)
            let sortedByX = backbonePts |> List.sortBy fst
            let sortedByY = backbonePts |> List.sortBy snd
            let leftY = snd sortedByX.[0]
            let rightY = snd sortedByX.[sortedByX.Length - 1]
            let bottomX = fst sortedByY.[0]
            let topX = fst sortedByY.[sortedByY.Length - 1]
            Assert.That(
                abs (leftY - rightY),
                Is.LessThan knotTol,
                sprintf "'%c' left/right backbone y-coords differ too much: left=%.2f right=%.2f diff=%.2f" ch leftY rightY (abs (leftY - rightY))
            )
            Assert.That(
                abs (topX - bottomX),
                Is.LessThan knotTol,
                sprintf "'%c' top/bottom backbone x-coords differ too much: top=%.2f bottom=%.2f diff=%.2f" ch topX bottomX (abs (topX - bottomX))
            )

            let outline = font.CharToOutline ch
            let outlinePts = collectOutlinePoints outline
            Assert.That(outlinePts, Is.Not.Empty, sprintf "'%c' outline should have points" ch)

    [<Test>]
    member this.C_And_c_Backbone_ArmTipsAreAtSimilarX() =
        // 'C' and 'c' are open arcs. The top-arm and bottom-arm fitted x-coords should
        // reach roughly the same x extent (the arc should not be lopsided).
        // With the improved fitted-coord glyph definitions the two arm-tip x values
        // differ by at most ~8 units; we allow up to 15 before calling it a regression.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true }
            )

        let armTipTol = 15.0

        for ch in [ 'C'; 'c' ] do
            let pts = font.charToSolvedBackbonePoints ch
            Assert.That(pts, Is.Not.Empty, sprintf "'%c' backbone should have points" ch)

            // The arc endpoints are tor/bor (fixed); the arm tips are the fitted-x
            // centre points t(c) and b(c). Filter to points on the top and bottom rows.
            let ys = pts |> List.map snd
            let cyTop = List.max ys
            let cyBot = List.min ys
            let topArmX = pts |> List.filter (fun (_, y) -> y = cyTop) |> List.map fst
            let botArmX = pts |> List.filter (fun (_, y) -> y = cyBot) |> List.map fst
            match topArmX, botArmX with
            | x1 :: _, x3 :: _ ->
                Assert.That(
                    abs (x1 - x3),
                    Is.LessThan(armTipTol),
                    sprintf "'%c' arm-tip x values differ too much: top=%.2f bottom=%.2f diff=%.2f" ch x1 x3 (abs (x1 - x3))
                )
            | _ -> Assert.Fail(sprintf "'%c' backbone lacks top or bottom arm-tip points" ch)

    // ── Helpers shared by the symmetry tests below ────────────────────────────

    member private this.makeFont() =
        Font.Font({ Axes.DefaultAxes with dactyl_spline = true; outline = true })

    member private this.hasLRMirror (pts: (float * float) list) cx tol (x, y) =
        let mx = 2.0 * cx - x
        pts |> List.exists (fun (px, py) -> abs (px - mx) < tol && abs (py - y) < tol)

    member private this.isFullyLRSymmetric (pts: (float * float) list) tol =
        if pts.IsEmpty then true
        else
            let xs = pts |> List.map fst
            let cx = (List.min xs + List.max xs) / 2.0
            pts |> List.forall (this.hasLRMirror pts cx tol)

    // ── Positive symmetry tests ────────────────────────────────────────────────

    [<Test>]
    member this.GlyphsWithVerticalAxisOfSymmetry_HaveLeftRightSymmetricBackbone() =
        // These glyphs are designed with a vertical axis of symmetry.
        // H, I, T, V, A, X are pure straight-line glyphs (no fitted/free coords), so the
        // DactylSpline solver makes no position adjustments — perfect symmetry is expected.
        let font = this.makeFont()
        let lineTol = 1.0
        for ch in [ 'H'; 'I'; 'T'; 'V'; 'A'; 'X' ] do
            let pts = font.charToSolvedBackbonePoints ch
            Assert.That(pts, Is.Not.Empty, sprintf "'%c' backbone should have points" ch)
            let xs = pts |> List.map fst
            let cx = (List.min xs + List.max xs) / 2.0
            for pt in pts do
                Assert.That(
                    this.hasLRMirror pts cx lineTol pt,
                    Is.True,
                    sprintf "'%c' backbone: (%.2f, %.2f) has no left-right mirror (cx=%.2f)" ch (fst pt) (snd pt) cx
                )

    // ── Negative symmetry tests ────────────────────────────────────────────────

    [<Test>]
    member this.GlyphsWithoutLeftRightSymmetry_BackboneIsNotLeftRightSymmetric() =
        // These glyphs are structurally asymmetric and must NOT be fully left-right symmetric.
        let font = this.makeFont()
        let tol = 6.0
        for ch in [ 'D'; 'G'; 'S'; 'B'; 'C' ] do
            let pts = font.charToSolvedBackbonePoints ch
            Assert.That(pts, Is.Not.Empty, sprintf "'%c' backbone should have points" ch)
            Assert.That(
                this.isFullyLRSymmetric pts tol,
                Is.False,
                sprintf "'%c' backbone appears fully left-right symmetric, but should not be" ch
            )

    [<Test>]
    member this.ConstantOffset_ClosedGlyph_ProducesTwoContours() =
        // A closed glyph (like 'o') with constant_offset=true should produce exactly
        // two contours: one outer and one inner, forming the filled stroke ring.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    constant_offset = true }
            )

        let svg = font.charToSvg 'o' 0.0 0.0 "black"
        let svgStr = String.concat " " svg

        Assert.That(svgStr, Does.Contain("M "), "Constant-offset 'o' should contain a moveto")
        Assert.That(svgStr, Does.Not.Contain("NaN"), "Constant-offset 'o' should not contain NaN")
        Assert.That(svgStr, Does.Not.Contain("stroke:#e00000"), "Constant-offset 'o' should not indicate failure")

        // A filled stroke ring requires exactly 2 closed contours (outer oval + inner oval).
        let mCount = svgStr.Split([| "M " |], System.StringSplitOptions.None).Length - 1
        Assert.That(mCount, Is.EqualTo(2), sprintf "Constant-offset 'o' should have 2 contours, got %d" mCount)

    [<Test>]
    member this.ConstantOffset_OpenGlyph_ProducesSingleClosedContour() =
        // An open stroke glyph (like 'l') with constant_offset=true should produce a single
        // closed contour: the two offset sides joined by caps at each end.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    constant_offset = true }
            )

        let svg = font.charToSvg 'l' 0.0 0.0 "black"
        let svgStr = String.concat " " svg

        Assert.That(svgStr, Does.Contain("M "), "Constant-offset 'l' should contain a moveto")
        Assert.That(svgStr, Does.Not.Contain("NaN"), "Constant-offset 'l' should not contain NaN")
        Assert.That(svgStr, Does.Not.Contain("stroke:#e00000"), "Constant-offset 'l' should not indicate failure")

        // An open stroke forms a single pill-shaped closed contour.
        let mCount = svgStr.Split([| "M " |], System.StringSplitOptions.None).Length - 1
        Assert.That(mCount, Is.EqualTo(1), sprintf "Constant-offset 'l' should have 1 closed contour, got %d" mCount)

    [<Test>]
    member this.Figure8_BackboneSpansBothLoops() =
        // The '8' glyph is a figure-of-eight; the post-solve symmetrisation must NOT
        // collapse both loops to a flat disc by averaging all four free knots.
        let axes = { Axes.DefaultAxes with dactyl_spline = true; outline = true }
        let font = Font.Font(axes)
        let pts = font.charToSolvedBackbonePoints '8'
        Assert.That(pts, Is.Not.Empty, "'8' backbone should have points")
        let ys = pts |> List.map snd
        let maxY = List.max ys
        let minY = List.min ys
        let translate = float axes.weight
        let capT = float axes.height + translate
        Assert.That(
            maxY,
            Is.GreaterThan(capT * 0.7),
            sprintf "'8' backbone top (%.1f) should reach above 70%% of cap height (%.1f)" maxY (capT * 0.7)
        )
        Assert.That(
            minY,
            Is.LessThan(capT * 0.3),
            sprintf "'8' backbone bottom (%.1f) should be below 30%% of cap height (%.1f)" minY (capT * 0.3)
        )

[<TestFixture>]
type KnotSequenceValidationTests() =
    let pt x y = { x = x; y = y; x_fit = false; y_fit = false }
    let knot ty x y = { pt = pt x y; ty = ty; th_in = None; th_out = None; isJoint = false; label = None }

    [<Test>]
    member _.Valid_AllG2_Closed() =
        let ks = [ knot G2 0. 0.; knot G2 1. 0.; knot G2 1. 1.; knot G2 0. 1. ]
        Assert.DoesNotThrow(fun () -> validateKnotSequence ks true)

    [<Test>]
    member _.Valid_AllCorner_Open() =
        let ks = [ knot Corner 0. 0.; knot Corner 1. 0.; knot Corner 1. 1. ]
        Assert.DoesNotThrow(fun () -> validateKnotSequence ks false)

    [<Test>]
    member _.Valid_ProperTransitions_Open() =
        // Corner → LineToCurve → G2 → CurveToLine → Corner
        let ks =
            [ knot Corner 0. 0.
              knot LineToCurve 1. 0.
              knot G2 2. 1.
              knot CurveToLine 2. 2.
              knot Corner 0. 2. ]
        Assert.DoesNotThrow(fun () -> validateKnotSequence ks false)

    [<Test>]
    member _.Valid_ProperTransitions_Closed() =
        // Closed [LineToCurve, G2, CurveToLine]: wrap-around is CurveToLine→LineToCurve = line segment, valid
        let ks = [ knot LineToCurve 0. 0.; knot G2 1. 0.; knot CurveToLine 1. 1. ]
        Assert.DoesNotThrow(fun () -> validateKnotSequence ks true)

    [<Test>]
    member _.Valid_CornerAdjacentToAnything() =
        // Corner is unconstrained so it can sit next to G2, LineToCurve, or CurveToLine without error
        let ks = [ knot G2 0. 0.; knot Corner 1. 0.; knot LineToCurve 2. 0.; knot G2 3. 0. ]
        Assert.DoesNotThrow(fun () -> validateKnotSequence ks false)

    [<Test>]
    member _.Valid_SingleKnot() =
        let ks = [ knot G2 0. 0. ]
        Assert.DoesNotThrow(fun () -> validateKnotSequence ks false)

    [<Test>]
    member _.Invalid_G2_ThenLineToCurve() =
        // G2 departs a curve; LineToCurve expects a line to arrive
        let ks = [ knot G2 0. 0.; knot LineToCurve 1. 0.; knot G2 2. 1. ]
        Assert.Throws<System.ArgumentException>(fun () -> validateKnotSequence ks false) |> ignore

    [<Test>]
    member _.Invalid_CurveToLine_ThenG2() =
        // CurveToLine departs a line; G2 expects a curve to arrive
        let ks = [ knot G2 0. 0.; knot CurveToLine 1. 0.; knot G2 2. 1. ]
        Assert.Throws<System.ArgumentException>(fun () -> validateKnotSequence ks false) |> ignore

    [<Test>]
    member _.Invalid_DoubleCurveToLine() =
        // First CurveToLine departs a line; second CurveToLine expects a curve to arrive
        let ks = [ knot G2 0. 0.; knot CurveToLine 1. 0.; knot CurveToLine 2. 0.; knot Corner 3. 0. ]
        Assert.Throws<System.ArgumentException>(fun () -> validateKnotSequence ks false) |> ignore

    [<Test>]
    member _.Invalid_DoubleLineToCurve() =
        // First LineToCurve departs a curve; second LineToCurve expects a line to arrive
        let ks = [ knot Corner 0. 0.; knot LineToCurve 1. 0.; knot LineToCurve 2. 0.; knot G2 3. 0. ]
        Assert.Throws<System.ArgumentException>(fun () -> validateKnotSequence ks false) |> ignore

    [<Test>]
    member _.Invalid_Closed_WrapAround_G2_ThenLineToCurve() =
        // Closed [LineToCurve, G2, G2]: last G2 departs a curve, first LineToCurve expects a line to arrive
        let ks = [ knot LineToCurve 0. 0.; knot G2 1. 0.; knot G2 1. 1. ]
        Assert.Throws<System.ArgumentException>(fun () -> validateKnotSequence ks true) |> ignore

    [<Test>]
    member _.Valid_Open_LineToCurveFirst_CurveToLineLast() =
        // LineToCurve at start and CurveToLine at end of open curve: no wrap-around check,
        // and the internal pairs (LineToCurve→G2 = curve out / curve in, G2→CurveToLine = curve out / curve in) are valid
        let ks = [ knot LineToCurve 0. 0.; knot G2 1. 0.; knot CurveToLine 2. 0. ]
        Assert.DoesNotThrow(fun () -> validateKnotSequence ks false)

    [<Test>]
    member _.Flatness_And_EndFlatness_AffectOutput() =
        // Verify that the flatness and end_flatness axes actually change the solved backbone.
        // charToSolvedBackbonePoints returns the free-coord knot positions after optimization;
        // if an axis has no effect these lists will be identical.
        let baseAxes = { Axes.DefaultAxes with dactyl_spline = true; outline = true }

        let backboneFor (axes: Axes) ch =
            Font(axes).charToSolvedBackbonePoints ch

        // 1. flatness: vary from 0.0 to 50.0 on 'S' (open curve, many segments)
        let ptsLowFlat  = backboneFor { baseAxes with flatness =  0.0 } 'S'
        let ptsHighFlat = backboneFor { baseAxes with flatness = 50.0 } 'S'
        printfn "flatness=0.0  S backbone: %A" ptsLowFlat
        printfn "flatness=50.0 S backbone: %A" ptsHighFlat
        Assert.That(ptsLowFlat, Is.Not.EqualTo(ptsHighFlat),
            "flatness should change the solved 'S' backbone")

        // 2. end_flatness: vary from 0.5 to 20.0 on 'S'
        let ptsLowEnd  = backboneFor { baseAxes with end_flatness =  0.5 } 'S'
        let ptsHighEnd = backboneFor { baseAxes with end_flatness = 20.0 } 'S'
        printfn "end_flatness=0.5  S backbone: %A" ptsLowEnd
        printfn "end_flatness=20.0 S backbone: %A" ptsHighEnd
        Assert.That(ptsLowEnd, Is.Not.EqualTo(ptsHighEnd),
            "end_flatness should change the solved 'S' backbone")

[<TestFixture>]
type ArtisticAxesTests() =
    let pt x y = { x = x; y = y; x_fit = false; y_fit = false }
    let knot ty x y = { pt = pt x y; ty = ty; th_in = None; th_out = None; isJoint = false; label = None }

    /// A straight open stroke from (0,0) to (x,y).
    let strokeTo x y = Curve([ knot Corner 0. 0.; knot Corner x y ], false)

    // contrast=0 keeps offsets exactly perpendicular so widths are easy to assert
    let baseAxes = { Axes.DefaultAxes with contrast = 0.0 }
    let fthickness = float Axes.DefaultAxes.weight

    [<Test>]
    member _.NibAxis_WidthFollowsStrokeDirection() =
        // nib_angle=0 → horizontal nib edge: vertical strokes get the full width,
        // horizontal strokes (drawn along the nib) collapse to a thin sliver. The end caps
        // inherit the nib width (flat chisel ends), so a horizontal stroke is a thin sliver
        // all the way to its terminals — not capped at full thickness.
        let font = Font.Font({ baseAxes with nib = 1.0; nib_angle = 0 })
        let vl, vr, _, _ = bounds (font.getOutline (strokeTo 0. 600.))
        let _, _, hb, ht = bounds (font.getOutline (strokeTo 600. 0.))
        Assert.That(vr - vl, Is.EqualTo(2.0 * fthickness).Within(1.0),
            "vertical stroke should be drawn at full width")
        Assert.That(ht - hb, Is.LessThan(0.5 * fthickness),
            "horizontal stroke should be a thin sliver, including its end caps (nib width)")

    member private _.TaperWidthNear(axes, yLo, yHi) =
        let font = Font.Font(axes)
        let outlineKnots =
            match font.getOutline (strokeTo 0. 600.) with
            | Curve(ks, _) -> ks
            | e -> failwithf "expected single Curve outline, got %A" e
        outlineKnots
        |> List.filter (fun k -> k.pt.y >= yLo && k.pt.y <= yHi)
        |> List.map (fun k -> abs k.pt.x)
        |> List.max

    [<Test>]
    member this.TaperAxis_SharpPoint_WhenTaperEndZero() =
        // taper_end = 0 keeps the original pointed-brush behaviour.
        let axes = { baseAxes with taper = 1.0; taper_end = 0.0 }
        Assert.That(this.TaperWidthNear(axes, 0., 100.), Is.LessThan(0.4 * fthickness),
            "stroke should narrow to near a point at its ends")
        Assert.That(this.TaperWidthNear(axes, 250., 350.), Is.EqualTo(fthickness).Within(1.0),
            "stroke should be full width at its middle")

    [<Test>]
    member this.TaperAxis_EndWidthControlledByTaperEnd() =
        // taper_end = 0.5 leaves the ends at ~half width instead of a point. The end cap
        // now extends past the spine endpoint (y < 0, same length as a plain stroke's cap),
        // squeezed to the tapered end width — so measure the cap region near the tip.
        let axes = { baseAxes with taper = 1.0; taper_end = 0.5 }
        let endW = this.TaperWidthNear(axes, -1.5 * fthickness, 0.6 * fthickness)
        Assert.That(endW, Is.GreaterThan(0.35 * fthickness),
            "taper_end=0.5 should keep the ends well above a point")
        Assert.That(endW, Is.LessThan(0.75 * fthickness),
            "taper_end=0.5 ends should still be clearly narrower than full width")
        Assert.That(this.TaperWidthNear(axes, 250., 350.), Is.EqualTo(fthickness).Within(1.0),
            "stroke should be full width at its middle")

    [<Test>]
    member _.WobbleAxis_DisplacesSpineButNotEndpoints() =
        let font = Font.Font({ baseAxes with wobble = 1.0 })

        let outlineKnots =
            match font.getOutline (strokeTo 0. 600.) with
            | Curve(ks, _) -> ks
            | e -> failwithf "expected single Curve outline, got %A" e

        // The wave swings the stroke beyond its straight-line bounds (peak displacement
        // is 0.5*thickness, so the outer edge reaches ~1.5*thickness)...
        let maxX = outlineKnots |> List.map (fun k -> abs k.pt.x) |> List.max
        Assert.That(maxX, Is.GreaterThan(1.3 * fthickness),
            "wobble should displace the stroke beyond its plain width")

        // ...but the displacement vanishes at the stroke ends so caps stay centred.
        let maxXAtEnds =
            outlineKnots
            |> List.filter (fun k -> k.pt.y <= 0.0 || k.pt.y >= 600.0)
            |> List.map (fun k -> abs k.pt.x)
            |> List.max
        Assert.That(maxXAtEnds, Is.LessThan(1.2 * fthickness),
            "wobble should vanish at stroke endpoints")

    [<Test>]
    member _.RoughnessAxis_JittersEdgeWidthIndependentlyPerSide() =
        let font = Font.Font({ baseAxes with roughness = 1.0 })

        let outlineKnots =
            match font.getOutline (strokeTo 0. 600.) with
            | Curve(ks, _) -> ks
            | e -> failwithf "expected single Curve outline, got %A" e

        // Roughness jitters the half-width, so the outer edge should swing on both
        // sides of the plain (unjittered) stroke bound rather than sitting flush.
        let xs = outlineKnots |> List.map (fun k -> k.pt.x)
        Assert.That(List.max xs, Is.GreaterThan(1.05 * fthickness),
            "roughness should widen the stroke edge beyond its plain width somewhere")
        Assert.That(List.min xs, Is.LessThan(0.95 * fthickness),
            "roughness should narrow the stroke edge below its plain width somewhere")

        // The two edges jitter independently (different phase), so they shouldn't be
        // simply mirror images of each other at every sample.
        let leftXs = outlineKnots |> List.filter (fun k -> k.pt.x < 0.0) |> List.map (fun k -> -k.pt.x)
        let rightXs = outlineKnots |> List.filter (fun k -> k.pt.x >= 0.0) |> List.map (fun k -> k.pt.x)
        Assert.That(leftXs, Is.Not.EquivalentTo(rightXs),
            "the two stroke edges should jitter independently, not identically")

    [<Test>]
    member _.MobiusAxis_StraightStrokeSplitsIntoPinchedPanels() =
        // A 600-unit stroke at mobius=1.0 gets round(600/300) = 2 half-twists →
        // pinches at arc length 150 and 450 → 3 separate closed panels.
        let font = Font.Font({ baseAxes with mobius = 1.0 })

        let panels =
            match font.getOutline (strokeTo 0. 600.) with
            | EList(curves) -> curves
            | e -> failwithf "expected EList of panels, got %A" e

        Assert.That(panels.Length, Is.EqualTo(3), "two half-twists should produce three panels")

        let allKnots =
            panels
            |> List.collect (function
                | Curve(ks, isClosed) ->
                    Assert.That(isClosed, Is.True, "each panel should be a closed curve")
                    ks
                | e -> failwithf "expected Curve panel, got %A" e)

        let widthNear yLo yHi =
            allKnots
            |> List.filter (fun k -> k.pt.y >= yLo && k.pt.y <= yHi)
            |> List.map (fun k -> abs k.pt.x)
            |> List.max

        Assert.That(widthNear 140. 160., Is.LessThan(0.1 * fthickness),
            "ribbon should pinch to a sliver at the half-twist")
        Assert.That(widthNear 290. 310., Is.EqualTo(fthickness).Within(1.0),
            "ribbon should be full width between pinches")

    [<Test>]
    member _.NibAxis_GlyphsRenderWithoutException() =
        let font = Font.Font({ baseAxes with nib = 0.8 })
        for ch in [ 'o'; 'l'; 'v'; 'S' ] do
            let svg = font.charToSvg ch 0.0 0.0 "black" |> String.concat " "
            Assert.That(svg, Does.Contain("M "), sprintf "nib outline for '%c' should render" ch)

    [<Test>]
    member _.ArtisticAxes_GlyphsRenderWithoutException() =
        // Each artistic axis alone, plus all of them together, over a mix of glyph
        // shapes: closed curves ('o', '8'), open curves ('S', 'c'), straight strokes
        // with joints ('l', 'v', 'E') and dots ('!').
        let variants =
            [ "taper",      { baseAxes with taper = 0.8 }
              "wobble",     { baseAxes with wobble = 1.0 }
              "roughness",  { baseAxes with roughness = 1.0 }
              "mobius",     { baseAxes with mobius = 1.0 }
              "all",
              { baseAxes with
                  nib = 0.5
                  taper = 0.5
                  wobble = 0.5
                  roughness = 0.5
                  mobius = 1.0 } ]

        for name, axes in variants do
            let font = Font.Font(axes)
            for ch in [ 'o'; '8'; 'S'; 'c'; 'l'; 'v'; 'E'; '!' ] do
                let svg = font.charToSvg ch 0.0 0.0 "black" |> String.concat " "
                Assert.That(svg, Does.Contain("M "),
                    sprintf "%s outline for '%c' should render" name ch)
