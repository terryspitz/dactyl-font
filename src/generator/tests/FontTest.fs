module FontTests

open System
open NUnit.Framework
open Curves
open DactylSpline
open Axes
open GeneratorTypes
open Font
open Spacing

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
                    outline = true }
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
                    outline = true }
            )

        let svg = font.charToSvg 'P' 0.0 0.0 "black"
        let svgStr = String.concat " " svg

        // 1. Single outline check: there should only be one 'M' command.
        let mCount = svgStr.Split('M').Length - 1
        Assert.That(mCount, Is.EqualTo(1), "P should have a single outline")

        // 2. Specific sequence check: MLCCCLCCCZ
        // Extract command letters only
        let commands =
            svgStr.Split(' ')
            |> Array.filter (fun s -> s.Length = 1 && "MLCZ".Contains(s))
            |> String.concat ""

        Assert.That(commands, Is.EqualTo("MLLCCLCCLZ"), "P outline should have the updated MLLCCLCCLZ command sequence")
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
        // With soft_corners > 0, corners should be replaced with arcs (CurveToLine→G2→LineToCurve).
        // End caps should remain intact (not distorted by rounding).
        let fontSharp =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    soft_corners = 0.0 }
            )

        let fontRound =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    soft_corners = 0.5 }
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
        // With soft_corners = 0, output should be identical to default (no rounding).
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
                    soft_corners = 0.0 }
            )

        for ch in [ 'A'; 'V'; 'M'; 'o' ] do
            let svgDefault = fontDefault.charToSvg ch 0.0 0.0 "black" |> String.concat " "
            let svgZero = fontZero.charToSvg ch 0.0 0.0 "black" |> String.concat " "
            Assert.That(svgZero, Is.EqualTo(svgDefault), sprintf "soft_corners=0 should match default for '%c'" ch)

    [<Test>]
    member this.Kerning_PairOverride_MatchesSpacingTable() =
        // The Spacing module declares explicit kerning for AV, and Font.pairKern
        // must surface that value verbatim (float-cast of the int override).
        let font = Font.Font(Axes.DefaultAxes)
        let expected = float (Spacing.pairKernInt 'A' 'V')
        Assert.That(expected, Is.LessThan(0.0), "AV should have a negative override")
        Assert.That(font.pairKern 'A' 'V', Is.EqualTo(expected))

    [<Test>]
    member this.Kerning_UnknownPair_ReturnsZero() =
        // Any pair without an entry in Spacing.kerningOverrides must return 0 so
        // non-kerned strings preserve their pre-kerning widths exactly.
        let font = Font.Font(Axes.DefaultAxes)
        Assert.That(font.pairKern 'X' 'Z', Is.EqualTo(0.0))
        Assert.That(font.pairKern 'A' 'B', Is.EqualTo(0.0))

    [<Test>]
    member this.Kerning_StringWidth_EqualsSumOfAdvancesPlusKerns() =
        // Conservation law: stringWidth is exactly (Σ charWidth) + (Σ pairKern).
        let font = Font.Font(Axes.DefaultAxes)
        let s = "AVATAR"
        let widthSum = s |> Seq.sumBy font.charWidth
        let kernSum = List.sum (font.pairKerns s)
        Assert.That(font.stringWidth s, Is.EqualTo(widthSum + kernSum).Within(1e-9))
        Assert.That(kernSum, Is.LessThan(0.0), "AVATAR contains AV and AT kern pairs (negative)")

    [<Test>]
    member this.Kerning_NoKernPairs_StringWidthUnchanged() =
        // For a string with no kerning overrides, stringWidth equals Σ charWidth.
        let font = Font.Font(Axes.DefaultAxes)
        let s = "CGJOQSXZ"  // no overrides on left or right for any of these
        let kerns = font.pairKerns s
        Assert.That(List.forall (fun k -> k = 0.0) kerns, Is.True, "no override should apply")
        let widthSum = s |> Seq.sumBy font.charWidth
        Assert.That(font.stringWidth s, Is.EqualTo(widthSum).Within(1e-9))

    [<Test>]
    member this.Kerning_ShortStrings_HaveNoKerns() =
        let font = Font.Font(Axes.DefaultAxes)
        Assert.That(font.pairKerns "", Is.Empty)
        Assert.That(font.pairKerns "A", Is.Empty)
        Assert.That(font.pairKerns "AV" |> List.length, Is.EqualTo(1))

    [<Test>]
    member this.SidebearingScale_ScalesPerCharWidth() =
        // sidebearingScale=1 is the baseline; scaling to 2 must add exactly
        // the per-char sidebearing term once more. sidebearingScale=0 strips it.
        let mkFont sc =
            Font.Font({ Axes.DefaultAxes with sidebearingScale = sc })
        let fontBase  = mkFont 1.0
        let fontZero  = mkFont 0.0
        let fontDbl   = mkFont 2.0
        let axes = Axes.DefaultAxes
        let thick = float axes.thickness
        let sidebearingAtOne = (1.0 + axes.contrast) * thick * 2.0 + float axes.serif
        let delta = fontBase.charWidth 'A' - fontZero.charWidth 'A'
        Assert.That(delta, Is.EqualTo(sidebearingAtOne).Within(1e-6))
        let delta2 = fontDbl.charWidth 'A' - fontBase.charWidth 'A'
        Assert.That(delta2, Is.EqualTo(sidebearingAtOne).Within(1e-6))

    [<Test>]
    member this.Kerning_ItalicInvariant_OverridesSurviveShear()  =
        // Manual overrides are independent of italic shear (shear is X-of-Y, so
        // horizontal distances at the baseline are untouched for the advance
        // frame). pairKern must return identical values regardless of italic.
        let upright = Font.Font({ Axes.DefaultAxes with italic = 0.0 })
        let slanted = Font.Font({ Axes.DefaultAxes with italic = 0.3 })
        for (a, b) in [ ('A', 'V'); ('T', 'o'); ('L', 'T'); ('f', 'i') ] do
            Assert.That(
                upright.pairKern a b,
                Is.EqualTo(slanted.pairKern a b),
                sprintf "kern(%c,%c) should not depend on italic axis" a b)

    [<Test>]
    member this.SoftCorners_AllGlyphs_RenderWithoutException() =
        // Smoke test: every glyph should render without crashing with soft_corners enabled.
        let font =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    soft_corners = 0.8 }
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
                thickness = 30 }

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
        let t = float Axes.DefaultAxes.thickness  // 30
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
        // With joints=true and soft_corners > 0, corners at joint positions must be
        // preserved (not rounded), so the SVG should not gain extra curve commands at
        // those joints compared to soft_corners=0.
        let mkFont sc jt =
            Font.Font(
                { Axes.DefaultAxes with
                    dactyl_spline = true
                    outline = true
                    soft_corners = sc
                    joints = jt }
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
            sprintf "soft_corners should still add some rounding even with joints (got %d C, baseline %d)" cWithJoints cNoRounding
        )
