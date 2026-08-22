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
        // Both of V's strokes are straight, so the sharp inner corner at BC resolves to a
        // single miter knot — the intersection of the two offset edges — and stays at 7
        // commands. (A sharp inner corner between *curved* edges bevels into two knots
        // instead; see offsetSegment's Corner case.)
        let commands = svgStr.Split(' ') |> Array.filter (fun s -> s = "L" || s = "C")

        Assert.That(
            commands,
            Has.Length.EqualTo(7),
            sprintf "Expected 7 path commands for v outline, got %d in: %s" commands.Length svgStr
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
        // The cursive axis swaps 'a' and 'g' between single-storey (Cursive, the
        // default) and two-storey Roman alternate shapes.  It must change the
        // output of 'a' and 'g' and leave every other glyph untouched.
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

        // 'a' and 'g' should render cleanly and differ between the two settings.
        for ch in [ 'a'; 'g' ] do
            let sCursive = svg fontCursive ch
            let sRoman = svg fontRoman ch
            Assert.That(sRoman, Does.Contain("M "), sprintf "Roman '%c' should render a moveto" ch)
            Assert.That(sRoman, Does.Not.Contain("NaN"), sprintf "Roman '%c' should not contain NaN" ch)
            Assert.That(sRoman, Does.Not.Contain("stroke:#e00000"), sprintf "Roman '%c' outline should not fail" ch)
            Assert.That(sRoman, Is.Not.EqualTo(sCursive), sprintf "cursive should change '%c'" ch)

        // Every other glyph must be identical with the axis on or off.
        for ch in "bcdefhijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" do
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
    member this.Kerning_OpticalOff_AllPairsKernToZero() =
        // Kerning is driven entirely by optical sampling, so turning the axis
        // off must leave every pair completely unkerned.
        let font = Font.Font({ Axes.DefaultAxes with opticalKerning = 0.0 })
        for (a, b) in [ ('A', 'V'); ('T', 'o'); ('f', 'j'); ('K', 'O') ] do
            Assert.That(font.pairKern a b, Is.EqualTo(0.0),
                        sprintf "kern(%c,%c) should be 0 with optical off" a b)

    [<Test>]
    member this.Kerning_UnknownPair_ReturnsZero() =
        // With optical kerning off, any pair without a manual override returns 0.
        let font = Font.Font({ Axes.DefaultAxes with opticalKerning = 0.0 })
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
        // AVATAR's AV/AT diagonal pairs tuck in, so optical kerning nets negative.
        Assert.That(kernSum, Is.LessThan(0.0), "AVATAR's diagonal pairs should kern negative")

    [<Test>]
    member this.Kerning_NoKernPairs_StringWidthUnchanged() =
        // For a string with no kerning overrides AND optical kerning off,
        // stringWidth equals Σ charWidth.
        let font = Font.Font({ Axes.DefaultAxes with opticalKerning = 0.0 })
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
    member this.Spacing_MovesGlyphsApart_WithOpticalKerningOn() =
        // Regression: `spacing` used to have NO effect with optical kerning on.
        // pairKern returns `target - advanceA - deltaMin`, so the placed
        // position `advanceA + kern` cancels advanceA identically — every unit
        // of spacing (or sidebearing) added to the advance was subtracted
        // straight back out. Passing `spacing` in as the target is the fix.
        let placed s =
            let f = Font.Font({ Axes.DefaultAxes with spacing = s; opticalKerning = 1.0 })
            f.charWidth 'A' + f.pairKern 'A' 'V'
        // 1 unit of spacing must buy 1 unit of separation (±1 for kern rounding).
        Assert.That(placed 40 - placed 0, Is.EqualTo(40.0).Within(1.0))
        Assert.That(placed 100 - placed 40, Is.EqualTo(60.0).Within(1.0))

    [<Test>]
    member this.Spacing_DoesNotChangeKernValues() =
        // The `+spacing` in the target cancels the one inside advanceA, so the
        // kern *value* is spacing-invariant — changing tracking must not churn
        // every entry of the exported kern/GPOS table.
        //
        // Holds only above the range where the give-away cap binds: that cap is
        // a fraction of the target (see GlyphProfile.maxGiveFraction, which
        // stops two receding sides closing the gap to nothing), so at tight
        // settings it does move the advance and the kern follows. 'n' gives away
        // at most 12.1, so the cap is slack for it from spacing ~35 up.
        let kern s =
            Font.Font({ Axes.DefaultAxes with spacing = s; opticalKerning = 1.0 }).pairKern 'n' 'n'
        Assert.That(kern 200, Is.EqualTo(kern 100).Within(1.0))
        Assert.That(kern 150, Is.EqualTo(kern 100).Within(1.0))

    [<Test>]
    member this.OpticalSidebearings_NeverCollide() =
        // Regression: each side's recession was subtracted independently with no
        // floor, so two heavily-receding sides could between them subtract more
        // than the whole gap. At spacing=100 that closed T|T to -14.3, I|T to
        // -5.7 and L|I to exactly 0 — ink touching or overlapping — and at the
        // Sidebearings stop there is no pair kern to rescue it.
        let font = Font.Font({ Axes.DefaultAxes with opticalKerning = 0.5; outline = true; filled = true })
        let inkGap (a: char) (b: char) =
            let pa, pb = font.glyphProfile a, font.glyphProfile b
            let ra = pa.RightEdges |> Array.filter (fun v -> v > System.Double.NegativeInfinity) |> Array.max
            let lb = pb.LeftEdges |> Array.filter (fun v -> v < System.Double.PositiveInfinity) |> Array.min
            (font.charWidth a + font.pairKern a b + font.glyphShift b + lb) - (font.glyphShift a + ra)
        let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let offenders =
            [ for a in chars do
                for b in chars do
                    let g = inkGap a b
                    if g < 10.0 then yield sprintf "%c%c=%.1f" a b g ]
        Assert.That(offenders, Is.Empty, sprintf "pairs whose ink nearly or actually collides: %A" offenders)

    [<Test>]
    member this.Kerning_ItalicInvariant_OverridesSurviveShear()  =
        // Kerns are independent of italic shear: profiles are sampled in the
        // pre-italic design frame, and any manual override is a static value.
        // pairKern must return identical values regardless of italic.
        let upright = Font.Font({ Axes.DefaultAxes with slant = 0.0 })
        let slanted = Font.Font({ Axes.DefaultAxes with slant = 0.3 })
        for (a, b) in [ ('A', 'V'); ('T', 'o'); ('L', 'T'); ('f', 'i') ] do
            Assert.That(
                upright.pairKern a b,
                Is.EqualTo(slanted.pairKern a b),
                sprintf "kern(%c,%c) should not depend on italic axis" a b)

    [<Test>]
    [<Explicit("Diagnostic — print optical kern values for notable pairs")>]
    member this.Diagnostic_OpticalKernValues() =
        // Run with: dotnet test --filter "Diagnostic_OpticalKernValues" \
        //                       --logger "console;verbosity=detailed"
        let axes = { Axes.DefaultAxes with opticalKerning = 1.0; outline = true; filled = true }
        let font = Font.Font(axes)
        let metrics = FontMetrics(axes)
        let thickness = float axes.weight
        let bandY0 = metrics.D - thickness
        let bandY1 = metrics.T + thickness
        let bandCount = 32
        // Profile every char that appears in the pairs we want to inspect.
        let chars = "AVTLOWYKfMabceigjlmnoprsuvwy.!,'"
        let profiles = System.Collections.Generic.Dictionary<char, GlyphProfile.GlyphProfile>()
        for c in chars do
            try
                let outline = font.CharToOutlinePreItalic c
                let svg, _, _ = font.elementToSvg outline
                let path = String.concat " " svg
                if path <> "" then
                    let cmds = GlyphProfile.parseSvgCommands path
                    profiles.[c] <- GlyphProfile.sampleProfile bandY0 bandY1 bandCount cmds
            with _ -> ()
        let opticalRaw (a: char) (b: char) : int =
            if profiles.ContainsKey(a) && profiles.ContainsKey(b) then
                GlyphProfile.pairKern (float axes.spacing) (font.charWidth a) profiles.[a] profiles.[b]
            else 0
        // Notable pairs: diagonals, overhangs, round-to-round and slab sequences.
        let pairs = [
            'V', 'o'; 'V', 'a'; 'V', 'e'; 'V', 'u'
            'Y', 'o'; 'Y', 'a'; 'Y', 'e'; 'Y', 'u'
            'W', 'o'; 'W', 'a'; 'W', 'e'; 'W', 'u'
            'T', 'o'; 'T', 'a'; 'T', 'e'; 'T', 'u'
            'A', 'V'; 'A', 'W'; 'A', 'Y'; 'A', 'T'
            'L', 'T'; 'L', 'V'; 'L', 'W'; 'L', 'Y'
            'K', 'O'; 'K', 'o'; 'K', 'e'; 'K', 'u'
            'M', 'o'; 'M', 'i'; 'M', 'a'; 'M', 'e'
            'f', 'a'; 'f', 'e'; 'f', 'o'; 'f', 'u'; 'f', 'i'; 'f', 'l'; 'f', 'j'
            'r', 'n'; 'r', 'm'; 'r', 'u'; 'r', 'a'
            'l', 'o'; 'o', 'l'; 'n', 'n'; 'o', 'o'
        ]
        printfn ""
        printfn "============ OPTICAL KERN (target=%d) ============" axes.spacing
        printfn "  pair  kern"
        for (a, b) in pairs do
            printfn "  %c%c    %5d" a b (opticalRaw a b)
        printfn "================================================="
        Assert.Pass()

    [<Test>]
    member this.SvgAndOtfKerns_AgreeForEveryPair() =
        // The SVG render path calls Font.pairKern per consecutive pair.
        // The OTF emission in Api.generateFontGlyphData builds a kern table
        // from the same Spacing overrides + GlyphProfile.pairKern. If the
        // two diverge (different threshold, different formula, etc.) text
        // laid out in CSS via the @font-face will differ from text laid
        // out by the SVG renderer — and you can't tell from looking at
        // either one alone. This test reproduces both sides on the same
        // axes and asserts equality across a representative sample.
        let axes = { Axes.DefaultAxes with opticalKerning = 1.0; outline = true; filled = true }
        let font = Font.Font(axes)
        let metrics = FontMetrics(axes)
        let thickness = float axes.weight
        let bandY0 = metrics.D - thickness
        let bandY1 = metrics.T + thickness
        let bandCount = 32
        // Sample profiles for the test characters using the same recipe
        // generateFontGlyphData uses (pre-italic outline, same band count).
        let testChars = "AVTLOoaeingdHmMYW.fjyt"
        let profiles = System.Collections.Generic.Dictionary<char, GlyphProfile.GlyphProfile>()
        for c in testChars do
            try
                let outline = font.CharToOutlinePreItalic c
                let svg, _, _ = font.elementToSvg outline
                let path = String.concat " " svg
                if path <> "" then
                    let cmds = GlyphProfile.parseSvgCommands path
                    profiles.[c] <- GlyphProfile.sampleProfile bandY0 bandY1 bandCount cmds
            with _ -> ()
        // Compute "OTF kern" exactly as Api would: the residual left over once
        // each glyph's own optical sidebearings (advance + shift) have been
        // applied. Both sides must agree on advance, shift AND threshold — get
        // any one of the three wrong and the CSS and SVG renders drift apart.
        let otfKern (a: char) (b: char) : int =
            if profiles.ContainsKey(a) && profiles.ContainsKey(b) then
                GlyphProfile.residualKern
                    (float axes.spacing)
                    (font.charWidth a)
                    (font.glyphShift a)
                    (font.glyphShift b)
                    Font.kernThreshold
                    profiles.[a]
                    profiles.[b]
            else 0
        // Compare across all ordered pairs of test chars.
        let mismatches = ResizeArray()
        for a in testChars do
            for b in testChars do
                let svgK = int (font.pairKern a b)
                let otfK = otfKern a b
                if svgK <> otfK then
                    mismatches.Add(sprintf "(%c,%c): svg=%d otf=%d" a b svgK otfK)
        if mismatches.Count > 0 then
            Assert.Fail(sprintf "SVG and OTF kerns disagree on %d pairs:\n%s"
                         mismatches.Count (String.concat "\n" mismatches))

    [<Test>]
    member this.OpticalKerning_ProfileSamplerIsItalicInvariant() =
        // Italic shear is X-of-Y so it shifts ink horizontally per band, but
        // the BAND-WISE leftmost / rightmost x at any given y move uniformly
        // for both glyphs in a pair. The profile-derived kern is invariant.
        // (We sample the pre-italicise outline to keep this exact in code.)
        let upright = Font.Font({ Axes.DefaultAxes with slant = 0.0; opticalKerning = 1.0 })
        let slanted = Font.Font({ Axes.DefaultAxes with slant = 0.3; opticalKerning = 1.0 })
        // pairs without manual overrides — exercise the optical path
        for (a, b) in [ ('C', 'O'); ('O', 'X'); ('S', 'Q') ] do
            Assert.That(
                upright.opticalPairKern a b,
                Is.EqualTo(slanted.opticalPairKern a b),
                sprintf "optical kern(%c,%c) should not depend on italic axis" a b)

    [<Test>]
    [<Explicit("Performance benchmark — invoke with --filter to run")>]
    member this.Benchmark_OpticalKerning_FullFontBuild() =
        // Replicates the work generateFontGlyphData does: render every glyph
        // outline, then (if optical) sample edge profiles and compute kerns
        // for every glyph pair. Times opticalKerning OFF vs ON for comparison.
        // Run with: dotnet test --filter "Benchmark_OpticalKerning" \
        //                       --logger "console;verbosity=detailed"
        let allChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@"
        let runOnce (opticalOn: float) =
            let axes = { Axes.DefaultAxes with opticalKerning = opticalOn; outline = true; filled = true }
            let font = Font.Font(axes)
            let metrics = FontMetrics(axes)
            let thickness = float axes.weight
            let bandY0 = metrics.D - thickness
            let bandY1 = metrics.T + thickness
            let bandCount = 32
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let profiles = System.Collections.Generic.Dictionary<char, GlyphProfile.GlyphProfile>()
            let mutable glyphCount = 0
            for c in allChars do
                try
                    let outline = font.CharToOutline c
                    let svg, _, _ = font.elementToSvg outline
                    let path = String.concat " " svg
                    glyphCount <- glyphCount + 1
                    if opticalOn >= 0.75 && path <> "" then
                        let cmds = GlyphProfile.parseSvgCommands path
                        profiles.[c] <- GlyphProfile.sampleProfile bandY0 bandY1 bandCount cmds
                with _ -> ()
            let glyphsMs = sw.ElapsedMilliseconds
            sw.Restart()
            let mutable opticalCount = 0
            if opticalOn >= 0.75 then
                for KeyValue(cL, pL) in profiles do
                    let advanceL = font.charWidth cL
                    for KeyValue(cR, pR) in profiles do
                        let k = GlyphProfile.pairKern (float axes.spacing) advanceL pL pR
                        if abs k >= 3 then opticalCount <- opticalCount + 1
            let kernMs = sw.ElapsedMilliseconds
            glyphsMs, kernMs, glyphCount, opticalCount
        // warm-up
        let _ = runOnce 0.0
        let _ = runOnce 1.0
        let runs = 3
        let mutable offGlyphs = 0L
        let mutable onGlyphs = 0L
        let mutable onKern = 0L
        let mutable opticalCount = 0
        let mutable glyphCount = 0
        for _ in 1 .. runs do
            let g1, _, n1, _ = runOnce 0.0
            let g2, k2, n2, oc = runOnce 1.0
            offGlyphs <- offGlyphs + g1
            onGlyphs <- onGlyphs + g2
            onKern <- onKern + k2
            opticalCount <- oc
            glyphCount <- n2
            ignore n1
        let offAvg = float offGlyphs / float runs
        let onGAvg = float onGlyphs / float runs
        let onKAvg = float onKern / float runs
        let totalOn = onGAvg + onKAvg
        printfn ""
        printfn "================ OPTICAL KERNING BENCHMARK ================"
        printfn "  Per generateFontGlyphData call (avg of %d runs, %d glyphs):" runs glyphCount
        printfn "    opticalKerning OFF: %.1f ms (outlines only)" offAvg
        printfn "    opticalKerning ON:  %.1f ms total" totalOn
        printfn "      outlines + profiles : %.1f ms" onGAvg
        printfn "      kern computation    : %.1f ms (%d optical pairs emitted)" onKAvg opticalCount
        printfn "    overhead from optical: +%.1f ms (+%.1f%%)" (totalOn - offAvg) ((totalOn - offAvg) / offAvg * 100.0)
        printfn "==========================================================="
        Assert.Pass()

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

[<TestFixture>]
type PenAxisTests() =

    /// All (x, y) pairs in a glyph's rendered path.
    let points (axes: Axes) (ch: char) =
        Font.Font(axes).charToSvg ch 0.0 0.0 "black"
        |> String.concat " "
        |> fun s ->
            System.Text.RegularExpressions.Regex.Matches(s, @"[ML] (-?\d+),(-?\d+)")
            |> Seq.map (fun m -> float m.Groups.[1].Value, float m.Groups.[2].Value)
            |> List.ofSeq

    /// Number of closed subpaths — one per ribbon edge loop.
    let subpaths (axes: Axes) (ch: char) =
        Font.Font(axes).charToSvg ch 0.0 0.0 "black"
        |> String.concat " "
        |> fun s -> s.Split([| "M " |], StringSplitOptions.None).Length - 1

    /// Outer bounding box of a glyph's outline. A stroke that has been widened
    /// pushes its outer edge out, so the box grows — which is checkable without
    /// depending on how the outline happens to be sampled.
    let bbox (axes: Axes) (ch: char) =
        let ps = points axes ch
        let xs = List.map fst ps
        let ys = List.map snd ps
        List.min xs, List.min ys, List.max xs, List.max ys

    let boxArea axes ch =
        let (x0, y0, x1, y1) = bbox axes ch
        (x1 - x0) * (y1 - y0)

    [<Test>]
    member _.Traces_OneIsASolidStroke() =
        // The whole design rests on traces = 1 being the ordinary stroke, so that
        // every existing font renders unchanged.
        let solid = { Axes.DefaultAxes with traces = 1 }
        for ch in [ 'o'; 'a'; 'n'; 'S' ] do
            Assert.That(points solid ch, Is.EqualTo<list<float * float>>(points Axes.DefaultAxes ch), sprintf "traces=1 changed '%c'" ch)

    [<Test>]
    member _.Traces_EachAddsARibbon() =
        // 'o' is a closed curve, so each ribbon contributes an outer and an inner loop.
        let at n =
            subpaths { Axes.DefaultAxes with traces = n; trace_spread = 2.0; trace_weight = 0.15 } 'o'
        Assert.That(at 1, Is.EqualTo(2))
        Assert.That(at 2, Is.EqualTo(4))
        Assert.That(at 4, Is.EqualTo(8))

    [<Test>]
    member _.Traces_StayInsideTheGlyph() =
        // Offsetting further than the radius of curvature used to fold a trace back
        // through itself, which showed up as loops escaping the letter. The saturation
        // in clampOffset should keep every trace within the solid stroke's own bounds
        // plus the spread it was asked for.
        let axes = { Axes.DefaultAxes with weight = 60; traces = 3; trace_spread = 3.0; trace_weight = 0.1 }
        let allowed = float axes.weight * axes.trace_spread
        for ch in [ 'e'; '5'; '8'; 'S'; 'g' ] do
            let baseline = points Axes.DefaultAxes ch
            let bx = baseline |> List.map fst
            let by = baseline |> List.map snd
            for (x, y) in points axes ch do
                Assert.That(x, Is.InRange(List.min bx - allowed, List.max bx + allowed), sprintf "'%c' x escaped" ch)
                Assert.That(y, Is.InRange(List.min by - allowed, List.max by + allowed), sprintf "'%c' y escaped" ch)

    [<Test>]
    member _.Pressure_RespondsToCurvatureAndNothingElse() =
        // The point of pressure: width follows how tightly the spine turns, so the
        // glyphs built only from straight lines must come out untouched while the
        // round ones fatten. The straights below are the repo's own list (see
        // docs/TODO.md), and the fact that they are exactly the glyphs pressure
        // leaves alone is the check that it is reading curvature and not, say, arc
        // length.
        let pressed = { Axes.DefaultAxes with pressure = 1.0 }
        for ch in "AEFHIKLMNTVWXYZ147" do
            Assert.That(points pressed ch, Is.EqualTo<list<float * float>>(points Axes.DefaultAxes ch), sprintf "straight glyph '%c' has no curvature to respond to" ch)
        for ch in "oscOSC" do
            Assert.That(boxArea pressed ch, Is.GreaterThan(boxArea Axes.DefaultAxes ch), sprintf "round glyph '%c' should thicken" ch)

    [<Test>]
    member _.InkSpread_WidensStraightsAndCurvesAlike() =
        // Bleed is a property of the paper, not of how the stroke was drawn, so unlike
        // pressure it must reach the straight glyphs too.
        let bled = { Axes.DefaultAxes with ink_spread = 1.0 }
        for ch in [ 'I'; 'H'; 'o'; 'S' ] do
            Assert.That(boxArea bled ch, Is.GreaterThan(boxArea Axes.DefaultAxes ch), sprintf "'%c' should bleed wider" ch)

    [<Test>]
    member _.Gravity_SagsHorizontalsAndSparesVerticals() =
        // Sag is downward in the glyph, not perpendicular to the stroke: the
        // perpendicular of a vertical stem is horizontal, so there is nothing for
        // gravity to pull it into. '|' is a single vertical bar.
        let heavy = { Axes.DefaultAxes with gravity = 1.0 }
        // Compared against a baseline that samples the same way rather than against
        // the defaults. Switching gravity on makes the sampler emit interior points
        // along straight segments — it has to, or sag evaluated only at the two ends
        // would be no sag at all — and that alone shifts an integer-rounded outline by
        // a unit, for every sampled axis, not just this one. Holding the sampling
        // fixed isolates what gravity itself does, which to a vertical bar is nothing.
        let sampledBase = { Axes.DefaultAxes with roughness = 1e-9 }
        let sampledHeavy = { sampledBase with gravity = 1.0 }
        Assert.That(bbox sampledHeavy '|', Is.EqualTo(bbox sampledBase '|'), "a vertical bar should not sag sideways")
        // An em dash is a single long horizontal, which is the case gravity exists for:
        // its middle should hang below where its ends are pinned. Measured on the
        // bottom edge, since that is what the sag carries downward. (A 'T' would not
        // show it on the bounding box — its stem foot sits on the baseline and hides
        // any droop in the bar above.)
        let lowest ax = points ax '—' |> List.map snd |> List.min
        Assert.That(lowest sampledHeavy, Is.LessThan(lowest sampledBase), "a horizontal stroke should sag at its middle")

    [<Test>]
    member _.Bounce_IsPerGlyphAndRepeatable() =
        // A font whose glyphs moved between renders could not be exported, and the two
        // 'o's in "look" have to agree, so the offset must come from the code point.
        let font = Font.Font({ Axes.DefaultAxes with bounce = 1.0 })
        for ch in [ 'a'; 'x'; '5' ] do
            Assert.That(font.bounceOffset ch, Is.EqualTo(font.bounceOffset ch))
        let offsets = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ] |> List.map font.bounceOffset
        Assert.That(List.distinct offsets |> List.length, Is.GreaterThan(3), "glyphs should not all bounce alike")
        // Bounded: a hand-lettered line wanders, it does not come apart.
        let metrics = FontMetrics({ Axes.DefaultAxes with bounce = 1.0 })
        for o in offsets do
            Assert.That(abs o, Is.LessThan(float metrics.X * 0.2))
        Assert.That(Font.Font(Axes.DefaultAxes).bounceOffset 'a', Is.EqualTo(0.0), "off by default")

[<TestFixture>]
type CornerOutlineTests() =
    let metrics = FontMetrics(Axes.DefaultAxes)
    let fthickness = float Axes.DefaultAxes.weight

    /// Widest excursion of the outline beyond the spine's own bounding box. A corner
    /// miter should stay within a stroke width or two; a degenerate one spikes far out.
    let outlineOvershoot (font: Font.Font) (def: string) =
        let elem = GlyphStringDefs.rawDefToElem metrics def false
        let sl, sr, sb, st = bounds elem
        let ol, orr, ob, ot = bounds (font.getOutline elem)
        List.max [ sl - ol; orr - sr; sb - ob; ot - st ]

    [<Test>]
    member _.CuspOutline_DoesNotSpike_OnNearReversal() =
        // A `K` cusp where the curve doubles back on itself is a ~180 degree bend.
        // norm() maps that to +PI or -PI arbitrarily, so the corner can be classified
        // as an inner bend, whose miter distance w/cos(bend/2) is unbounded there — it
        // used to shoot a spike right out of the glyph (seen on a '3' waisted at the
        // centre). Both sides must wrap the tip instead.
        for constantOffset in [ true; false ] do
            let font =
                Font.Font(
                    { Axes.DefaultAxes with
                        dactyl_spline = true
                        outline = true
                        constant_offset = constantOffset }
                )

            // Two arcs meeting head-on at the middle: in heading West, out heading East.
            let overshoot = outlineOvershoot font "tol~t(c)~(th)r~hcK~(bh)r~b(c)~bol"

            Assert.That(
                overshoot,
                Is.LessThan(1.55 * fthickness),
                sprintf
                    "cusp outline should stay near the spine (constant_offset=%b), overshot by %.1f"
                    constantOffset
                    overshoot
            )

    [<Test>]
    member _.CuspGlyphs_RenderWithoutFallback() =
        // '3' and '5' are now single strokes joined at a `K` kink; a solver or outline
        // failure would fall back to the red error dot.
        for axes in
            [ Axes.DefaultAxes
              { Axes.DefaultAxes with weight = 60 }
              { Axes.DefaultAxes with serif = 30 }
              { Axes.DefaultAxes with constant_offset = false }
              { Axes.DefaultAxes with dactyl_spline = false } ] do
            let font = Font.Font(axes)
            for ch in [ '3'; '5'; 'm' ] do
                let svg = font.charToSvg ch 0.0 0.0 "black" |> String.concat " "
                Assert.That(svg, Does.Contain("M "), sprintf "'%c' should render" ch)
                Assert.That(svg, Does.Not.Contain("stroke:#e00000"),
                    sprintf "'%c' should not fall back to the error dot" ch)

    [<Test>]
    member _.AcuteJoin_DoesNotTaperTheIncomingStroke() =
        // '5' runs its stem into its bowl at an acute kink. The stem's east edge must
        // stay dead straight all the way down to the join: a bisector miter there lands
        // off both offset edges (further off the sharper the corner, and further still
        // once clamped), which sloped this edge inwards and visibly tapered the stem.
        //
        // Checked on both outline-building paths — the sampled/constant-offset path
        // (default; emitAtBezPt's Corner case) and the segment-based path
        // (constant_offset=false; offsetSegment's Corner case) — since they have their
        // own, independent implementations of the same corner geometry and the taper
        // bug was fixed in each separately.
        for constantOffset in [ true; false ] do
            let axes = { Axes.DefaultAxes with constant_offset = constantOffset }
            let font = Font.Font(axes)
            let metrics = FontMetrics(axes)
            let t = metrics.thickness
            // charToElem translates the glyph by (thickness, thickness).
            let spineX = metrics.L + t
            let joinY = metrics.H + t
            let edgeX = spineX + t * (1.0 + axes.contrast)

            let outline =
                match font.CharToOutline '5' with
                | Curve(knots, _) -> knots |> List.map (fun k -> k.pt.x, k.pt.y)
                | e -> failwithf "expected a single Curve outline for '5' (constant_offset=%b), got %A" constantOffset e

            // Where does the outline cross a given height, on the stem's east side?
            let crossingsAt (y: float) =
                [ for i in 0 .. outline.Length - 1 do
                    let x1, y1 = outline.[i]
                    let x2, y2 = outline.[(i + 1) % outline.Length]
                    if (y1 - y) * (y2 - y) <= 0.0 && abs (y2 - y1) > 1e-9 then
                        let x = x1 + (x2 - x1) * (y - y1) / (y2 - y1)
                        if x > spineX then yield x ]

            for above in [ 2.0; 4.0; 6.0 ] do
                let y = joinY + above * t
                let xs = crossingsAt y
                Assert.That(xs, Is.Not.Empty, sprintf "outline should have an east-side edge at y=%.0f (constant_offset=%b)" y constantOffset)
                let nearest = xs |> List.minBy (fun x -> abs (x - edgeX))
                Assert.That(
                    abs (nearest - edgeX),
                    Is.LessThan 2.0,
                    sprintf
                        "stem's east edge should sit at x=%.1f at y=%.0f (%.0f above the join, constant_offset=%b), got %.1f — the stem is tapering into the join"
                        edgeX y (above * t) constantOffset nearest
                )

    [<Test>]
    member _.ZGlyph_OutlineDoesNotCrossItself() =
        // 'z' is three straight strokes meeting at two acute corners. Both edges of each
        // corner really are straight, so their offset lines do meet and the inner side
        // resolves to that single meeting point. Ending the two edges at their own
        // perpendicular feet instead cuts the corner: the incoming edge stops short and
        // dives inside the bar it is joining, so the contour crosses its own bar edge —
        // a spike and a notch at each corner, and a diagonal thinner than the stroke.
        //
        // Checked on both outline-building paths — the sampled/constant-offset path
        // (default; emitAtBezPt's Corner case) and the segment-based path
        // (constant_offset=false; offsetSegment's Corner case) — since each has its own
        // independent implementation of the same corner geometry.
        for constantOffset in [ true; false ] do
            let axes = { Axes.DefaultAxes with constant_offset = constantOffset }
            let font = Font.Font(axes)

            let pts =
                match font.CharToOutline 'z' with
                | Curve(knots, _) -> knots |> List.map (fun k -> k.pt.x, k.pt.y) |> Array.ofList
                | e -> failwithf "expected a single Curve outline for 'z' (constant_offset=%b), got %A" constantOffset e

            let n = pts.Length

            // Do two *open* segments properly cross? Touching at a shared endpoint doesn't count.
            let crosses (ax, ay) (bx, by) (cx, cy) (dx, dy) =
                let side (px, py) (qx, qy) (rx, ry) = (qx - px) * (ry - py) - (qy - py) * (rx - px)
                let d1 = side (ax, ay) (bx, by) (cx, cy)
                let d2 = side (ax, ay) (bx, by) (dx, dy)
                let d3 = side (cx, cy) (dx, dy) (ax, ay)
                let d4 = side (cx, cy) (dx, dy) (bx, by)
                d1 * d2 < 0.0 && d3 * d4 < 0.0

            for i in 0 .. n - 1 do
                // j starts at i+2 so neighbouring segments (which share a vertex) are skipped,
                // and the last segment is skipped for i=0 since it wraps back to vertex 0.
                for j in i + 2 .. n - 1 do
                    if not (i = 0 && j = n - 1) then
                        let a, b = pts.[i], pts.[(i + 1) % n]
                        let c, d = pts.[j], pts.[(j + 1) % n]

                        Assert.That(
                            crosses a b c d,
                            Is.False,
                            sprintf
                                "'z' outline crosses itself (constant_offset=%b): %A-%A crosses %A-%A — an acute corner is cutting into its own stroke"
                                constantOffset a b c d
                        )

    [<Test>]
    member _.CuspGlyphs_AreSingleStrokes() =
        // The point of the `K` marker: '3' and '5' are one curve each, not two
        // overlapping strokes whose end caps meet in the middle.
        let font = Font.Font(Axes.DefaultAxes)

        let curveCount ch =
            let rec count e =
                match e with
                | Curve _ -> 1
                | EList(es) -> List.sumBy count es
                | _ -> 0
            count (font.charToElem ch)

        Assert.That(curveCount '3', Is.EqualTo(1), "'3' should be a single stroke")
        Assert.That(curveCount '5', Is.EqualTo(1), "'5' should be a single stroke")

    [<Test>]
    member _.M_ArchesAreOneStrokeJoinedAtTheMiddleLeg() =
        // 'm' is stem + (arch, kink, arch, right leg) + middle leg: the two arches
        // belong to one stroke that kinks over the middle leg, rather than the second
        // arch springing off the first leg with an end cap in the crotch.
        let font = Font.Font(Axes.DefaultAxes)

        let curves =
            let rec collect e =
                match e with
                | Curve(knots, _) -> [ knots ]
                | EList(es) -> List.collect collect es
                | _ -> []
            collect (font.charToElem 'm')

        Assert.That(curves.Length, Is.EqualTo(3), "'m' should be three strokes")

        let labelsOf (knots: Knot list) = knots |> List.choose (fun k -> k.label)

        let archStroke =
            curves
            |> List.filter (fun ks ->
                let ls = labelsOf ks
                List.contains "x(llw)" ls && List.contains "x(rw)" ls)

        Assert.That(archStroke.Length, Is.EqualTo(1), "both arch apexes should be on one stroke")

        let kink = archStroke.Head |> List.find (fun k -> k.label = Some "xxblwK")
        Assert.That(kink.ty, Is.EqualTo(GeneratorTypes.Corner), "the arches meet at a corner")
        Assert.That(kink.th_in, Is.EqualTo(None), "kink tangents are left to the solver")
        Assert.That(kink.th_out, Is.EqualTo(None), "kink tangents are left to the solver")

        // The middle leg hangs off that same point as an explicit joint, so its top
        // gets no cap (serif / flare / bulb) in the middle of the letter.
        let legTop =
            curves
            |> List.collect id
            |> List.filter (fun k -> k.label = Some "xxblwJ")

        Assert.That(legTop.Length, Is.EqualTo(1), "middle leg should start at the kink")
        Assert.That(legTop.Head.isJoint, Is.True, "middle leg top is a joint")
        Assert.That(legTop.Head.pt.GetXY, Is.EqualTo(kink.pt.GetXY), "leg top sits on the kink")
