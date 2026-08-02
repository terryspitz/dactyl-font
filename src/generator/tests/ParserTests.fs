module ParserTests

open NUnit.Framework
open Axes
open GlyphStringDefs
open GeneratorTypes
open SpiroPointType
open Font

[<TestFixture>]
type ParserTests() =
    let axes =
        { Axes.Axes.DefaultAxes with
            width = 1000
            height = 1000
            x_height = 0.5
            debug = true }

    let metrics = FontMetrics(axes)

    [<Test>]
    member this.TestBasic() =
        // "tl" -> y="t", x="l"
        let pt, _, _, _, label, _ = parse_point metrics "tl"
        Assert.That(label, Is.EqualTo("tl"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.False)
        Assert.That(pt.x_fit, Is.False)

    [<Test>]
    member this.TestOptionalY() =
        // "(t)l" -> y="t" optional, x="l" fixed
        let pt, _, _, _, label, _ = parse_point metrics "(t)l"
        Assert.That(label, Is.EqualTo("(t)l"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.True, "y should be fit")
        Assert.That(pt.x_fit, Is.False, "x should not be fit")

    [<Test>]
    member this.TestOptionalX() =
        // "t(l)" -> y="t" fixed, x="l" optional
        let pt, _, _, _, label, _ = parse_point metrics "t(l)"
        Assert.That(label, Is.EqualTo("t(l)"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.False, "y should not be fit")
        Assert.That(pt.x_fit, Is.True, "x should be fit")

    [<Test>]
    member this.TestBothOptional() =
        // "(t)(l)" -> both optional
        let pt, _, _, _, label, _ = parse_point metrics "(t)(l)"
        Assert.That(label, Is.EqualTo("(t)(l)"))

        Assert.That(pt.y_fit, Is.True)
        Assert.That(pt.x_fit, Is.True)

    [<Test>]
    member this.TestTangentLineToCurveCorner() =
        // "tl-blE~hr" -> Curve to line with explicit tangent E.
        // Should have a point at bl with ty = Corner, th_in = None, th_out = E
        let elem = parse_curve metrics "tl-blE~hr" false

        match elem with
        | Curve(knots, isClosed) ->
            Assert.That(knots.Length, Is.EqualTo(3), "Should have 3 points")
            Assert.That(knots.[0].label, Is.EqualTo(Some "tl"))
            Assert.That(knots.[1].label, Is.EqualTo(Some "blE"))
            Assert.That(knots.[2].label, Is.EqualTo(Some "hr"))
            
            let k = knots.[1]
            Assert.That(k.pt.y, Is.EqualTo(metrics.B))
            Assert.That(k.ty, Is.EqualTo(SpiroPointType.Corner), "Point should be Corner due to explicit tangent")
            Assert.That(k.th_out, Is.EqualTo(Some 0.0), "Point should have the East tangent on th_out")
            Assert.That(k.th_in, Is.EqualTo(None), "Point should have no tangent on th_in")
        | _ -> Assert.Fail("Expected Curve")

    [<Test>]
    member this.TestTangentThrowsOnStraightLine() =
        // "tlE-bl" -> Tangent on a point connected only to a line. Should throw.
        Assert.Throws<System.ArgumentException>(fun () -> parse_curve metrics "tlE-bl" false |> ignore) |> ignore
        // "tl-blE" -> Tangent on end of line. Should throw.
        Assert.Throws<System.ArgumentException>(fun () -> parse_curve metrics "tl-blE" false |> ignore) |> ignore

    [<Test>]
    member this.TestDigitRepeatX() =
        // "r4c" should equal the expanded "rrrrc": four parts R, one part C.
        let expanded, _, _, _, _, _ = parse_point metrics "brrrrc"
        let shorthand, _, _, _, _, _ = parse_point metrics "br4c"
        Assert.That(shorthand.x, Is.EqualTo(expanded.x))
        Assert.That(shorthand.x, Is.EqualTo((4.0 * metrics.R + metrics.C) / 5.0))

    [<Test>]
    member this.TestDigitRepeatY() =
        // "b2t" should equal "bbt": one-third up from the bottom.
        let expanded, _, _, _, _, _ = parse_point metrics "bbtl"
        let shorthand, _, _, _, _, _ = parse_point metrics "b2tl"
        Assert.That(shorthand.y, Is.EqualTo(expanded.y))
        Assert.That(shorthand.y, Is.EqualTo((2.0 * metrics.B + metrics.T) / 3.0))

    [<Test>]
    member this.TestDigitRepeatInBrackets() =
        // Digit weighting works inside fitting brackets, and keeps the fit flag.
        let expanded, _, _, _, _, _ = parse_point metrics "t(rrrrc)"
        let shorthand, _, _, _, _, _ = parse_point metrics "t(r4c)"
        Assert.That(shorthand.x, Is.EqualTo(expanded.x))
        Assert.That(shorthand.x_fit, Is.True)

    [<Test>]
    member this.TestSingleLetterUnchanged() =
        // No digit means count 1 — plain coordinates are unaffected.
        let pt, _, _, _, _, _ = parse_point metrics "tl"
        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))

    [<Test>]
    member this.TestJointMarker() =
        // A trailing `j` marks an explicit interior joint; the coordinate is
        // unchanged and the `j` is consumed (not left in the remaining def).
        let plain, _, _, plainJoint, _, _ = parse_point metrics "hc"
        let jointed, _, _, isJoint, label, rest = parse_point metrics "hcj"
        Assert.That(plainJoint, Is.False, "plain point is not a joint")
        Assert.That(isJoint, Is.True, "`j` suffix should mark a joint")
        Assert.That(jointed.x, Is.EqualTo(plain.x), "x unchanged by joint marker")
        Assert.That(jointed.y, Is.EqualTo(plain.y), "y unchanged by joint marker")
        Assert.That(label, Is.EqualTo("hcj"))
        Assert.That(rest, Is.EqualTo(""), "`j` should be consumed")

    [<Test>]
    member this.TestJointMarkerOnKnotAndDetection() =
        // The `j` marker survives into the parsed knot, and Font.isJoint reports
        // true at that point even when the geometric `joints` heuristic is off.
        let elem = parse_curve metrics "hcj-br" false

        match elem with
        | Curve(knots, _) ->
            Assert.That(knots.[0].isJoint, Is.True, "first knot marked joint")
            Assert.That(knots.[1].isJoint, Is.False, "second knot is a plain terminal")
        | _ -> Assert.Fail("Expected Curve")

        let noHeuristic = FontMetrics({ axes with joints = false })
        let font = Font.Font({ axes with joints = false })
        Assert.That(
            font.isJoint elem (noHeuristic.H) (noHeuristic.C),
            Is.True,
            "explicit joint honoured with the geometric heuristic disabled"
        )

    [<Test>]
    member this.TestCornerMarker() =
        // A trailing `k` marks an explicit corner (kink); the coordinate is unchanged
        // and the `k` is consumed. It composes with the joint marker (`kj`).
        let plain, _, plainCorner, _, _, _ = parse_point metrics "hc"
        let kinked, _, isCorner, _, label, rest = parse_point metrics "hck"
        Assert.That(plainCorner, Is.False, "plain point is not a corner")
        Assert.That(isCorner, Is.True, "`k` suffix should mark a corner")
        Assert.That(kinked.x, Is.EqualTo(plain.x), "x unchanged by corner marker")
        Assert.That(kinked.y, Is.EqualTo(plain.y), "y unchanged by corner marker")
        Assert.That(label, Is.EqualTo("hck"))
        Assert.That(rest, Is.EqualTo(""), "`k` should be consumed")

        let _, _, bothCorner, bothJoint, _, _ = parse_point metrics "hckj"
        Assert.That(bothCorner, Is.True, "`k` before `j` is still a corner")
        Assert.That(bothJoint, Is.True, "`j` after `k` is still a joint")

    [<Test>]
    member this.TestCornerMarkerBreaksLineToCurve() =
        // Without `k` a line running into a curve is a smooth LineToCurve join, so the
        // curve leaves along the line's heading. With `k` it becomes a Corner with both
        // tangents free, letting the curve leave at its own angle (the '5' stem/bowl join).
        match parse_curve metrics "tl-hl~tc" false with
        | Curve(knots, _) ->
            Assert.That(knots.[1].ty, Is.EqualTo(SpiroPointType.Right), "plain join is LineToCurve")
        | _ -> Assert.Fail("Expected Curve")

        match parse_curve metrics "tl-hlk~tc" false with
        | Curve(knots, _) ->
            Assert.That(knots.[1].ty, Is.EqualTo(SpiroPointType.Corner), "`k` join is a Corner")
            Assert.That(knots.[1].th_in, Is.EqualTo(None), "no incoming tangent constraint")
            Assert.That(knots.[1].th_out, Is.EqualTo(None), "no outgoing tangent constraint")
            Assert.That(knots.[1].label, Is.EqualTo(Some "hlk"))
        | _ -> Assert.Fail("Expected Curve")

    [<Test>]
    member this.TestCornerMarkerKeepsExplicitTangent() =
        // `k` and an explicit direction can be combined: the direction still applies to
        // the curve side, and the point is a Corner either way.
        match parse_curve metrics "tl-hlEk~tc" false with
        | Curve(knots, _) ->
            Assert.That(knots.[1].ty, Is.EqualTo(SpiroPointType.Corner))
            Assert.That(knots.[1].th_out, Is.EqualTo(Some 0.0), "East tangent kept on th_out")
            Assert.That(knots.[1].th_in, Is.EqualTo(None))
        | _ -> Assert.Fail("Expected Curve")

[<EntryPoint>]
let main argv =
    0 // Return an integer exit code