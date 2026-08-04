module ParserTests

open NUnit.Framework
open Axes
open GlyphStringDefs
open GeneratorTypes
open SpiroPointType
open Font

[<TestFixture>]
type ParserTests() =
    // Optical corrections off, so that the tests below can assert the exact
    // arithmetic of the coordinate language; they get their own tests further
    // down.
    let axes =
        { Axes.Axes.DefaultAxes with
            width = 1000
            height = 1000
            x_height = 0.5
            overshoot = 0
            balance = 0
            debug = true }

    let metrics = FontMetrics(axes)

    [<Test>]
    member this.TestBasic() =
        // "tl" -> y="t", x="l"
        let pt, _, _, label, _ = parse_point metrics "tl"
        Assert.That(label, Is.EqualTo("tl"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.False)
        Assert.That(pt.x_fit, Is.False)

    [<Test>]
    member this.TestOptionalY() =
        // "(t)l" -> y="t" optional, x="l" fixed
        let pt, _, _, label, _ = parse_point metrics "(t)l"
        Assert.That(label, Is.EqualTo("(t)l"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.True, "y should be fit")
        Assert.That(pt.x_fit, Is.False, "x should not be fit")

    [<Test>]
    member this.TestOptionalX() =
        // "t(l)" -> y="t" fixed, x="l" optional
        let pt, _, _, label, _ = parse_point metrics "t(l)"
        Assert.That(label, Is.EqualTo("t(l)"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.False, "y should not be fit")
        Assert.That(pt.x_fit, Is.True, "x should be fit")

    [<Test>]
    member this.TestBothOptional() =
        // "(t)(l)" -> both optional
        let pt, _, _, label, _ = parse_point metrics "(t)(l)"
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
        let expanded, _, _, _, _ = parse_point metrics "brrrrc"
        let shorthand, _, _, _, _ = parse_point metrics "br4c"
        Assert.That(shorthand.x, Is.EqualTo(expanded.x))
        Assert.That(shorthand.x, Is.EqualTo((4.0 * metrics.R + metrics.C) / 5.0))

    [<Test>]
    member this.TestDigitRepeatY() =
        // "b2t" should equal "bbt": one-third up from the bottom.
        let expanded, _, _, _, _ = parse_point metrics "bbtl"
        let shorthand, _, _, _, _ = parse_point metrics "b2tl"
        Assert.That(shorthand.y, Is.EqualTo(expanded.y))
        Assert.That(shorthand.y, Is.EqualTo((2.0 * metrics.B + metrics.T) / 3.0))

    [<Test>]
    member this.TestDigitRepeatInBrackets() =
        // Digit weighting works inside fitting brackets, and keeps the fit flag.
        let expanded, _, _, _, _ = parse_point metrics "t(rrrrc)"
        let shorthand, _, _, _, _ = parse_point metrics "t(r4c)"
        Assert.That(shorthand.x, Is.EqualTo(expanded.x))
        Assert.That(shorthand.x_fit, Is.True)

    [<Test>]
    member this.TestSingleLetterUnchanged() =
        // No digit means count 1 — plain coordinates are unaffected.
        let pt, _, _, _, _ = parse_point metrics "tl"
        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))

    [<Test>]
    member this.TestJointMarker() =
        // A trailing `j` marks an explicit interior joint; the coordinate is
        // unchanged and the `j` is consumed (not left in the remaining def).
        let plain, _, plainJoint, _, _ = parse_point metrics "hc"
        let jointed, _, isJoint, label, rest = parse_point metrics "hcj"
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

/// Optical corrections from https://www.typography.com/blog/typographic-illusions:
/// overshoot (round/pointed extremes project past the flat guides) and balance
/// (the optical middle sits above the geometric half).
[<TestFixture>]
type OpticalTests() =
    let axes =
        { Axes.Axes.DefaultAxes with
            width = 1000
            height = 1000
            x_height = 0.5
            overshoot = 10
            balance = 20 }

    let metrics = FontMetrics(axes)

    let y def =
        let pt, _, _, _, _ = parse_point metrics def
        pt.y

    let knotsOf def =
        match parse_curve metrics def false with
        | Curve(knots, _) -> List.toArray knots
        | _ -> failwith "expected a Curve"

    [<Test>]
    member this.TestGuideHeightsDoNotMove() =
        // The reference lines themselves are never nudged, whatever the balance.
        Assert.That(y "tl", Is.EqualTo(metrics.T))
        Assert.That(y "xl", Is.EqualTo(metrics.X))
        Assert.That(y "bl", Is.EqualTo(metrics.B))
        Assert.That(y "dl", Is.EqualTo(metrics.D))
        // ...nor is a repeated single guide (`tt` is still the top).
        Assert.That(y "t2l", Is.EqualTo(metrics.T))

    [<Test>]
    member this.TestBalanceRaisesMidHeights() =
        // `h` is the half height and takes the full raise (H = T/2 here).
        Assert.That(y "hl", Is.EqualTo(metrics.H + 20.0).Within(1e-9), "crossbar of H/E/F")
        // A height between two guides is raised by a sine-tapered amount:
        // bh sits a quarter up, so sin(pi/4) ~ 0.707 of the full raise.
        Assert.That(y "bhl", Is.EqualTo(metrics.H / 2.0 + 20.0 * sqrt 0.5).Within(1e-6), "crossbar of A")
        // Lowercase bars (e.g. the bar of `e`) are raised too.
        Assert.That(y "xbl", Is.GreaterThan(metrics.X / 2.0))

    [<Test>]
    member this.TestBalanceSkipsFittedAndDescenderHeights() =
        // A fitted height is the *side* extreme of a round letter (O, o) and
        // stays symmetric.
        Assert.That(y "(h)l", Is.EqualTo(metrics.H))
        Assert.That(y "(xb)l", Is.EqualTo(metrics.X / 2.0))
        // Below the baseline the raise has faded out entirely.
        Assert.That(y "bdl", Is.EqualTo(metrics.D / 2.0))

    [<Test>]
    member this.TestRoundOvershoot() =
        // The `o` bowl: flat top and bottom of the curve push past x-height and
        // the baseline, while the side extremes stay put.
        let knots = knotsOf "(xb)l~x(c)~(xb)r~b(c)~"
        Assert.That(knots.[1].pt.y, Is.EqualTo(metrics.X + 10.0).Within(1e-9), "top of the bowl overshoots")
        Assert.That(knots.[3].pt.y, Is.EqualTo(metrics.B - 10.0).Within(1e-9), "bottom of the bowl overshoots")
        Assert.That(knots.[0].pt.y, Is.EqualTo(metrics.X / 2.0), "side extreme unchanged")
        Assert.That(knots.[2].pt.y, Is.EqualTo(metrics.X / 2.0), "side extreme unchanged")

    [<Test>]
    member this.TestPointedOvershoot() =
        // The apex of `A` is a wedge, so it gets the larger pointed overshoot;
        // the feet are open-curve endpoints and stay on the baseline.
        let knots = knotsOf "bl-tc-br"
        Assert.That(knots.[1].pt.y, Is.EqualTo(metrics.T + 15.0).Within(1e-9), "apex overshoots by 1.5x")
        Assert.That(knots.[0].pt.y, Is.EqualTo(metrics.B))
        Assert.That(knots.[2].pt.y, Is.EqualTo(metrics.B))

    [<Test>]
    member this.TestFlatLettersDoNotOvershoot() =
        // `E`: every corner has a neighbour at the same height, so nothing moves.
        let knots = knotsOf "tr-tl-bl-br"
        Assert.That(knots.[1].pt.y, Is.EqualTo(metrics.T), "top left corner stays on the cap line")
        Assert.That(knots.[2].pt.y, Is.EqualTo(metrics.B), "bottom left corner stays on the baseline")

    [<Test>]
    member this.TestOnlyConvergingCornersOvershoot() =
        // `M`: the stem tops are corners between a vertical and a diagonal — not
        // points — and stay on the cap line; the middle vertex is a wedge and
        // dips below the baseline.
        let knots = knotsOf "bl-tl-blw-tw-bw"
        Assert.That(knots.[1].pt.y, Is.EqualTo(metrics.T), "stem top is not a point")
        Assert.That(knots.[3].pt.y, Is.EqualTo(metrics.T), "stem top is not a point")
        Assert.That(knots.[2].pt.y, Is.EqualTo(metrics.B - 15.0).Within(1e-9), "middle vertex is a point")

    [<Test>]
    member this.TestOpticalAxesOff() =
        // Both corrections are plain axes and can be switched off.
        let plain = FontMetrics({ axes with overshoot = 0; balance = 0 })
        let pt, _, _, _, _ = parse_point plain "hl"
        Assert.That(pt.y, Is.EqualTo(plain.H))

        match parse_curve plain "bl-tc-br" false with
        | Curve(knots, _) -> Assert.That(knots.[1].pt.y, Is.EqualTo(plain.T))
        | _ -> Assert.Fail("Expected Curve")

[<EntryPoint>]
let main argv =
    0 // Return an integer exit code