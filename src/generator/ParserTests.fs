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

    let glyph = FontMetrics(axes)

    [<Test>]
    member this.TestBasic() =
        // "tl" -> y="t", x="l"
        let pt, _, _ = parse_point glyph "tl"

        Assert.That(pt.y, Is.EqualTo(glyph.T))
        Assert.That(pt.x, Is.EqualTo(glyph.L))
        Assert.That(pt.y_fit, Is.False)
        Assert.That(pt.x_fit, Is.False)

    [<Test>]
    member this.TestOptionalY() =
        // "(t)l" -> y="t" optional, x="l" fixed
        let pt, _, _ = parse_point glyph "(t)l"

        Assert.That(pt.y, Is.EqualTo(glyph.T))
        Assert.That(pt.x, Is.EqualTo(glyph.L))
        Assert.That(pt.y_fit, Is.True, "y should be fit")
        Assert.That(pt.x_fit, Is.False, "x should not be fit")

    [<Test>]
    member this.TestOptionalX() =
        // "t(l)" -> y="t" fixed, x="l" optional
        let pt, _, _ = parse_point glyph "t(l)"

        Assert.That(pt.y, Is.EqualTo(glyph.T))
        Assert.That(pt.x, Is.EqualTo(glyph.L))
        Assert.That(pt.y_fit, Is.False, "y should not be fit")
        Assert.That(pt.x_fit, Is.True, "x should be fit")

    [<Test>]
    member this.TestBothOptional() =
        // "(t)(l)" -> both optional
        let pt, _, _ = parse_point glyph "(t)(l)"

        Assert.That(pt.y_fit, Is.True)
        Assert.That(pt.x_fit, Is.True)

    [<Test>]
    member this.TestDotSeparator() =
        // "tl.tl" -> should result in TWO points at tl, separated by corner
        let elem = parse_curve glyph "tl.tl" false

        match elem with
        | Curve(pts, isClosed) ->
            Assert.That(pts.Length, Is.EqualTo(2), "Should have 2 points")
            let k1 = pts.[0]
            let k2 = pts.[1]
            Assert.That(k1.pt, Is.EqualTo(k2.pt), "Points should be coincident")
            Assert.That(k1.ty, Is.EqualTo(SpiroPointType.Corner), "First point should be Corner")
        | _ -> Assert.Fail("Expected Curve")

    [<Test>]
    member this.TestTangentDot() =
        // "blE.bl" -> Coincident points. First has Tangent E. Second has None.
        let elem = parse_curve glyph "blE.bl" false

        match elem with
        | Curve(pts, isClosed) ->
            Assert.That(pts.Length, Is.EqualTo(2))
            let k1 = pts.[0]
            let k2 = pts.[1]
            Assert.That(k1.pt, Is.EqualTo(k2.pt))
            Assert.That(k1.th_in, Is.Not.Null, "First point should have th_in tangent")
            Assert.That(k1.th_out, Is.Not.Null, "First point should have th_out tangent")
            Assert.That(k2.th_in, Is.EqualTo(None))
            Assert.That(k2.th_out, Is.EqualTo(None))
        | _ -> Assert.Fail("Expected Curve")
