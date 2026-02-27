module ParserTests

open NUnit.Framework
open Axes
open GlyphFsDefs
open GlyphStringDefs
open GeneratorTypes
open SpiroPointType

[<TestFixture>]
type ParserTests() =
    let axes =
        { Axes.Axes.DefaultAxes with
            width = 1000
            height = 1000
            x_height = 0.5
            debug = true }

    let glyph = GlyphFsDefs(axes)

    [<Test>]
    member this.TestBasic() =
        // "tl" -> y="t", x="l"
        let pt, _, _ = parse_point glyph "tl"

        match pt with
        | YX(y, x) ->
            Assert.That(y, Is.EqualTo(glyph._T))
            Assert.That(x, Is.EqualTo(glyph._L))
        | _ -> Assert.Fail("Expected YX")

    [<Test>]
    member this.TestOptionalY() =
        // "(t)l" -> y="t" optional, x="l" fixed
        let pt, _, _ = parse_point glyph "(t)l"

        match pt with
        | OYX(y, x, yfit, xfit) ->
            Assert.That(y, Is.EqualTo(glyph._T))
            Assert.That(x, Is.EqualTo(glyph._L))
            Assert.That(yfit, Is.True, "y should be fit")
            Assert.That(xfit, Is.False, "x should not be fit")
        | _ -> Assert.Fail("Expected OYX")

    [<Test>]
    member this.TestOptionalX() =
        // "t(l)" -> y="t" fixed, x="l" optional
        let pt, _, _ = parse_point glyph "t(l)"

        match pt with
        | OYX(y, x, yfit, xfit) ->
            Assert.That(y, Is.EqualTo(glyph._T))
            Assert.That(x, Is.EqualTo(glyph._L))
            Assert.That(yfit, Is.False, "y should not be fit")
            Assert.That(xfit, Is.True, "x should be fit")
        | _ -> Assert.Fail("Expected OYX for t(l)")

    [<Test>]
    member this.TestBothOptional() =
        // "(t)(l)" -> both optional
        let pt, _, _ = parse_point glyph "(t)(l)"

        match pt with
        | OYX(y, x, yfit, xfit) ->
            Assert.That(yfit, Is.True)
            Assert.That(xfit, Is.True)
        | _ -> Assert.Fail("Expected OYX")

    [<Test>]
    member this.TestDotSeparator() =
        // "tl.tl" -> should result in TWO points at tl, separated by corner
        let elem = parse_curve glyph "tl.tl" false

        match elem with
        | TangentCurve(pts, isClosed) ->
            Assert.That(pts.Length, Is.EqualTo(2), "Should have 2 points")
            let (p1, ty1, th1) = pts.[0]
            let (p2, ty2, th2) = pts.[1]
            Assert.That(p1, Is.EqualTo(p2), "Points should be coincident")
            Assert.That(ty1, Is.EqualTo(SpiroPointType.Corner), "First point should be Corner")
        | _ -> Assert.Fail("Expected TangentCurve")

    [<Test>]
    member this.TestTangentDot() =
        // "blE.bl" -> Coincident points. First has Tangent E. Second has None.
        let elem = parse_curve glyph "blE.bl" false

        match elem with
        | TangentCurve(pts, isClosed) ->
            Assert.That(pts.Length, Is.EqualTo(2))
            let (p1, ty1, th1) = pts.[0]
            let (p2, ty2, th2) = pts.[1]
            Assert.That(p1, Is.EqualTo(p2))
            Assert.That(th1, Is.Not.Null, "First point should have tangent")
            Assert.That(th2, Is.EqualTo(None), "Second point should be None")
        | _ -> Assert.Fail("Expected TangentCurve")
