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
        let pt, _, label, _ = parse_point metrics "tl"
        Assert.That(label, Is.EqualTo("tl"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.False)
        Assert.That(pt.x_fit, Is.False)

    [<Test>]
    member this.TestOptionalY() =
        // "(t)l" -> y="t" optional, x="l" fixed
        let pt, _, label, _ = parse_point metrics "(t)l"
        Assert.That(label, Is.EqualTo("(t)l"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.True, "y should be fit")
        Assert.That(pt.x_fit, Is.False, "x should not be fit")

    [<Test>]
    member this.TestOptionalX() =
        // "t(l)" -> y="t" fixed, x="l" optional
        let pt, _, label, _ = parse_point metrics "t(l)"
        Assert.That(label, Is.EqualTo("t(l)"))

        Assert.That(pt.y, Is.EqualTo(metrics.T))
        Assert.That(pt.x, Is.EqualTo(metrics.L))
        Assert.That(pt.y_fit, Is.False, "y should not be fit")
        Assert.That(pt.x_fit, Is.True, "x should be fit")

    [<Test>]
    member this.TestBothOptional() =
        // "(t)(l)" -> both optional
        let pt, _, label, _ = parse_point metrics "(t)(l)"
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

[<EntryPoint>]
let main argv =
    0 // Return an integer exit code