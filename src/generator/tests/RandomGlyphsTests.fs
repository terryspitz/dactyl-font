module RandomGlyphsTests

open NUnit.Framework
open Axes
open GeneratorTypes

/// Tests for the runtime Propose -> Filter -> Assemble generator
/// (RandomGlyphs.fs). See docs/RandomGlyphs.md for the design and the
/// measurements these guard against regressing.
[<TestFixture>]
type RandomGlyphsTests() =

    [<Test>]
    member _.``generates the requested count under default axes``() =
        let defs = RandomGlyphs.generateAlphabetDefs Axes.DefaultAxes 12345 26
        Assert.That(defs.Length, Is.EqualTo(26), "should reach the requested count within the try budget")

    [<Test>]
    member _.``same seed is deterministic``() =
        let a = RandomGlyphs.generateAlphabetDefs Axes.DefaultAxes 777 20
        let b = RandomGlyphs.generateAlphabetDefs Axes.DefaultAxes 777 20
        Assert.That((a = b), "same seed should produce the same alphabet")

    [<Test>]
    member _.``different seeds produce different alphabets``() =
        let a = RandomGlyphs.generateAlphabetDefs Axes.DefaultAxes 1 20
        let b = RandomGlyphs.generateAlphabetDefs Axes.DefaultAxes 2 20
        Assert.That((a <> b), "different seeds should produce different alphabets")

    [<Test>]
    member _.``every generated glyph parses to a real element, not a Dot``() =
        let metrics = FontMetrics(Axes.DefaultAxes)
        let defs = RandomGlyphs.generateAlphabetDefs Axes.DefaultAxes 555 30

        let degenerate =
            defs
            |> List.filter (fun def ->
                try
                    match GlyphStringDefs.rawDefToElem metrics def false with
                    | Dot _ -> true
                    | _ -> false
                with _ -> true)

        Assert.That(degenerate, Is.Empty, sprintf "%d/%d generated glyphs came back degenerate" degenerate.Length defs.Length)

    [<Test>]
    member _.``generated glyphs within an alphabet are mostly distinct strings``() =
        let defs = RandomGlyphs.generateAlphabetDefs Axes.DefaultAxes 42 26
        let unique = defs |> List.distinct |> List.length
        // The distinctiveness filter compares decoded skeletons, not raw strings, so
        // a handful of coincidental exact-string repeats is expected -- but most of
        // an alphabet should still be textually unique.
        Assert.That(unique, Is.GreaterThanOrEqualTo(defs.Length * 3 / 4))

    [<Test>]
    member _.``scales to non-default axes without throwing``() =
        let axes = { Axes.DefaultAxes with width = 500; height = 900; x_height = 0.5 }
        let metrics = FontMetrics(axes)
        let defs = RandomGlyphs.generateAlphabetDefs axes 9 15
        Assert.That(defs, Is.Not.Empty)

        for def in defs do
            let elem = GlyphStringDefs.rawDefToElem metrics def false

            match elem with
            | Dot _ -> Assert.Fail(sprintf "degenerate at non-default axes: %s" def)
            | _ -> ()

    [<Test>]
    member _.``empty request returns no glyphs``() =
        let defs = RandomGlyphs.generateAlphabetDefs Axes.DefaultAxes 1 0
        Assert.That(defs, Is.Empty)
