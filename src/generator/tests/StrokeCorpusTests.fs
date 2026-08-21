module StrokeCorpusTests

open NUnit.Framework
open Axes
open GeneratorTypes

/// The stroke corpus in StrokeCorpus.fs is generated offline by
/// tools/randomglyphs/emit_corpus.py from the Hershey simplex faces.  These
/// tests are the round-trip gate on that generator: every harvested stroke must
/// still be readable by the parser it was written for.
///
/// This matters because `rawDefToElem` swallows every exception and hands back a
/// `Dot`, so a malformed definition fails *silently* -- it renders as a speck
/// rather than throwing.  A corpus quietly full of specks would look like a
/// generator bug much later on, so "came back as a Dot" is treated as failure
/// here rather than success.
[<TestFixture>]
type StrokeCorpusTests() =
    let metrics = FontMetrics(Axes.DefaultAxes)

    [<Test>]
    member _.``corpus is non-empty and covers every role``() =
        Assert.That(StrokeCorpus.strokes, Is.Not.Empty)
        let roles = StrokeCorpus.strokes |> List.map (fun (r, _, _) -> r) |> List.distinct |> List.sort
        let expected = [ "arc"; "bar"; "bowl"; "diag"; "dot"; "stem" ]
        Assert.That(
            (roles = expected),
            sprintf "generated corpus should carry all six geometric roles, got %A" roles
        )

    [<Test>]
    member _.``every harvested stroke parses to a real element``() =
        let failures =
            [ for (role, source, def) in StrokeCorpus.strokes do
                  let degenerate =
                      try
                          // A single-point `dot` is legitimately a Dot; anything else
                          // coming back as one means the parser rejected the string.
                          match GlyphStringDefs.rawDefToElem metrics def false with
                          | Dot _ -> role <> "dot"
                          | _ -> false
                      with _ -> true

                  if degenerate then yield sprintf "%s/%s: %s" source role def ]

        Assert.That(failures, Is.Empty, sprintf "%d corpus strokes failed to parse" failures.Length)

    [<Test>]
    member _.``role patterns are non-empty and use known roles``() =
        Assert.That(StrokeCorpus.rolePatterns, Is.Not.Empty)
        let known = StrokeCorpus.strokes |> List.map (fun (r, _, _) -> r) |> Set.ofList

        let unknown =
            StrokeCorpus.rolePatterns
            |> List.collect id
            |> List.distinct
            |> List.filter (fun r -> not (known.Contains r))

        Assert.That(unknown, Is.Empty, "role patterns should only name roles present in the corpus")
        Assert.That((StrokeCorpus.rolePatterns |> List.forall (List.isEmpty >> not)), "no role pattern should be empty")
