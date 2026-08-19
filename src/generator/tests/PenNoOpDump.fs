/// A harness for proving that a change to the stroke renderer is a no-op.
///
/// Renders every glyph across a spread of artistic axis settings and writes one
/// line of SVG per (variant, glyph) to a file.  Run it on two commits and diff
/// the output: identical files mean the change cannot have moved any outline,
/// which is a far stronger check than the unit tests alone and does not need the
/// visual snapshots regenerated.
///
///     DUMP_OUT=/tmp/before.txt dotnet test src/generator/tests/generator.tests.fsproj \
///         --filter "FullyQualifiedName~DumpAll"
///
/// Explicit, so it never runs in CI.
module PenNoOpDump

open NUnit.Framework
open Axes
open Font

[<TestFixture>]
type PenNoOpDumpTests() =

    [<Test; Explicit("Diagnostic: dump every glyph across artistic axis settings for no-op comparison")>]
    member _.DumpAll() =
        let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,!?()@&"
        let variants : (string * (Axes -> Axes)) list =
            [ "default",   id
              "nib",       (fun a -> { a with nib = 0.7; nib_angle = 40 })
              "taper",     (fun a -> { a with taper = 0.6; taper_end = 0.2 })
              "taper0",    (fun a -> { a with taper = 0.8; taper_end = 0.0 })
              "wobble",    (fun a -> { a with wobble = 0.5 })
              "rough",     (fun a -> { a with roughness = 0.6 })
              "mobius",    (fun a -> { a with mobius = 1.5 })
              "jointgap",  (fun a -> { a with joint_gap = 0.5 })
              "serifflare",(fun a -> { a with serif = 40; flare = 0.5; end_bulb = 0.4 })
              "combo",     (fun a -> { a with nib = 0.5; taper = 0.4; wobble = 0.3; roughness = 0.3 })
              "italic",    (fun a -> { a with slant = 0.3; nib = 0.4 })
              "soft",      (fun a -> { a with softness = 0.7; contrast = 0.3 }) ]
        let sb = System.Text.StringBuilder()
        for (name, f) in variants do
            let axes = f Axes.DefaultAxes
            let font = Font(axes)
            for ch in chars do
                let svg =
                    try font.charToSvg ch 0.0 0.0 "black" |> String.concat " "
                    with e -> "EX:" + e.Message
                sb.AppendLine(sprintf "%s\t%c\t%s" name ch svg) |> ignore
        let out =
            match System.Environment.GetEnvironmentVariable("DUMP_OUT") with
            | null | "" -> System.IO.Path.Combine(System.IO.Path.GetTempPath(), "dactyl-glyph-dump.txt")
            | path -> path
        System.IO.File.WriteAllText(out, sb.ToString())
        printfn "Wrote %d glyph renderings to %s" (chars.Length * variants.Length) out
