// Functional Fonts by terryspitz
// Mar 2020

module Main

open System
open System.IO

open Axes
open Generator
open FontForge
open DactylSpline

open Curves


let writeFile filename text = 
    let trim (x : string) = x.Trim()
    let trimmedText = List.map trim text
    printfn "Writing %s" filename
    File.WriteAllLines(filename, trimmedText) |> ignore

[<EntryPoint>]
let main argv =

    let runUnitTest = false
    if runUnitTest then
        printfn "Test:CheckLinesCornerCorner"
        DactylSplineTest.TestClass().CheckLinesCornerCorner()
        printfn "Test:CheckLinesCornerSmooth"
        DactylSplineTest.TestClass().CheckLinesCornerSmooth()
        printfn "Test:CheckLinesSmoothCorner"
        DactylSplineTest.TestClass().CheckLinesSmoothCorner()
        printfn "Test:CheckLinesSmoothSmooth"
        DactylSplineTest.TestClass().CheckLinesSmoothSmooth()
        printfn "Test:CheckTwoPointCurvesWithAlignedTangents"
        DactylSplineTest.TestClass().CheckTwoPointCurvesWithAlignedTangents()
        printfn "Test:CheckTwoPointCurvesWithOtherTangents"
        DactylSplineTest.TestClass().CheckTwoPointCurvesWithOtherTangents()

    else
        if not (Directory.Exists @"output") then
            Directory.CreateDirectory @"output" |> printfn "%A"

        let dactylSplineVisualTests = true
        if dactylSplineVisualTests then
            VisualTests.splineStaticPage() 
            |> toHtmlDocument 0 0 10 12
            |> writeFile @"output\visualTests.html"

        let debugSingleChar = true
        if debugSingleChar then
            // let font = Font(Axes.DefaultAxes)
            let font = Font({Axes.DefaultAxes with dactyl_spline=true; debug=true})
            for svg in font.charToSvg 'o' 0 0 black do
                printfn "%s" svg

        let fonts = [
            // ("Dactyl Knots", "Extra Light", Font({Axes.DefaultAxes with show_knots = true}))
            // ("Dactyl Spiro", "Extra Light", Font({Axes.DefaultAxes with spline2 = false}))
            ("Dactyl Sans Extra Light", "Extra Light", Font({Axes.DefaultAxes with thickness = 3}))
            ("Dactyl Sans", "Regular", Font({Axes.DefaultAxes with thickness = 30; show_knots=true}))
            ("Dactyl Sans Italic", "Italic", Font({Axes.DefaultAxes with italic = 0.15}))
            ("Dactyl Sans Bold", "Bold", Font({Axes.DefaultAxes with thickness = 60}))
            ("Dactyl Round", "Round", Font({Axes.DefaultAxes with end_bulb = 0.5; axis_align_caps = false; thickness=90}))
            ("Dactyl Mono", "Regular", Font({Axes.DefaultAxes with monospace = 1.0}))
            ("Dactyl Stroked", "Regular", Font({Axes.DefaultAxes with stroked = true; thickness = 60;}))
            ("Dactyl Scratch", "Regular", Font({Axes.DefaultAxes with scratches = true; thickness = 60;}))
            ("Dactyl Roman", "Regular", Font({Axes.DefaultAxes with serif=30}))
            ("Dactyl Smooth", "Regular", Font({Axes.DefaultAxes with spline2=true; smooth=true}))
        ]

        let allGlyphs = true
        if allGlyphs then
            // SVG output, side by side
            let rowHeights = List.scan (+) 0 [
                for i in 0..fonts.Length-1 do
                    let _, _, font = fonts.[i] in (200 + font.charHeight * 2)]
            // let text = ["THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789"
            //             """the quick brown fox jumps OVER THE LAZY DOG !"#£$%&'()*+,-./"""]
            let text = ["abcdefghijklmnopqrstuvwxyz 0123456789"
                        """ABCDEFGHIJKLMNOPQRSTUVWXYZ !"#£$%&'()*+,-./"""]
            [for i in 0..fonts.Length-1 do
                let name, _, font = fonts.[i]
                printfn "\n%s\n" name
                yield sprintf "<g id='%s%d'>" name i
                let y = rowHeights.[i]
                yield svgText 0 (y+200) name
                yield! font.stringToSvgLines text 0 (y+400) black
                yield "</g>"
            ] |> toHtmlDocument 0 0 (Axes.DefaultAxes.width * 70) (List.max rowHeights)
              |> writeFile @"output\allGlyphs.html"


        // Proofs output using https://www.typography.com/blog/text-for-proofing-fonts
        let outputProofs = false
        if outputProofs then
            printfn "\nProofs\n" 
            let proofDir = @".\proofs\text files\design\alphabet (latin)\"
            let splitWords (s:string)= s.Split('\n','\r') |> Seq.filter (fun w -> w.Length > 0 && w.[0] <> '#')
                                       |> Seq.collect (fun s -> s.Split(' ')) |> List.ofSeq
            let lowercase = File.ReadAllText (proofDir + "lowercase (latin).txt") |> splitWords
            // let lowercaseLengths = List.map (fun (w : string) -> w.Length) lowercase
            // let uppercase = File.ReadAllText (proofDir + "uppercase (latin).txt") |> splitWords
            let targetWidth = 20000
            for i in 0..fonts.Length-1 do
                let name, _, font = fonts.[i]
                let charsPerLine = targetWidth / Axes.DefaultAxes.width
                let wrap lines (w : string) =
                    let lineLength = List.fold (fun len (word : string) -> len + word.Length + 1) 0
                    match lines with
                    | head::tail -> if (w.Length + lineLength head) < charsPerLine then
                                        (head @ [w]) :: tail
                                    else
                                        [w] :: head :: tail
                    | [] -> [[w]]
                let lines = name :: (List.fold wrap [] lowercase |> List.map (String.concat " ") |> List.rev)
                printfn "\n%s\n" name
                font.stringToSvg lines 0 0 true black |> writeFile (sprintf @"output\proofs %s lower.svg" name)
        
        // FontForge output
        let writeFonts = false
        if writeFonts then
            for i in 0..fonts.Length-1 do
                let name, weight, font = fonts.[i]
                let dir = sprintf @"output\fontforge\%s.sfdir" name
                printfn "Writing font to %s" dir
                if not (Directory.Exists dir) then
                    Directory.CreateDirectory dir |> printfn "%A"
                writeFile (dir + @"\font.props") (fontForgeProps name weight)
                let allChars = ['A'..'Z'] @ ['a'..'z'] @ ['0'..'9'] @ List.ofSeq """!"#£$%&'()*+,-./"""
                for ch in allChars do 
                    charToFontForge font ch |> writeFile (sprintf @"%s\%s.glyph" dir (fontForgeGlyphFile ch))

        // Interpolate along font variable axes as in https://levien.com/spiro/s_interp2.png
        let outputInterpolatedStr = false
        if outputInterpolatedStr then
            let str = "dog"
            [
                for r in 1..10 do
                for c in 1..10 do
                    let font = Font({Axes.DefaultAxes with 
                                                x_height = float (11-r) * 0.2; roundedness = c*30; thickness = r*6;})
                    yield! font.stringToSvg [str] 0 0 true black
            ] |> writeFile @"output\interp.svg"

    0 // return code
