// Functional Fonts by terryspitz
// Mar 2020

module Main

open System
open System.IO

open Generator
open FontForge


let svgText x y text =
    sprintf "<text x='%d' y='%d' font-size='200'>%s</text>" x y text

let toSvgDocument height width svg =
    [
        "<svg xmlns='http://www.w3.org/2000/svg'";
        sprintf "viewBox='0 0 %d %d'>" width height;
        "<g id='layer1'>";
    ] @ svg @ [
        "</g>";
        "</svg>";
    ]


let writeFile filename text = 
    let trim (x : string) = x.Trim()
    let trimmedText = List.map trim text
    printfn "Writing %s" filename
    File.WriteAllLines(filename, trimmedText) |> ignore

[<EntryPoint>]
let main argv =

    let fonts = [
        // ("Dactyl Knots", "Extra Light", Font({Axes.DefaultAxes with show_knots = true}))
        ("Dactyl Sans Extra Light", "Extra Light", Font({Axes.DefaultAxes with thickness = 3}))
        ("Dactyl Sans", "Regular", Font({Axes.DefaultAxes with thickness = 30}))
        ("Dactyl Sans Italic", "Italic", Font({Axes.DefaultAxes with thickness = 30; italic = 0.15}))
        ("Dactyl Sans Bold", "Bold", Font({Axes.DefaultAxes with thickness = 60}))
        ("Dactyl Mono", "Regular", Font({Axes.DefaultAxes with thickness = 30; monospace = 1.0}))
        ("Dactyl Stroked", "Regular", Font({Axes.DefaultAxes with stroked = true; thickness = 60;}))
        ("Dactyl Scratch", "Regular", Font({Axes.DefaultAxes with scratches = true; thickness = 60;}))
        ("Dactyl Roman", "Regular", Font({Axes.DefaultAxes with thickness = 30; serif=30}))
    ]

    //debug
    // let _, _, font = fonts.[0]
    // font.charToSvg 'O' 0 0 false |> ignore

    // SVG output, side by side
    let rowHeights = List.scan (+) 0 [for i in 0..fonts.Length-1 do let _, _, font = fonts.[i] in (200 + font.charHeight * 2)]
    let text = ["THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789"
                """the quick brown fox jumps OVER THE LAZY DOG !"#£$%&'()*+,-./"""]
    // let text = ["3"]
    [for i in 0..fonts.Length-1 do
        let name, _, font = fonts.[i]
        printfn "\n%s\n" name
        let y = rowHeights.[i]
        yield svgText 0 (y+200) name
        yield! font.stringToSvgLines text 0 (y+400)
    ] |> toSvgDocument (List.max rowHeights) (Axes.DefaultAxes.width * 70) |> writeFile @".\allGlyphs.svg"


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
            font.stringToSvg lines 0 0 |> writeFile (sprintf @".\svg\%s_lower.svg" name)
    
    // FontForge output
    let writeFonts = false
    if writeFonts then
        for i in 0..fonts.Length-1 do
            let name, weight, font = fonts.[i]
            let dir = sprintf @".\fontforge\%s.sfdir" name
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
                                            x_height = (11-r)*60; roundedness = c*30; thickness = r*6;})
                yield! font.stringToSvg [str] 0 0
        ] |> writeFile @".\interp.svg"

    0 // return code
