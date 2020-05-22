// Functional Fonts by terryspitz
// Mar 2020

module main

open System
open System.IO
open System.Text.RegularExpressions

open Generator


let charToFontForge (this: Font) (ch : char) =
    // reverse engineered from saved font  
    let thickness = this.axes.thickness
    //let scpToString (scp : SCP) = sprintf "%f %f %c" scp.X scp.Y (char scp.Type)
    let scpToString (scp : SCP) = sprintf "%f %f %c" scp.X scp.Y (SpiroPointType.ToChar scp.Type)
    let spiroToFF spiro =
        //rearrange SVG bezier curve format to fontforge format
        let matchEval (amatch : Match) = amatch.Groups.[2].Value.Replace(","," ") + " "
                                         + amatch.Groups.[1].Value.ToLower() + " 0"
        let reorder s = Regex.Replace(s, "(.) (.*)", matchEval)
        let bezierString = this.spiroToSvg spiro |> List.collect (fun (s:string)-> s.Split('\r', '\n') |> List.ofArray)
                           |> List.map reorder
        let spiroString =
            match spiro with
            | SpiroOpenCurve(scps, _) -> scps |> List.map scpToString
            | SpiroClosedCurve(scps, _) -> scps |> List.map scpToString
            | SpiroDot(p) -> 
                let x,y = this.GetXY p 
                [
                    sprintf "%d %d o " (x-thickness) (y) +
                    sprintf "%d %d o " (x) (y+thickness) +
                    sprintf "%d %d o " (x+thickness) (y)]
            | SpiroSpace -> []
        bezierString @ ["Spiro"] @ spiroString @ ["0 0 z"; "EndSpiro";]

    let spineSpiros = Glyph(ch) |> Font({this.axes with thickness = 2}).getSansOutlines
                      |> this.ElementToSpiros |> List.collect spiroToFF
    let outlineSpiros = Glyph(ch) |> this.getOutline
                        |> this.ElementToSpiros |> List.collect spiroToFF
    [
        sprintf "StartChar: %c\n" ch;
        sprintf "Encoding: %d %d 0\n" (int ch) (int ch);
        sprintf "Width: %d\n" (this.charWidth ch + thickness);
        """
        InSpiro: 1
        Flags: H
        LayerCount: 2
        Back
        SplineSet
        """;
    ] @ spineSpiros @ [
        """
        EndSplineSet
        Fore
        SplineSet
        """
    ] @ outlineSpiros @ [
        """
        EndSplineSet
        EndChar
        """;
    ]

let fontForgeProps name weight =
    let props = File.ReadAllText @".\generator\font.props"
    [
        "SplineFontDB: 3.2";
        "FamilyName: Dactyl";
        sprintf "FontName: %s" name
        sprintf "FullName: %s" name
        sprintf "Weight: %s" weight
    ] @ [props]//(props.Split('\r','\n') |> List.ofArray)

let fontForgeGlyphFile ch =
    match(ch) with
    | ch when ch >= 'A' && ch <='Z' -> sprintf "_%c" ch  //Capitals have underscore prefix
    | ch when ch >= 'a' && ch <='z' -> string ch
    | ch when ch >= '0' && ch <='9' -> string ch
    // names from https://github.com/adobe-type-tools/agl-aglfn/blob/master/glyphlist.txt
    | '!' -> "exclam"
    | '"' -> "quotedbl"
    | '#' -> "numbersign"
    | '£' -> "sterling"
    | '$' -> "dollar"
    | '%' -> "percent"
    | '&' -> "ampersand"
    | ''' -> "quotesingle"
    | '(' -> "parenleft"
    | ')' -> "parenright"
    | '*' -> "asterisk"
    | '+' -> "plus"
    | ',' -> "comma"
    | '-' -> "hyphen"
    | '.' -> "period"
    | '/' -> "slash"
    | ':' -> "colon"
    | ';' -> "semicolon"
    //| '' -> ""
    | _ -> invalidArg "ch" (sprintf "Unknown char %c" ch)

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
        ("Dactyl Sans Extra Light", "Extra Light", Font({Axes.DefaultAxes with thickness = 3;}));
        ("Dactyl Sans", "Regular", Font({Axes.DefaultAxes with thickness = 30;}));
        ("Dactyl Sans Italic", "Italic", Font({Axes.DefaultAxes with thickness = 30; italic = 0.15}));
        ("Dactyl Sans Bold", "Bold", Font({Axes.DefaultAxes with thickness = 60;}));
        ("Dactyl Mono", "Regular", Font({Axes.DefaultAxes with thickness = 30; monospace = 1.0}));
        ("Dactyl Stroked", "Regular", Font({Axes.DefaultAxes with stroked = true; thickness = 60;}));
        ("Dactyl Scratch", "Regular", Font({Axes.DefaultAxes with scratches = true; thickness = 60;}));
    ]

    //debug
    // let _, _, font = fonts.[0]
    // font.charToSvg 'O' 0 0 false |> ignore

    // SVG output, side by side
    let rowHeights = List.scan (+) 0 [for i in 0..fonts.Length-1 do let _, _, font = fonts.[i] in (200 + font.charHeight * 2)]
    let text = "THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789\n" +
                """the quick brown fox jumps OVER THE LAZY DOG !"#£$%&'()*+,-./""" 
    [for i in 0..fonts.Length-1 do
        let name, _, font = fonts.[i]
        printfn "\n%s\n" name
        let y = rowHeights.[i]
        yield svgText 0 (y+200) name
        yield! font.stringToSvgLines text 0 (y+400)
    ] |> toSvgDocument (List.max rowHeights) (Axes.DefaultAxes.width * 70) |> writeFile @".\allGlyphs.svg"


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
                yield! font.stringToSvg str 0 0
        ] |> writeFile @".\interp.svg"

    0 // return code
