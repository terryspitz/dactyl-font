// Functional Fonts by terryspitz
// Mar 2020

module main

open System
open System.IO
open System.Text.RegularExpressions

open Generator


let charToFontForge (this: Font) (ch : char) =
    // reverse engineered from saved font  
    // TODO: coords shifted by (thickness, thickness) (hard for beziers)
    let thickness = this.Axes.thickness
    //let scpToString (scp : SCP) = sprintf "%f %f %c" scp.X scp.Y (char scp.Type)
    let scpToString (scp : SCP) = sprintf "%f %f %c" scp.X scp.Y (SpiroPointType.ToChar scp.Type)
    let spiroToFF spiro =
        //rearrange SVG bezier curve format to fontforge format
        let matchEval (amatch : Match) = amatch.Groups.[2].Value.Replace(","," ") + " "
                                         + amatch.Groups.[1].Value.ToLower() + " 0"
        let reorder s = Regex.Replace(s, "(.) (.*)", matchEval)
        let bezierString = this.toSvgBezierCurve spiro |> List.collect (fun (s:string)-> s.Split('\r', '\n') |> List.ofArray)
                           |> List.map reorder
        let spiroString =
            match spiro with
            | SpiroOpenCurve(scps, _) -> scps |> List.map scpToString
            | SpiroClosedCurve(scps, _) -> scps |> List.map scpToString
            | SpiroDot(p) -> 
                let x,y = this.getXY p 
                [
                    sprintf "%d %d o " (x-thickness) (y) +
                    sprintf "%d %d o " (x) (y+thickness) +
                    sprintf "%d %d o " (x+thickness) (y)]
            | SpiroSpace -> []
        bezierString @ ["Spiro"] @ spiroString @ ["0 0 z"; "EndSpiro";]

    let spineSpiros = Glyph(ch) |> Font({this.Axes with thickness = 2}).getSansOutlines
                      |> this.elementToSpiros |> List.collect spiroToFF
    let outlineSpiros = Glyph(ch) |> this.getOutline //|> Font({this.Axes with thickness = 2}).getSansOutlines 
                        |> this.elementToSpiros |> List.collect spiroToFF
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
    sprintf """<text x='%d' y='%d' font-size='200'>%s</text>\n""" x y text

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
        ("Dactyl Sans Extra Light", "Extra Light", Font(Axes.DefaultAxes));
        ("Dactyl Sans", "Regular", Font({Axes.DefaultAxes with outline = true; thickness = 30;}));
        ("Dactyl Sans Italic", "Italic", Font({Axes.DefaultAxes with outline = true; thickness = 30; italic = 0.15}));
        ("Dactyl Sans Bold", "Bold", Font({Axes.DefaultAxes with outline = true; thickness = 60;}));
        ("Dactyl Mono", "Regular", Font({Axes.DefaultAxes with outline = true; thickness = 30; monospace = 1.0}));
        ("Dactyl Stroked", "Regular", Font({Axes.DefaultAxes with stroked = true; thickness = 60;}));
        ("Dactyl Scratch", "Regular", Font({Axes.DefaultAxes with scratches = true; thickness = 60;}));
    ]

    //debug
    // let _, _, font = fonts.[0]
    // font.charToSvg 'O' 0 0 false |> ignore

    // SVG output, side by side
    let rowHeight = Axes.DefaultAxes.height + 400
    let svg = [for i in 0..fonts.Length-1 do
                let name, _, font = fonts.[i]
                printfn "\n%s\n" name
                let y = i*rowHeight*2
                yield svgText 0 (y+200) name
                yield! font.stringToSvg "THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789" 0 (y+rowHeight+100)
                yield! font.stringToSvg """the quick brown fox jumps OVER THE LAZY DOG !"#£$%&'()*+,-./""" 0 (y+rowHeight*2-100)
              ]

    svg |> toSvgDocument (fonts.Length * (rowHeight * 2+1)) (Axes.DefaultAxes.width * 70) |> writeFile @".\allGlyphs.svg"


    // FontForge output
    let writeFonts = true
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
                                            x_height = (11-r)*60; offset = c*30; thickness = r*6;})
                yield! font.stringToSvg str (c*600*str.Length) ((11-r)*1000)
        ] |> toSvgDocument 10 30 |> writeFile @".\interp.svg"

    0 // return code
