// Functional Fonts by terryspitz
// Mar 2020

open Generator

let charToFontForge (this: Font) (ch : char) =
    // reverse engineered from saved font  
    // TODO: coords shifted by (thickness, thickness) (hard for beziers)
    let thickness = this.Axes.thickness
    let scpToString (scp : SCP) = sprintf "%f %f %c" scp.X scp.Y (char scp.Type)
    let spiroToFF spiro =
        //rearrange SVG bezier curve format to fontforge format
        let matchEval (amatch : Match) = amatch.Groups.[2].Value.Replace(","," ") + " "
                                         + amatch.Groups.[1].Value.ToLower() + " 0"
        let reorder s = Regex.Replace(s, "(.) (.*)", matchEval)
        let bezierString = this.toSvgBezierCurve spiro |> fun s-> s.Split("\r\n") 
                           |> Array.map reorder |> String.concat "\n"
        let spiroString =
            match spiro with
            | SpiroOpenCurve(scps, _) -> scps |> List.map scpToString |> concatLines
            | SpiroClosedCurve(scps, _) -> scps |> List.map scpToString |> concatLines
            | SpiroDot(p) -> let x,y = this.getXY true p 
                             sprintf "%d %d o " (x-thickness) (y) +
                             sprintf "%d %d o " (x) (y+thickness) +
                             sprintf "%d %d o " (x+thickness) (y)
            | SpiroSpace -> ""
        sprintf """
                %s
                Spiro
                %s
                0 0 z
                EndSpiro
                """ bezierString spiroString
    let spineSpiros = Glyph(ch) |> Font({this.Axes with thickness = 2}).getSansOutlines
                      |> this.elementToSpirosOffset true |> List.map spiroToFF |> concatLines
    let outlineSpiros = Glyph(ch) |> this.getOutline //|> Font({this.Axes with thickness = 2}).getSansOutlines 
                        |> this.elementToSpiros |> List.map spiroToFF |> concatLines
    sprintf "StartChar: %c\n" ch +
    sprintf "Encoding: %d %d 0\n" (int ch) (int ch) +
    sprintf "Width: %d\n" (this.width (Glyph(ch)) + thickness) +
    sprintf """
            InSpiro: 1
            Flags: H
            LayerCount: 2
            Back
            SplineSet
                %s
            EndSplineSet
            Fore
            SplineSet
                %s
            EndSplineSet
            EndChar
            """
            spineSpiros outlineSpiros

let fontForgeProps name weight =
    let props = File.ReadAllText @".\font.props"
    sprintf
        """SplineFontDB: 3.2
         FamilyName: Dactyl
         FontName: %s
         FullName: %s
         Weight: %s
         %s""" name name weight props

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
    sprintf """<svg xmlns='http://www.w3.org/2000/svg'
            viewBox='0 0 %d %d'>
            <g id='layer1'>
            %s
            </g>
            </svg>""" width height svg

//end module


let writeFile filename (text : string) = 
    let trim (x : string) = x.Trim()
    let trimmedText = text.Split("\n") |> Array.map trim |> String.concat "\n"
    printfn "Writing %s" filename
    File.WriteAllText(filename, trimmedText) |> ignore

[<EntryPoint>]
let main argv =
    let showKnots = false
    let fonts = [
        ("Dactyl Sans Extra Light", "Extra Light", Font(Axes.DefaultAxes));
        ("Dactyl Sans", "Regular", Font({Axes.DefaultAxes with outline = true; thickness = 30;}));
        ("Dactyl Sans Italic", "Italic", Font({Axes.DefaultAxes with outline = true; thickness = 30; italic_fraction = 0.15}));
        ("Dactyl Sans Bold", "Bold", Font({Axes.DefaultAxes with outline = true; thickness = 60;}));
        ("Dactyl Stroked", "Regular", Font({Axes.DefaultAxes with stroked = true; thickness = 60;}));
        ("Dactyl Scratch", "Regular", Font({Axes.DefaultAxes with scratches = true; thickness = 60;}));
    ]
    // SVG output, side by side
    let rowHeight = Axes.DefaultAxes.height + 400
    let svg = [for i in 0..fonts.Length-1 do
                let name, _, font = fonts.[i]
                let y = i*rowHeight*2
                svgText 0 (y+200) name +
                font.stringToSvg "THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789" 0 (y+rowHeight+100) showKnots +
                font.stringToSvg """the quick brown fox jumps OVER THE LAZY DOG !"#£$%&'()*+,-./""" 0 (y+rowHeight*2-100) showKnots
              ] |> concatLines

    writeFile @".\allGlyphs.svg" (toSvgDocument (fonts.Length * (rowHeight * 2+1)) (Axes.DefaultAxes.width * 70) svg)


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
                font.charToFontForge ch |> writeFile (sprintf @"%s\%s.glyph" dir (fontForgeGlyphFile ch))

    // Interpolate along font variable axes as in https://levien.com/spiro/s_interp2.png
    let outputInterpolatedStr = false
    if outputInterpolatedStr then
        let str = "dog"
        [
            for r in 1..10 do
            for c in 1..10 do
                let font = Font({Axes.DefaultAxes with 
                                            x_height = (11-r)*60; offset = c*30; thickness = r*6;})
                font.stringToSvg str (c*600*str.Length) ((11-r)*1000) false
        ] |> String.concat "\n" |> toSvgDocument 10 30 |> writeFile @".\interp.svg"

    0 // return code
