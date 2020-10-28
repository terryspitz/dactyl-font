module FontForge

open System.IO
open System.Text.RegularExpressions

open GeneratorTypes
open Generator


let charToFontForge (font: Font) (ch : char) =
    // reverse engineered from saved font
    let thickness = font.axes.thickness
    //let scpToString (scp : SCP) = sprintf "%f %f %c" scp.X scp.Y (char scp.Type)
    let ptToString (p, t) =  let x,y = font.GetXY p in sprintf "%d %d %c" x y (SpiroPointType.ToChar t)
    let elemToFF elem =
        //rearrange SVG bezier curve format to fontforge format
        let matchEval (amatch : Match) = 
            sprintf "%s %s 0" (amatch.Groups.[2].Value.Replace(","," ")) (amatch.Groups.[1].Value.ToLower())
        let reorder s = Regex.Replace(s, "(.) (.*)", matchEval)
        let split (s : string) = s.Split([|'\r'; '\n'|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        let bezierString spiro = font.spiroToSvg spiro
                                    |> List.collect split
                                    |> List.map reorder
        let rec toSpiroString elem =
            match elem with
            | OpenCurve(pts) | ClosedCurve(pts) -> List.map ptToString pts
            | EList(elems) -> List.collect toSpiroString elems
            | Dot(p) -> 
                let x,y = font.GetXY p 
                [
                    sprintf "%d %d o " (x-thickness) (y)
                    sprintf "%d %d o " (x) (y+thickness)
                    sprintf "%d %d o " (x+thickness) (y)
                ]
            | Space -> []
            | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)
        (font.ElementToSpiroSegments elem |> List.collect bezierString)
        @ ["Spiro"] 
        @ toSpiroString elem 
        @ ["0 0 z"; "EndSpiro"]

    let spineSpiros = Font({font.axes with thickness = 2; outline = true}).charToOutline ch 
                        |> font.translateByThickness
                        |> elemToFF
    let outlineSpiros = font.charToOutline ch
                        |> elemToFF
    [
        sprintf "StartChar: %c\n" ch
        sprintf "Encoding: %d %d 0\n" (int ch) (int ch)
        sprintf "Width: %d\n" (font.charWidth ch + thickness)
        """
        InSpiro: 1
        Flags: H
        LayerCount: 2
        Back
        SplineSet
        """] 
    @ spineSpiros
    @ [
        """
        EndSplineSet
        Fore
        SplineSet
        """] 
    @ outlineSpiros 
    @ [
        """
        EndSplineSet
        EndChar
        """
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
