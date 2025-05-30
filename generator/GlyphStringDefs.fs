module GlyphStringDefs

open System.Text.RegularExpressions
open GeneratorTypes
open GlyphFsDefs
open SpiroPointType

let PI = System.Math.PI

/// This file defines a minimal language for defining glyph outlines (AMLFDGO)
/// Turns out it's like a limited version of METAFONT from the 1970s.
/// This was invented independently.

/// Defines glyphs using following symbols:
/// y coordinates: (b)ottom, (t)op, (h)alf height, (x)-height, (d)escender
/// (o) adds/subtracts a 'roundness' offset to y
/// x coordinates: (l)eft, (r)ight, (c)enter, (w)ide (em-width)
/// note multiple y or x coordinates means average across them, so "bt"="h" and "bbt" means one-third up
/// optional tangent direction (N)orth (S)outh (E)ast (W)est
/// lines/curves: (-) straight line, (~) curve, note: lines join curves smoothly 
/// (.) for a corner (which requires a repeated point on both sides, sorry!)
/// ( ) terminates a curve, a preceeding (- or ~) means closed curve (last point rejoins first point)
/// Solo points become dots

/// Regex for the language
let y_re = "[txhbd]+"
let offset_re = "o"
let x_re = "[lrcw]+"
let direction_re = "[NSEW]"
let line_re = "[-~.]"
let separator_re = " "
let optional_re x = x + "?"
let point_re = y_re + optional_re offset_re + x_re + optional_re direction_re
let curve_re = "(" + point_re + line_re + ")*" + point_re + optional_re line_re
let glyph_re = "^ ?$|^(" + curve_re + separator_re + ")*" + curve_re + "$"

let glyphMap = Map.ofList [
        ' ', " "
        '□', "tl-tr-br-bl- xl-xr bl-dl-dr-br"
        '!', "tc-hbc bc"
        '"', "tllr-tthllr tlrr-tthlrr"
        '#', "ttbl-ttbr tbbl-tbbr tllr-bllr tlrr-blrr"
        '£', "tor~tc~txl~xllc~bl.bl-br xl-xcr"
        '$', "tthor~tthc~tthol~hc~hbbor~hbbc~hbbol tc-bc"
        '%', "tthllc bbhrrc tr-bl"
        '&', "hrS~bc~hbl~thcr~tlcc~thl-br"
        ''', "tl-tthl"
        '’', "tl-tthl"
        '(', "tlc~hl~blc"
        ')', "tl~hlc~bl"
        '*', "xl-xr-xbllc-txc-xbrrc-"
        '+', "hl-hr htc-hbc"
        '-', "hl-hr"
        '.', "bl"
        ',', "blc-bbdl"
        '/', "bl-tr"
        ':', "xl bl"
        ';', "xcl bocl-bl"
        '<', "xr-xbl-br"
        '=', "xl-xr xbl-xbr"
        '>', "xl-xbr-bl"
        '?', "tol~tc~tor~hc-bbhc bc"
        '@', ""
        '[', ""
        '\\', ""
        ']', ""
        '^', ""
        '_', ""
        '`', ""
        '{', ""
        '|', ""
        '}', ""
        '~', ""

        '0', "hl~tc~hr~bc~ tr-bl"
        '1', "tl-bl"
        '2', "tol~tc~thr~hbc-bl.bl-br"
        '3', "tol~tc~thr~hc-hllr hllr-hc~bhr~bc~bol"
        '4', "brrrl-trrrl-bhl-bhr"
        '5', "tr-tl-hl hl~ttbc~bbtr~bc~bol"
        '6', "tor~tc~hl~bbtl~bc~bbtr~ttbc~bbtlN"
        '7', "tl-tr-bcl"
        '8', "hc~thl~tc~thr~ hc~bhl~bc~bhr~"
        '9', "bol~bc~hr~ttbr~tc~ttbl~bbtc~ttbrS"
    
        'A', "bl-tc-br bhlc-bhrc"
        'a', "xr-br xor~xc~xbl~bc~bor"
        'B', "hl-hlc~bhr~bl.bl-tl.tl~thr~hlc-hl"
        'b', "tl-bl bol~bc~xbr~xc~xol"
        'C', "tor~tc~hl~bc~bor"
        'c', "xor~xc~xbl~bc~bor"
        'D', "tl-bl.bl~hr~"
        'd', "tr-br xor~xc~xbl~bc~bor"
        'E', "tr-tl-bl-br hl-hr"
        'e', "xbl-xbrN.xbrN~xc~xbl~bc~br"
        'F', "bl-tl-tr hl-hrc"
        'f', "bl-xl~tc xl-xc"
        'G', "tor~tc~hl~bc~bhr-hr.hr-hc"
        'g', "xr-br~dc~dol xor~xc~xbl~bc~bor"
        'H', "tl-bl hl-hr tr-br"
        'h', "tl-bl xol~xc~xbr-br"
        'I', "tl-tr tc-bc bl-br"
        'i', "xl-bl tl"
        'J', "tl-tr.tr-hr~bc~bol"
        'j', "xr-br~dc~dol tr"
        'K', "tl-bl tr-hl hl-br"
        'k', "tl-bl xcr-xbl xbl-bcr"
        'L', "tl-bl-br"
        'l', "tl-xbl~bc"
        'M', "bl-tl-blw-tw-bw"
        'm', "xl-bl xol~xllw~xblw-blw xxblw~xlwwww~xbw-bw"
        'N', "bl-tl-br-tr"
        'n', "xl-bl xol~xc~xbr-br"
        'O', "hl~tc~hr~bc~"
        'o', "xbl~xc~xbr~bc~"
        'P', "bl-tlE.tl~thr~hlE"
        'p', "xl-dl bol~bc~xbr~xc~xol"
        'Q', "hl~tc~hr~bc~ br-hbc"
        'q', "xr-dr xor~xc~xbl~bc~bor"
        'R', "bl-tlE.tlE~thr~hcl-hl hc-br"
        'r', "xl-bl xol~xc~xor"
        'S', "thr~tc~ttbl~hc~tbbr~bc~bhl"
        's', "xor~xc~xxbl~xbc~xbbr~bc~bol"
        'T', "tl-tr tc-bc"
        't', "tl-xbl~bc xl-xc"
        'U', "tl-hl~bc~hr-tr"
        'u', "xl-xbl~bc~xbr-xr xr-br"
        'V', "tl-bc-tr"
        'v', "xl-bc-xr"
        'W', "tl-blllw-tlw-blwww-tw"
        'w', "xl-blllw-xlw-blwww-xw"
        'X', "tl-br tr-bl"
        'x', "xl-br xr-bl"
        'Y', "tl-hc-tr hc-bc"
        'y', "xl-xbl~bc~xbr-xr xr-br~dc~dol"
        'Z', "tl-tr-bl-br"
        'z', "xl-xr-bl-br"
]

// parse
let parse_point (glyph : GlyphFsDefs) def_raw =
    let mutable def = def_raw
    // y_coord
    let match_ = Regex.Match(def, "^" + y_re)
    // printfn "ycoord %A" match_
    let ys = match_.Value
    let y_coords = [
        for y in ys do
            match y with
            | 't' -> glyph._T
            | 'x' -> glyph._X
            | 'h' -> glyph._H
            | 'b' -> glyph._B
            | 'd' -> glyph._D
            | _ -> invalidArg "y" (sprintf "Invalid Y coord %A (should be in %A)" y y_re) 
    ]
    let mutable y_coord = List.averageBy float y_coords |> int
    def <- def.[match_.Length..]
    // printfn "post-ycoord %A" def_left
    // offset
    if def.StartsWith(offset_re) then
        def <- def.[1..]
        y_coord <- if y_coord > glyph._H then y_coord-glyph._offset else y_coord+glyph._offset
    // printfn "post-offset %A" def_left
    // x_coord-------------------------------------
    let match_ = Regex.Match(def, "^" + x_re)
    // printfn "xcoord match %A" match_
    let xs = match_.Value
    let x_coords = [
        for x in xs do
            match x with
            | 'l' -> glyph._L
            | 'c' -> glyph._C
            | 'r' -> glyph._R
            | 'w' -> glyph._R*3/2
            | _ -> invalidArg "x" (sprintf "Invalid X coord %A  (should be in %A)" x x_re)
    ]
    let x_coord = List.averageBy float x_coords |> int
    def <- def.[match_.Length..]
    // printfn "%A" def_left

    let match_ = Regex.Match(def, "^" + direction_re)
    let tangent = 
        if match_.Success then
            def <- def.[match_.Length..]
            Some (
                match match_.Value with
                    | "N" -> PI * 0.5
                    | "S" -> PI * -0.5
                    | "E" -> 0.
                    | "W" -> PI 
                    | _ -> invalidArg "d" (sprintf "Invalid direction %A  (should be in %A)" match_.Value direction_re)
            )
        else None

    YX(y_coord, x_coord), tangent, def

let parse_curve (glyph : GlyphFsDefs) raw_def debug =
    let mutable pts, lines, tangents = [], [], []
    let mutable def : string = raw_def
    let mutable last_line = ""
    while def.Length > 0 do
        let pt, tangent, new_def = parse_point glyph def
        def <- new_def
        if last_line = "." then
            if pt <> pts.[pts.Length-1] then
                invalidArg "string" "'.' must separate points at the same location"
        else
            pts <- pts @ [pt]
            tangents <- tangents @ [tangent]
        // line_re
        let match_ = Regex.Match(def, "^" + line_re)
        if match_.Success then
            // printfn "line match %A" match_
            if match_.Value = "-" then
                if last_line = "~" then
                    lines <- lines @ [CurveToLine]
                else
                    lines <- lines @ [Corner]
            elif match_.Value = "~" then
                if last_line = "-" then
                    lines <- lines @ [LineToCurve]
                elif last_line = "." then
                    lines <- lines @ [Corner]
                else
                    lines <- lines @ [G2]
            elif match_.Value = "." then
                // lines <- lines @ [Corner]
                // don't add this line, or next point
                ()
            else
                invalidArg "match_" (sprintf "Unrecognised line separator %A" match_.Value)
            last_line <- match_.Value
            def <- def.[match_.Length..]
        else
            assert (def.Length = 0)
        // printfn "post-line %A" def

    if debug then
        printfn "post-parse curve %A %A %A" pts lines def
    if pts.Length = 1 then
        Dot(pts.[0])
    elif pts.Length = lines.Length || last_line = "." then    // Closed curve
        if last_line = "." then 
            lines <- lines @ [Corner]
        TangentCurve(List.zip3 pts lines tangents, true)
    else // Open curve
        if last_line = "~" then
            lines <- lines @ [G2]
        else
            lines <- lines @ [Corner]
        TangentCurve(List.zip3 pts lines tangents, false)

let stringDefsToElem (glyph : GlyphFsDefs) e debug = 
    let def = glyphMap.[e]
    assert Regex.IsMatch(def, glyph_re)
    if debug then
        printfn "%A: %A" e def
    if def = "" then
        Dot(HC)
    elif def = " " then
        Space
    else
        let curves = EList([
            for def in def.Split(separator_re) do parse_curve glyph def debug])
        // printfn "%A" curves
        curves

