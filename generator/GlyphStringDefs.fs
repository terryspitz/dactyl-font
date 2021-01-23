module GlyphStringDefs

open System.Text.RegularExpressions
open GeneratorTypes
open GlyphDefs
open SpiroPointType

/// This file defines a minimal language for defining glyph outlines (AMLFDGO)
/// Turns out it's like a limited version of METAFONT from the 1970s, this was invented independently.

/// Defines glyphs using following symbols:
/// y coordinates: (b)ottom, (t)op, (h)alf height, (x)-height, (d)escender
/// (o) adds/subtracts a 'roundness' offset to y
/// x coordinates: (l)eft, (r)ight, (c)enter, (w)ide (em-width)
/// note multiple y or x coordinates means average across them, so "bt"="h" and "bbt" means one-third up
/// lines/curves: (-) straight line, (~) curve, note: lines join curves smoothly 
/// (.) for a corner (which requires a repeated point on both sides, sorry!)
/// ( ) terminates a curve, a preceeding (- or ~) means closed curve (last point rejoins first point)
/// Solo points become dots

/// Regex checker for the language
let y_re = "[txhbd]+"
let offset_re = "o"
let x_re = "[lrcw]+"
let line_re = "[-~.]"
let separator_re = " "
let optional_re x = x + "?"
let point_re = y_re + optional_re offset_re + x_re
let curve_re = "(" + point_re + line_re + ")*" + point_re + optional_re line_re
let glyph_re = "^ ?$|^(" + curve_re + separator_re + ")*" + curve_re + "$"

let glyphMap = Map.ofList [
        ' ', " "
        '!', "tc-hbc bc"
        '"', "tllr-tthllr tlrr-tthlrr"
        '#', ""
        '£', ""
        '$', ""
        '%', ""
        '&', ""
        ''', "tl-tthl"
        '’', "tl-tthl"
        '(', ""
        ')', ""
        '*', ""
        '+', ""
        '-', ""
        '.', "bl"
        ',', "blc-bbdl"
        '/', ""
        ':', ""
        ';', ""
        '<', ""
        '=', ""
        '>', ""
        '?', ""
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
        '6', "tor~tc~hl~bbtl~bc~bbtr~ttbc~bbtl"
        '7', "tl-tr-bcl"
        '8', "hc~thl~tc~thr~ hc~bhl~bc~bhr~"
        '9', "bol~bc~hr~ttbr~tc~ttbl~bbtc~ttbr"
    
        'A', "bl-tc-br bhlc-bhrc"
        'a', "xr-br xor~xc~xbl~bc~bor"
        'B', "hl-hlc~bhr~bl.bl-tl.tl~thr~hlc-"
        'b', "tl-bl bol~bc~xbr~xc~xol"
        'C', "tor~tc~hl~bc~bor"
        'c', "xor~xc~xbl~bc~bor"
        'D', "tl-bl.bl~hr~"
        'd', "tr-br xor~xc~xbl~bc~bor"
        'E', "tr-tl-bl-br hl-hr"
        'e', "xbl-xbr.xbr~xc~xbl~bc~br"
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
        'K', "tl-bl tr-hl-br"
        'k', "tl-bl xcr-xbl-bcr"
        'L', "tl-bl-br"
        'l', "tl-xbl~bc"
        'M', "bl-tl-blw-tw-bw"
        'm', "xl-bl xol~xllw~xblw-blw xxblw~xlwwww~xbw-bw"
        'N', "bl-tl-br-tr"
        'n', "xl-bl xol~xc~xbr-br"
        'O', "hl~tc~hr~bc~"
        'o', "xbl~xc~xbr~bc~"
        'P', "bl-tl.tl~thr~hl"
        'p', "xl-dl bol~bc~xbr~xc~xol"
        'Q', "hl~tc~hr~bc~ br-hbc"
        'q', "xr-dr xor~xc~xbl~bc~bor"
        'R', "bl-tl.tl~thr~hcl-hl hc-br"
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
let parse_point (font : GlyphDefs) def_raw =
    let mutable def = def_raw
    // y_coord
    let match_ = Regex.Match(def, "^" + y_re)
    // printfn "ycoord %A" match_
    let ys = match_.Value
    let y_coords = [
        for y in ys do
            match y with
            | 't' -> font._T
            | 'x' -> font._X
            | 'h' -> font._H
            | 'b' -> font._B
            | 'd' -> font._D
            ]
    let mutable y_coord = List.averageBy float y_coords |> int
    def <- def.[match_.Length..]
    // printfn "post-ycoord %A" def_left
    // offset
    if def.StartsWith(offset_re) then
        def <- def.[1..]
        y_coord <- if y_coord > font._H then y_coord-font._offset else y_coord+font._offset
    // printfn "post-offset %A" def_left
    // x_coord-------------------------------------
    let match_ = Regex.Match(def, "^" + x_re)
    // printfn "xcoord match %A" match_
    let xs = match_.Value
    let x_coords = [
        for x in xs do
            match x with
            | 'l' -> font._L
            | 'c' -> font._C
            | 'r' -> font._R
            | 'w' -> font._R*3/2]
    let x_coord = List.averageBy float x_coords |> int
    def <- def.[match_.Length..]
    // printfn "%A" def_left
    YX(y_coord, x_coord), def

let parse_curve (font : GlyphDefs) raw_def =
    let mutable pts, lines = [], []
    let mutable def : string = raw_def
    let mutable last_line = ""
    while def.Length > 0 do
        let pt, new_def = parse_point font def
        def <- new_def
        if last_line = "." then
            assert (pt = pts.[pts.Length-1])
        else
            pts <- pts @ [pt]
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

    // printfn "post-parse curve %A %A %A" pts lines def
    if pts.Length = 1 then
        Dot(pts.[0])
    elif pts.Length = lines.Length || last_line = "." then
        if last_line = "." then 
            lines <- lines @ [Corner]
            printfn "last_line = '.'"
        ClosedCurve(List.zip pts lines)
        // ClosedCurve(List.zip pts (SpiroPointType.G2 :: lines.[..lines.Length-2]))
    else
        if last_line = "~" then
            lines <- lines @ [G2]
        else
            lines <- lines @ [Corner]
        OpenCurve(List.zip pts lines)

let stringDefsToElem (glyphDefs : GlyphDefs) e = 
    let def = glyphMap.[e]
    assert Regex.IsMatch(def, glyph_re)
    printfn "%A: %A" e def
    if def = "" then
        Dot(HC)
    elif def = " " then
        Space
    else
        let curves = EList([for c in def.Split(separator_re) do parse_curve glyphDefs c])
        printfn "%A" curves
        curves

