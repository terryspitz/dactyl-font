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
/// y coordinates: (b)ottom/(b)ase, (t)op, (h)alf height, (x)-height, (d)escender
/// (o) adds/subtracts a 'roundness' offset to y
/// x coordinates: (l)eft, (r)ight, (c)enter, (w)ide (em-width)
/// note multiple y or x coordinates means average across them, so "bt"="h" and "bbt" means one-third up
/// brackets mean coordinates are adjusted (by the DactylSpline only) to be smoother
/// optional tangent direction (N)orth (S)outh (E)ast (W)est
/// lines/curves: (-) straight line, (~) curve, note: lines join curves smoothly
/// (.) for a corner (which requires a repeated point on both sides, sorry!)
/// ( ) terminates a curve, a preceeding (- or ~) means closed curve (last point rejoins first point)
/// Solo points become dots
/// Regex for the language
let y_re = "[txhbd]+|\([txhbd]+\)"
let offset_re = "o"
let x_re = "[lrcw]+|\([lrcw]+\)"
let direction_re = "[NSEW]"
let line_re = "[-~.]"
let separator_re = " "
let optional_re x = x + "?"
let point_re = y_re + optional_re offset_re + x_re + optional_re direction_re
let curve_re = "(" + point_re + line_re + ")*" + point_re + optional_re line_re
let glyph_re = "^ ?$|^(" + curve_re + separator_re + ")*" + curve_re + "$"

let glyphMap =
    Map.ofList
        [ ' ', " "
          '□', "tl-tr-br-bl- xl-xr bl-dl-dr-br"
          '!', "tc-hbc bc"
          '"', "tllr-tthllr tlrr-tthlrr"
          '#', "ttbl-ttbr tbbl-tbbr tllr-bllr tlrr-blrr"
          '£', "tor~tc~txl~xllc~bl.bl-br xl-xcr"
          '$', "tthor~tthc~tthol~hc~hbbor~hbbc~hbbol tc-bc"
          '%', "tthllc bbhrrc tr-bl"
          '&', "hbrS~bc~hbl~thcr~tlcc~thl-br"
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
          //  two loops:
          //  '8', "hc~thl~tc~thr~ hc~bhl~bc~bhr~"
          // figure of eight:
          '8', "hc~thl~tc~thr~hc~bhl~bc~bhr~"
          '9', "bol~bc~hr~ttbr~tc~ttbl~bbtc~ttbrS"

          'A', "bl-tc-br bhlc-bhrc"
          'a', "xr-br xor~x(c)~xbl~bc~bor"
          'B', "hlE~(bh)r~blE.bl-tl.tlE~(th)r~hlE"
          'b', "tl-bl bol~bc~xbr~xc~xol"
          'C', "tor~tc~hl~bc~bor"
          'c', "xor~xc~xbl~bc~bor"
          'D', "tl-bl.bl~hr~tl"
          'd', "tr-br xor~xc~xbl~bc~bor"
          'E', "tr-tl-bl-br hl-hr"
          'e', "xbl-xbr xbrN~xcW~xblS~bcE~bor"
          'F', "bl-tl-tr hl-hrc"
          'f', "bllc-xtllc~tcrW xl-xc"
          'G', "tor~tc~hl~bc~bhr-hr.hr-hc"
          'g', "xr-bdr~dcW~dol xor~xcW~xbl~bcE~bor"
          'H', "tl-bl hl-hr tr-br"
          'h', "tl-bl xol~xc~xbcr-bcr"
          'I', "tl-tr tc-bc bl-br"
          'i', "xl-bl ttxl"
          'J', "tl-tr.tr-hr~bc~bol"
          'j', "xcr-bdcr~dc~dol ttxcr"
          'K', "tl-bl tr-hl hl-br"
          'k', "tl-bl xcr-xbl xbl-bcr"
          'L', "tl-bl-br"
          'l', "tl-xbl~bc"
          'M', "bl-tl-blw-tw-bw"
          'm', "xl-bl xol~xllw~xblw-blw xxblw~xlwwww~xbw-bw"
          'N', "bl-tl-br-tr"
          'n', "xl-bl xol~x(c)E~xbr-br"
          'O', "hl~tc~hr~bc~"
          'o', "xbl~xc~xbr~bc~"
          'P', "bl-tlE.tl~thr~hlE"
          'p', "xl-dl bol~bc~xbr~xc~xol"
          'Q', "hl~tc~hr~bc~ br-hbc"
          'q', "xr-dr xor~xc~xbl~bc~bor"
          'R', "bl-tlE.tlE~thr~hcl-hl hc-br"
          'r', "xl-bl xol~xlcc~xoccr"
          'S', "thr~tc~ttbl~hc~tbbr~bc~bhl"
          's', "xor~x(c)W~xbl~xbcE~xbbr~bcW~bol"
          'T', "tl-tr tc-bc"
          't', "tlc-xbblc~bc~bccr xl-xccr"
          'U', "tl-hl~bc~hr-tr"
          'u', "xl-xbl~bc~xbcr-xcr xcr-bcr"
          'V', "tl-bc-tr"
          'v', "xl-bc-xr"
          'W', "tl-blllw-tlw-blwww-tw"
          'w', "xl-blllw-xlw-blwww-xw"
          'X', "tl-br tr-bl"
          'x', "xl-br xr-bl"
          'Y', "tl-hc-tr hc-bc"
          'y', "xl-xbl~bc~xbr-xr xr-br~dc~dol"
          'Z', "tl-tr-bl-br"
          'z', "xl-xr-bl-br" ]

// parse
let parse_point (glyph: GlyphFsDefs) def_raw =
    let mutable def = def_raw
    // y_coord
    let match_ = Regex.Match(def, "^" + y_re)
    // printfn "ycoord %A" match_
    let ys = match_.Value
    let y_fit = ys.StartsWith("(")

    let y_coords =
        [ for c in ys do
              match c with
              | '('
              | ')' -> () // ignore brackets
              | c ->
                  yield
                      Some(
                          match c with
                          | 't' -> glyph._T
                          | 'x' -> glyph._X
                          | 'h' -> glyph._H
                          | 'b' -> glyph._B
                          | 'd' -> glyph._D
                          | _ -> invalidArg "y" (sprintf "Invalid Y coord %A (should be in %A)" c y_re)
                      ) ]
        |> List.choose id

    let mutable y_coord = List.averageBy float y_coords |> int
    def <- def.[match_.Length ..]
    // printfn "post-ycoord %A" def_left
    // offset
    if def.StartsWith(offset_re) then
        def <- def.[1..]

        y_coord <-
            if y_coord >= glyph._X || y_coord >= glyph._H then
                y_coord - glyph._offset
            else
                y_coord + glyph._offset
    // printfn "post-offset %A" def_left
    // x_coord-------------------------------------
    let match_ = Regex.Match(def, "^" + x_re)
    // printfn "xcoord match %A" match_
    let xs = match_.Value
    let x_fit = xs.StartsWith("(")

    let x_coords =
        [ for c in xs do
              match c with
              | '('
              | ')' -> () // ignore brackets
              | c ->
                  yield
                      Some(
                          match c with
                          | 'l' -> glyph._L
                          | 'c' -> glyph._C
                          | 'r' -> glyph._R
                          | 'w' -> glyph._R * 3 / 2
                          | _ -> invalidArg "x" (sprintf "Invalid X coord %A  (should be in %A)" c x_re)
                      ) ]
        |> List.choose id

    let x_coord = List.averageBy float x_coords |> int
    def <- def.[match_.Length ..]
    // printfn "%A" def_left

    let match_ = Regex.Match(def, "^" + direction_re)

    let tangent =
        if match_.Success then
            def <- def.[match_.Length ..]

            Some(
                match match_.Value with
                | "N" -> PI * 0.5
                | "S" -> PI * -0.5
                | "E" -> 0.
                | "W" -> PI
                | _ -> invalidArg "d" (sprintf "Invalid direction %A  (should be in %A)" match_.Value direction_re)
            )
        else
            None

    if x_fit || y_fit then
        OYX(y_coord, x_coord, y_fit, x_fit), tangent, def
    else
        YX(y_coord, x_coord), tangent, def

let parse_curve (glyph: GlyphFsDefs) raw_def debug =
    let mutable pts, lines, tangents = [], [], []
    let mutable def: string = raw_def
    let mutable last_line = ""

    while def.Length > 0 do
        let pt, tangent, new_def = parse_point glyph def
        def <- new_def

        if last_line = "." && pt <> pts.[pts.Length - 1] then
            invalidArg "string" "'.' must separate points at the same location"

        pts <- pts @ [ pt ]
        tangents <- tangents @ [ tangent ]
        // line_re
        let match_ = Regex.Match(def, "^" + line_re)

        if match_.Success then
            // printfn "line match %A" match_
            if match_.Value = "-" then
                if last_line = "~" then
                    lines <- lines @ [ CurveToLine ]
                else
                    lines <- lines @ [ Corner ]
            elif match_.Value = "~" then
                if last_line = "-" then lines <- lines @ [ LineToCurve ]
                elif last_line = "." then lines <- lines @ [ Corner ]
                else lines <- lines @ [ G2 ]
            elif match_.Value = "." then
                lines <- lines @ [ Corner ]
            else
                invalidArg "match_" (sprintf "Unrecognised line separator %A" match_.Value)

            last_line <- match_.Value
            def <- def.[match_.Length ..]
        else
            assert (def.Length = 0)
    // printfn "post-line %A" def

    if debug then
        printfn "post-parse curve %A left: %A" (List.zip3 pts lines tangents) def

    if pts.Length = 1 then
        Dot(pts.[0])
    elif pts.Length = lines.Length || last_line = "." then // Closed curve
        if last_line = "." then
            lines <- lines @ [ Corner ]

        TangentCurve(List.zip3 pts lines tangents, true)
    else // Open curve
        if last_line = "~" then
            lines <- lines @ [ G2 ]
        else
            lines <- lines @ [ Corner ]

        TangentCurve(List.zip3 pts lines tangents, false)

let stringDefsToElem (glyph: GlyphFsDefs) e debug =
    let def = glyphMap.[e]
    assert Regex.IsMatch(def, glyph_re)

    if debug then
        printfn "%A: %A" e def

    if def = "" then
        Dot(HC)
    elif def = " " then
        Space
    else
        let curves =
            EList(
                [ for def in def.Split(separator_re) do
                      parse_curve glyph def debug ]
            )
        // printfn "%A" curves
        curves
