module GlyphStringDefs

open System.Text.RegularExpressions
open GeneratorTypes
open GeneratorTypes
open SpiroPointType

let PI = System.Math.PI

/// This file defines a minimal language for defining glyph outlines (AMLFDGO)
/// Turns out it's like a limited version of METAFONT from the 1970s.
/// This was invented independently.
/// Defines glyphs using following symbols:
/// y coordinates: (b)ottom/(b)ase, (t)op, (h)alf height, (x)-height, (d)escender
/// "o" adds/subtracts a "roundness" offset to y (inwards)
/// "e" adds/subtracts an "extended" thickness offset to y (outwards)
/// x coordinates: (l)eft, (r)ight, (c)enter, (w)ide (em-width)
/// note multiple y or x coordinates means average across them, so "bt"="h" and "bbt" means one-third up
/// brackets mean coordinates are adjusted (by the DactylSpline only) to be smoother
/// optional tangent direction (N)orth (S)outh (E)ast (W)est
/// lines/curves: (-) straight line, (~) curve, note: lines join curves smoothly
/// ( ) terminates a curve, a preceeding (- or ~) means closed curve (last point rejoins first point)
/// Solo points become dots
/// Regex for the language
let y_re = "[txhbd]+|\([txhbd]+\)"
let offset_re = "[oe]"
let x_re = "[lrcw]+|\([lrcw]+\)"
let direction_re = "[NSEW]"
let line_re = "[-~]"
let separator_re = " "
let optional_re x = x + "?"
let point_re = y_re + optional_re offset_re + x_re + optional_re direction_re
let curve_re = "(" + point_re + line_re + ")*" + point_re + optional_re line_re
let glyph_re = "^ ?$|^(" + curve_re + separator_re + ")*" + curve_re + "$"

let glyphMap =
    Map.ofList
        [ ' ', " "
          '□', "tl-tr-br-bl- xl-xr bl-dl-dr-br" //frame for showing top/x/descender heights
          '!', "tl-hbl bl"
          '"', "tellr-tthllr telrr-tthlrr"
          '#', "ttbl-ttbr tbbl-tbbr tllr-bllr tlrr-blrr"
          '£', "tor~tc~txl~xllc~blS-br xl-xcr"
          '$', "thr~tc~ttbl~hc~tbbr~bc~bhl tec-bec"
          '%', "tllc~tthllc~tthlc~ brrc~bbhrrc~bbhrc~ ter-bel"
          '&', "hbrS~bc~hbl~thcr~tlcc~thl-br"
          ''', "tel-tthl"
          '’', "telc-tthl"
          '`', "tel-tthlc"
          '(', "telc~hl~belc"
          ')', "tel~hlc~bel"
          '*', "xl-xbr xbl-xr txxc-xbbc"
          '+', "hl-hr htc-hbc"
          '-', "hl-hr"
          '.', "bl"
          ',', "blc-bbdl"
          '/', "bel-ter"
          ':', "xbl bl"
          ';', "xbcl bocl-bbdl"
          '<', "xr-xbl-br"
          '=', "xxbl-xxbr xbbl-xbbr"
          '>', "xl-xbr-bl"
          '?', "thl~tc~thr~hhbc-bbhc bc"
          '@', "bbtrcc~bbbtc~hcl~ttbc~hrcc~bbtrccS~bbbtcrr~hrN~te(c)~hlS~be(c)~bor"
          '[', "tec-tel-bel-bec"
          '\\', "tel-ber"
          ']', "tec-ter-ber-bec"
          '^', "ttbl-tc-ttbr"
          '_', "bel-ber"
          '{', "tecW~hlE hlE~becW"
          '}', "telE~hcW hcW~belE"
          '|', "tec-bec"
          '~', "tttthl~tlc~tttthc~ttthhrc~tttthr"

          '0', "hl~tc~hr~bc~ tr-bl"
          '1', "tol-tlllr-blllr"
          '2', "tol~tc~thr~hbc-bl-br"
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
          'B', "hlE~(bh)r~blW-tlE~(th)r~hlE"
          'b', "tl-bl bol~bc~xbr~xc~xol"
          'C', "tor~tc~hl~bc~bor"
          'c', "xor~xc~xbl~bc~bor"
          'D', "tl-blE~hr~tl"
          'd', "tr-br xor~xc~xbl~bc~bor"
          'E', "tr-tl-bl-br hl-hr"
          'e', "xbl-xbrN~xcW~xblS~b(c)~bor"
          'F', "bl-tl-tr hl-hrc"
          'f', "bllc-xtllc~tcrW xl-xc"
          'G', "tor~tc~(h)lS~bc~bhr-hr-hc"
          'g', "xr-bdr~dcW~dol xor~xcW~xbl~bcE~bor"
          'H', "tl-bl hl-hr tr-br"
          'h', "tl-bl xol~xc~xbr-br"
          'I', "tl-tr tc-bc bl-br"
          'i', "xl-bl ttxl"
          'J', "tl-tr-hr~bc~bol"
          'j', "xcr-bdcr~dc~dol ttxcr"
          'K', "tl-bl tr-hl hl-br"
          'k', "tl-bl xcr-xbl xbl-bcr"
          'L', "tl-bl-br"
          'l', "tl-xbl~bcW"
          'M', "bl-tl-blw-tw-bw"
          'm', "xl-bl xol~xllw~xblw-blw xxblw~xlwwww~xbw-bw"
          'N', "bl-tl-br-tr"
          'n', "xl-bl xol~x(c)E~xbr-br"
          'O', "hl~tc~hr~bc~"
          'o', "xbl~xc~xbr~bc~"
          'P', "bl-tlE~(th)rS~hlE"
          'p', "xl-dl bol~bc~xbr~xc~xol"
          'Q', "hl~tc~hr~bc~ br-hbc"
          'q', "xr-dr xor~xc~xbl~bc~bor"
          'R', "bl-tlE~thr~hcl-hl hc-br"
          'r', "xl-bl xol~xlcc~xoccr"
          'S', "thr~tc~ttbl~hc~tbbr~bc~bhl"
          's', "xor~x(c)W~xxbl~xbcE~xbbr~bcW~bol"
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
let parse_point (glyph: FontMetrics) def_raw =
    let mutable def = def_raw
    let start_def = def_raw

    // y_coord
    let match_y = Regex.Match(def, "^" + y_re)
    let ys = match_y.Value
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
                          | 't' -> glyph.T
                          | 'x' -> glyph.X
                          | 'h' -> glyph.H
                          | 'b' -> glyph.B
                          | 'd' -> glyph.D
                          | _ -> invalidArg "y" (sprintf "Invalid Y coord %A (should be in %A)" c y_re)
                      ) ]
        |> List.choose id

    let mutable y_coord = List.averageBy float y_coords
    def <- def.[match_y.Length ..]

    // offset
    let matchOffset = Regex.Match(def, "^" + offset_re)

    if matchOffset.Success then
        def <- def.[matchOffset.Length ..]

        let isExtended = matchOffset.Value = "e"
        let offsetAmount = if isExtended then glyph.thickness else -glyph.offset

        y_coord <-
            if y_coord >= glyph.X || y_coord >= glyph.H then
                y_coord + offsetAmount
            else
                y_coord - offsetAmount

    // x_coord
    let match_x = Regex.Match(def, "^" + x_re)
    let xs = match_x.Value
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
                          | 'l' -> glyph.L
                          | 'c' -> glyph.C
                          | 'r' -> glyph.R
                          | 'w' -> glyph.W
                          | _ -> invalidArg "x" (sprintf "Invalid X coord %A  (should be in %A)" c x_re)
                      ) ]
        |> List.choose id

    let x_coord = List.averageBy float x_coords
    def <- def.[match_x.Length ..]

    let match_dir = Regex.Match(def, "^" + direction_re)

    let tangent =
        if match_dir.Success then
            def <- def.[match_dir.Length ..]

            Some(
                match match_dir.Value with
                | "N" -> PI * 0.5
                | "S" -> PI * -0.5
                | "E" -> 0.
                | "W" -> PI
                | _ -> invalidArg "d" (sprintf "Invalid direction %A  (should be in %A)" match_dir.Value direction_re)
            )
        else
            None
    
    let label = start_def.Substring(0, start_def.Length - def.Length)
    { y = y_coord; x = x_coord; y_fit = y_fit; x_fit = x_fit }, tangent, label, def

let parse_curve (glyph: FontMetrics) raw_def debug =
    let mutable pts = []
    let mutable explicit_tangents = []
    let mutable labels = []
    let mutable seps_out = []
    let mutable def: string = raw_def

    while def.Length > 0 do
        let pt, tangent, label, new_def = parse_point glyph def
        def <- new_def

        pts <- pts @ [ pt ]
        explicit_tangents <- explicit_tangents @ [ tangent ]
        labels <- labels @ [ label ]
        // line_re
        let match_ = Regex.Match(def, "^" + line_re)

        if match_.Success then
            seps_out <- seps_out @ [ match_.Value ]
            def <- def.[match_.Length ..]
        else
            seps_out <- seps_out @ [ "" ]
            assert (def.Length = 0)
    // printfn "post-line %A" def

    if pts.Length = 1 then
        Dot(pts.[0])
    else
        let isClosed = (seps_out.[pts.Length - 1] = "-" || seps_out.[pts.Length - 1] = "~")
        
        let knots =
            [ for i in 0 .. pts.Length - 1 do
                  let in_sep = 
                      if i = 0 then 
                          if isClosed then seps_out.[pts.Length - 1] else ""
                      else seps_out.[i - 1]
                  let out_sep = 
                      if i = pts.Length - 1 && not isClosed then ""
                      else seps_out.[i]
                  
                  let has_curve_in = (in_sep = "~")
                  let has_curve_out = (out_sep = "~")
                  
                  let tIn, tOut = 
                      match explicit_tangents.[i] with
                      | Some t ->
                          if not has_curve_in && not has_curve_out then
                              invalidArg "tangent" "Explicit tangents cannot be applied to points with only straight lines."
                          elif has_curve_in && has_curve_out then
                              Some t, Some t
                          elif has_curve_in then
                              Some t, None
                          else
                              None, Some t
                      | None -> None, None
                      
                  let mutable ty = 
                      match in_sep, out_sep with
                      | "-", "~" -> LineToCurve
                      | "~", "-" -> CurveToLine
                      | _, "-" -> Corner
                      | _, "~" -> G2
                      | "~", "" -> G2
                      | "-", "" -> Corner
                      | _, _ -> Corner

                  if ty = CurveToLine && tIn.IsSome then ty <- Corner
                  if ty = LineToCurve && tOut.IsSome then ty <- Corner

                  { pt = pts.[i]; ty = ty; th_in = tIn; th_out = tOut; label = Some labels.[i] } ]
            |> mergeConsecutive
                (fun k -> System.Math.Round(k.pt.x, 3), System.Math.Round(k.pt.y, 3))
                (fun k1 k2 ->
                    let ty = if k1.ty = Corner || k2.ty = Corner then Corner else k2.ty

                    { k2 with
                        ty = ty
                        th_in = Option.orElse k1.th_in k2.th_in
                        th_out = Option.orElse k2.th_out k1.th_out
                        label = Option.orElse k1.label k2.label })

        Curve(knots, isClosed)

let private parse_curves (glyph: FontMetrics) (def: string) debug =
    if System.String.IsNullOrEmpty(def) then
        Dot({ y = glyph.H; x = glyph.C; y_fit = false; x_fit = false })
    elif def = " " then
        Space
    else
        EList(
            [ for d in def.Split(separator_re) do
                  if not (System.String.IsNullOrWhiteSpace(d)) then
                      parse_curve glyph d debug ]
        )

let stringDefsToElem (glyph: FontMetrics) e debug =
    let def = glyphMap.[e]
    assert Regex.IsMatch(def, glyph_re)

    if debug then
        printfn "%A: %A" e def

    parse_curves glyph def debug

let rawDefToElem (glyph: FontMetrics) (rawDef: string) debug =
    try
        parse_curves glyph rawDef debug
    with _ ->
        Dot({ y = glyph.H; x = glyph.C; y_fit = false; x_fit = false })
