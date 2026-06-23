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
/// a digit after a coordinate letter repeats it that many times (a shorthand for the weighting above),
/// so "b2t"="bbt" (one-third up) and "r4c"="rrrrc" (four-fifths from centre toward the right)
/// brackets mean coordinates are adjusted (by the DactylSpline only) to be smoother
/// optional tangent direction (N)orth (S)outh (E)ast (W)est
/// lines/curves: (-) straight line, (~) curve, note: lines join curves smoothly
/// ( ) terminates a curve, a preceeding (- or ~) means closed curve (last point rejoins first point)
/// Solo points become dots
/// Regex for the language
let y_re = "[txhbd0-9]+|\([txhbd0-9]+\)"
let offset_re = "[oe]"
let x_re = "[lrcw0-9]+|\([lrcw0-9]+\)"
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
          '$', "thr~t(c)~(ttb)l~hc~(tbb)r~b(c)~bhl tec-bec"
          '%', "tllc~tthllc~tthlc~ brrc~bbhrrc~bbhrc~ ter-bel"
          '&', "hbbr~b(c)~(hb)l~thcr~tlcc~thl-br"
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
          '?', "thl~t(c)~(th)r~hhbc-bbhc bc"
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

          '0', "(h)l~t(c)~(h)r~b(c)~ tr-bl"
          '1', "tol-tlllr-blllr"
          '2', "tol~t(c)~(th)r~hbc-bl-br"
          '3', "tol~t(c)~(th)r~hc-hllr hllr-hc~(bh)r~b(c)~bol"
          '4', "brrrl-trrrl-bhl-bhr"
          '5', "tr-tl-hl hl~ttb(c)~(bbt)r~b(c)~bol"
          '6', "tor~t(c)~(h)l~bbtl~b(c)~bbtr~ttbc~bbtlN"
          '7', "tl-tr-bcl"
          //  two loops:
          //  '8', "hc~thl~tc~thr~ hc~bhl~bc~bhr~"
          // figure of eight:
          '8', "hc~(th)l~t(c)~(th)r~hc~(bh)l~b(c)~(bh)r~"
          '9', "bol~b(c)~(h)r~ttbr~t(c)~ttbl~bbtc~ttbrS"

          'A', "bl-tc-br bhlc-bhrc"
          'a', "xr-br xor~x(c)~(xb)l~b(c)~bor"
          'B', "hlE~(bh)r~blW-tlE~(th)r~hlE"
          'b', "tl-bl bol~b(c)~(xb)r~x(c)~xol"
          'C', "tor~t(c)~(h)l~b(c)~bor"
          'c', "xor~x(c)~(xb)l~b(c)~bor"
          'D', "tl-blE~(h)r~tlE"
          'd', "tr-br xor~x(c)~(xb)l~b(c)~bor"
          'E', "tr-tl-bl-br hl-hr"
          'e', "xbl-xbrN~x(c)~xblS~b(c)~bor"
          'F', "bl-tl-tr hl-hrc"
          'f', "bllc-xtllc~tcrW xl-xc"
          'G', "tor~t(c)~(h)l~b(c)~bhr-hr-hc"
          'g', "xr-bdr~d(c)~dol xor~x(c)~(xb)l~b(c)~bor"
          'H', "tl-bl hl-hr tr-br"
          'h', "tl-bl xol~x(c)~xbr-br"
          'I', "tl-tr tc-bc bl-br"
          'i', "xl-bl ttxl"
          'J', "tl-tr-hr~b(c)~bol"
          'j', "xc-bdc~dlE ttxc"
          'K', "tl-bl tr-hl hl-br"
          'k', "tl-bl xcr-xbl xbl-bcr"
          'L', "tl-bl-br"
          'l', "tl-xbl~bcW"
          'M', "bl-tl-blw-tw-bw"
          'm', "xl-bl xol~x(llw)~xxblw-blw xolw~x(lwwww)~xxbw-bw"
          'N', "bl-tl-br-tr"
          'n', "xl-bl xol~x(c)~xbr-br"
          'O', "(h)l~t(c)~(h)r~b(c)~"
          'o', "(xb)l~x(c)~(xb)r~b(c)~"
          'P', "bl-tlE~(th)r~hlE"
          'p', "xl-dl bol~b(c)~(xb)r~x(c)~xol"
          'Q', "(h)l~t(c)~(h)r~b(c)~ br-hbc"
          'q', "xr-dr xor~x(c)~(xb)l~b(c)~bor"
          'R', "bl-tlE~(th)r~hcl-hl hc-br"
          'r', "xl-bl xol~xlcc~xoccr"
          'S', "thr~t(c)~(ttb)l~hc~(tbb)r~b(c)~bhl"
          's', "xor~x(c)~(xxb)l~xbcE~(xbb)r~b(c)~bol"
          'T', "tl-tr tc-bc"
          't', "tlc-xblc~bccrW xl-xccr"
          'U', "tl-hl~b(c)~hr-tr"
          'u', "xl-xbl~b(llcr)~bocr xcr-bcr"
          'V', "tl-bc-tr"
          'v', "xl-bc-xr"
          'W', "tl-blllw-tlw-blwww-tw"
          'w', "xl-blllw-xlw-blwww-xw"
          'X', "tl-br tr-bl"
          'x', "xl-br xr-bl"
          'Y', "tl-hc-tr hc-bc"
          'y', "xl-xbl~b(c)~xbr-xr xr-br~d(c)~dol"
          'Z', "tl-tr-bl-br"
          'z', "xl-xr-bl-br" ]

// parse

/// Expand a coordinate string into the list of guide values to average.
/// Parentheses are ignored here (they set the fit flag separately). A digit
/// run immediately after a coordinate letter repeats that letter that many
/// times, so it counts proportionally in the average:
///   "r4c" -> [R;R;R;R;C]   "b2t" -> [B;B;T]   "th" -> [T;H] (unchanged)
let weightedCoords (cs: string) (coordOf: char -> float) =
    let rec loop chars acc =
        match chars with
        | [] -> List.rev acc
        | c :: rest when c = '(' || c = ')' -> loop rest acc
        | c :: rest ->
            let digits = rest |> List.takeWhile System.Char.IsDigit
            let rest2 = rest |> List.skipWhile System.Char.IsDigit
            let count =
                match digits with
                | [] -> 1
                | _ -> digits |> List.fold (fun a d -> a * 10 + (int d - int '0')) 0
            let v = coordOf c
            loop rest2 (List.replicate count v @ acc)

    loop (List.ofSeq cs) []

let parse_point (glyph: FontMetrics) def_raw =
    let mutable def = def_raw
    let start_def = def_raw

    // y_coord
    let match_y = Regex.Match(def, "^" + y_re)
    let ys = match_y.Value
    let y_fit = ys.StartsWith("(")

    let y_coords =
        weightedCoords ys (fun c ->
            match c with
            | 't' -> glyph.T
            | 'x' -> glyph.X
            | 'h' -> glyph.H
            | 'b' -> glyph.B
            | 'd' -> glyph.D
            | _ -> invalidArg "y" (sprintf "Invalid Y coord %A (should be in %A)" c y_re))

    let mutable y_coord = List.average y_coords
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
        weightedCoords xs (fun c ->
            match c with
            | 'l' -> glyph.L
            | 'c' -> glyph.C
            | 'r' -> glyph.R
            | 'w' -> glyph.W
            | _ -> invalidArg "x" (sprintf "Invalid X coord %A  (should be in %A)" c x_re))

    let x_coord = List.average x_coords
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

        // Auto-assign cardinal tangents to fitted-coordinate points that lack explicit ones.
        // y_fit means the point slides along a fixed x (left/right extremum) → vertical tangent.
        // x_fit means the point slides along a fixed y (top/bottom extremum) → horizontal tangent.
        // Direction (S/N or E/W) is inferred from the sign of the displacement to the next point.
        let n = pts.Length
        let explicit_tangents =
            [ for i in 0 .. n - 1 do
                let pt = pts.[i]
                match explicit_tangents.[i] with
                | Some _ as t -> t
                | None when pt.y_fit || pt.x_fit ->
                    let isInterior = isClosed || (i > 0 && i < n - 1)
                    if isInterior then
                        let prev = pts.[if i = 0 then n - 1 else i - 1]
                        let next = pts.[if i = n - 1 then 0 else i + 1]
                        if pt.y_fit then
                            Some(if prev.y > next.y then PI * -0.5 else PI * 0.5)
                        else
                            Some(if next.x > prev.x then 0.0 else PI)
                    else None
                | None -> None ]

        let knots =
            [ for i in 0 .. n - 1 do
                  let in_sep =
                      if i = 0 then
                          if isClosed then seps_out.[n - 1] else ""
                      else seps_out.[i - 1]
                  let out_sep =
                      if i = n - 1 && not isClosed then ""
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

        validateKnotSequence knots isClosed
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
