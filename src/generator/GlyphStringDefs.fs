module GlyphStringDefs

open System.Text.RegularExpressions
open GeneratorTypes
open GeneratorTypes
open SpiroPointType

let PI = System.Math.PI

/// A minimal declarative language for defining glyph outlines (invented
/// independently; turns out to be a limited METAFONT). See docs/DactylGlyphs.md
/// for the full syntax reference.
/// Regex for the language
let y_re = "[txhbd0-9]+|\([txhbd0-9]+\)"
let offset_re = "[oe]"
let x_re = "[lrcw0-9]+|\([lrcw0-9]+\)"
let direction_re = "[NSEW]"
// Explicit interior-joint marker: a trailing `j` on a point declares that an
// open-stroke endpoint landing here is a joint against another stroke, so its
// cap (serif/flare/bulb) is suppressed. See Font.isJointRaw and DactylGlyphs.md.
let joint_re = "j"
// Explicit corner (kink) marker: a trailing `K` forces the point to be a Corner,
// breaking tangent continuity there while leaving both tangents free for the
// solver. This is what lets a straight stem run directly into a curve that
// leaves at an angle of its own choosing (e.g. the acute join in '5'), which a
// plain `-`~`~` junction would otherwise smooth over. See Font.fs and
// DactylGlyphs.md.
let corner_re = "K"
let line_re = "[-~]"
let separator_re = " "
let optional_re x = x + "?"
let point_re =
    y_re
    + optional_re offset_re
    + x_re
    + optional_re offset_re
    + optional_re direction_re
    + optional_re corner_re
    + optional_re joint_re
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
          // Smart quotes: raised ticks matching the apostrophe style. The
          // opening pair (‘ “) slant one way, the closing pair (’ ”) mirror it.
          '‘', "tel-tthlc"
          '“', "tel-tthlc tec-tthcr"
          '”', "telc-tthl tecr-tthc"
          '`', "tel-tthlc"
          '(', "telc~hl~belc"
          ')', "tel~hlc~bel"
          '*', "xl-xbr xbl-xr txxc-xbbc"
          '+', "hl-hr htc-hbc"
          '-', "hl-hr"
          // Dashes at hyphen height, progressively wider: hyphen (to R) <
          // en dash (to R..W midpoint) < em dash (to W).
          '–', "hl-hrw"
          '—', "hl-hw"
          '.', "bl"
          '•', "hc"          // bullet: a single mid-height dot
          '…', "bl bc br"    // ellipsis: three baseline dots
          ',', "blc-bbdl"
          '/', "bel-ter"
          ':', "xbl bl"
          ';', "xbcl bocl-bbdl"
          '<', "xr-xbl-br"
          '=', "xxbl-xxbr xbbl-xbbr"
          '>', "xl-xbr-bl"
          '?', "thl~t(c)~(th)r~hhbc-bbhc bc"
          '@', "bbtrcc~b3tc~hcl~ttbc~hrcc~bbtrccS~b3tcrr~hrN~te(c)~hlS~be(c)~bor"
          '[', "tec-tel-bel-bec"
          '\\', "tel-ber"
          ']', "tec-ter-ber-bec"
          '^', "ttbl-tc-ttbr"
          '_', "bel-ber"
          '{', "tecW~hlE hlE~becW"
          '}', "telE~hcW hcW~belE"
          '|', "tec-bec"
          '~', "t4hl~tlc~t4hc~t3h2rc~t4hr"

          '0', "(h)l~t(c)~(h)r~b(c)~ tr-bl"
          '1', "tol-tl3r-bl3r"
          '2', "tol~t(c)~(th)r~hbc-bl-br"
          // One continuous stroke through the waist: the upper bowl runs into the lower
          // one at a kink (`K`), instead of two strokes each ending in a horizontal
          // spur drawn twice on top of itself. `E` at the kink makes both tangents
          // horizontal — in from the east, out to the east — so the waist is level.
          '3', "tol~t(c)~(th)r~hllrEK~(bh)r~b(c)~bol"
          '4', "br3l-tr3l-bhl-bhr"
          // One continuous stroke: the stem runs into the bowl at an acute kink (`K`),
          // rather than two overlapping strokes whose caps left a notch at the join.
          '5', "tr-tl-hlK~ttb(c)~(bbt)r~b(c)~bol"
          '6', "tor~t(c)~(h)l~bbtl~b(c)~bbtr~ttbc~bbtlNj"
          '7', "tl-tr-bcl"
          //  two loops:
          //  '8', "hc~thl~tc~thr~ hc~bhl~bc~bhr~"
          // figure of eight:
          '8', "hc~(th)l~t(c)~(th)r~hc~(bh)l~b(c)~(bh)r~"
          '9', "bol~b(c)~(h)r~ttbr~t(c)~ttbl~bbtc~ttbrSj"

          'A', "bl-tc-br bhl3cj-bhcr3j"
          'a', "xr-br xor~x(c)~(xb)l~b(c)~bor"
          'B', "hl-hlo~(bh)r~blo-bl-tl-tlo~(th)r~hlo-hl"
          'b', "tl-bl bol~b(c)~(xb)r~x(c)~xol"
          'C', "tor~t(c)~(h)l~b(c)~bor"
          'c', "xor~x(c)~(xb)l~b(c)~bor"
          'D', "tl-bl-blo~(h)r~tlo-"
          'd', "tr-br xor~x(c)~(xb)l~b(c)~bor"
          'E', "tr-tl-bl-br hl-hr"
          'e', "xblj-xbrN~x(c)~xblS~b(c)~bor5c"
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
          // Leg springs from the arm (like 'k' below), not from the stem: two strokes
          // both ending at the stem cap each other perpendicular to their own axis, and
          // the caps cross inside the stem, leaving the ink between them unfilled — a
          // white bite out of the junction that widens with weight. `j` buries the leg's
          // cap inside the arm instead. The junction sits at `h9b` rather than `h`: `h`
          // takes the `balance` raise meant for crossbars and waists, which lifted this
          // vertex above the optical middle. `h8tl4r` is 1/5 along the arm, the point the
          // coordinate grid puts closest to the arm's spine once it is lowered (0.2 units
          // off) — springing from off the spine leaves a spur at hairline weights.
          'K', "tl-bl tr-h9bl h8tl4rj-br"
          'k', "tl-bl xb2l-xcr x2bc3lj-bcr"
          'L', "tl-bl-br"
          'l', "tl-xbl~bcW"
          'M', "bl-tl-blw-tw-bw"
          // The two arches are one stroke, joined by a kink (`K`) over the middle leg,
          // which then hangs from that kink as a joint. Previously the second arch
          // sprang from a point part-way down the first leg, so its end cap sat in the
          // crotch — the thinnest part of the junction — and stepped the outline there.
          // Of the three strokes meeting here, the leg is the one whose cap hides best:
          // it starts below the crotch with arch ink either side of it.
          'm', "xl-bl xolj~x(llw)~xxblwK~x(rw)~xxbw-bw xxblwj-blw"
          'N', "bl-tl-br-tr"
          'n', "xl-bl xol~x(c)~xbr-br"
          'O', "(h)l~t(c)~(h)r~b(c)~"
          'o', "(xb)l~x(c)~(xb)r~b(c)~"
          'P', "bl-tl-tlo~(th)r~hlo-hl"
          'p', "xl-dl bol~b(c)~(xb)r~x(c)~xol"
          'Q', "(h)l~t(c)~(h)r~b(c)~ br-hbc"
          'q', "xr-dr xor~x(c)~(xb)l~b(c)~bor"
          'R', "bl-tl-tlo~(th)r~hlo-hlj hloj-br"
          'r', "xl-bl xol~xlcc~xoccr"
          'S', "thr~t(c)~(ttb)l~hc~(tbb)r~b(c)~bhl"
          's', "xor~x(c)~(xxb)l~xbcE~(xbb)r~b(c)~bol"
          'T', "tl-tr tc-bc"
          't', "tlc-xblc~bccrW xl-xccr"
          'U', "tl-hl~b(c)~hr-tr"
          'u', "xl-xbl~b(llcr)~bocr xcr-bcr"
          'V', "tl-bc-tr"
          'v', "xl-bc-xr"
          'W', "tl-bl3w-tlw-blw3-tw"
          'w', "xl-bl3w-xlw-blw3-xw"
          'X', "tl-br tr-bl"
          'x', "xl-br xr-bl"
          'Y', "tl-hc-tr hcj-bc"
          'y', "xl-xbl~b(c)~xbr-xr xr-br~d(c)~dol"
          'Z', "tl-tr-bl-br"
          'z', "xl-xr-bl-br" ]

/// Alternate (stylistic-alternate) glyph shapes, selected by the `cursive` axis.
/// The default 'a' and 'g' above are single-storey forms (a circular bowl with a
/// straight stem, and an open-tail g).  These provide two-storey forms modelled
/// on humanist sans faces like Open Sans:
///   'a': a right stem whose top arches over into an open hood ending high on
///        the left, over a flat-topped bowl occupying the lower ~60%.
///   'g': a binocular g — a small round bowl hanging from x-height, a flat ear
///        at x-height reaching the right edge, and a short central neck down to
///        a wider, flatter loop sitting wholly below the baseline.
/// Both are written in the same coordinate language as glyphMap, so they inherit
/// width, x-height, thickness, roundedness, italic, etc. from the other axes.
let altGlyphMap =
    Map.ofList
        [ 'a', "br-xxbr~x(c)~xol3c xbr~b2x3(c)~(bbx)l~b(c)~bor"
          'g', "(bx)l~x(c)~(bx)r2c~b2x(c)~ xc-xr b2xlc3W~blc3W (bd)l~bc~(bd)r~d(c)~" ]

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

    // Optical balance ("mid height > 1/2"): a height that sits *between* the
    // guides — a crossbar (`h` in H/E/F, `xb` in e) or a bowl waist (`h` in B/S)
    // — is nudged up, because we read a letter drawn with an arithmetically
    // centred bar as bottom-heavy.  Heights written as a single guide letter
    // (`t`, `x`, `b`, `d`) are the reference lines themselves and never move, and
    // neither do fitted heights (`(h)l`), which are the *side* extremes of round
    // letters like O and o and want to stay symmetric.
    let yLetters = ys |> Seq.filter System.Char.IsLetter |> Seq.distinct |> List.ofSeq

    let isGuideHeight =
        match yLetters with
        | [ c ] -> c <> 'h' // `h` is itself a mid height, so it takes the raise
        | _ -> false

    let balanceRaise =
        if y_fit || isGuideHeight then
            0.0
        else
            glyph.balanceRaise y_coord

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

    // Applied after the inward/outward offset so that the offset still keys off
    // the guide the point was written against.
    y_coord <- y_coord + balanceRaise

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

    let mutable x_coord = List.average x_coords
    def <- def.[match_x.Length ..]

    // horizontal offset (mirrors the vertical offset above): moves the point
    // inward toward the vertical centerline, used to carve short flat
    // "shoulders" on bowls (e.g. B/D/P/R). Unlike the vertical 'o' offset,
    // this shoulder shrinks as roundedness increases (and nearly vanishes at
    // max roundedness) so that lower roundedness gives squarer letterforms.
    // The shoulder spans 90% of the glyph width at roundedness=0, shrinking
    // to the same length the old flat formula gave at roundedness=60 (i.e.
    // 100-60=40 units at default width) by roundedness=100.
    let matchXOffset = Regex.Match(def, "^" + offset_re)

    if matchXOffset.Success then
        def <- def.[matchXOffset.Length ..]

        let isExtended = matchXOffset.Value = "e"

        let offsetAmount =
            if isExtended then
                glyph.thickness
            else
                let maxFraction = 0.9
                let minFraction = 40.0 / 300.0
                let fraction = maxFraction - (maxFraction - minFraction) * (glyph.offset / 100.0)
                -(glyph.R * fraction)

        x_coord <-
            if x_coord >= glyph.C then
                x_coord + offsetAmount
            else
                x_coord - offsetAmount

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

    // optional explicit-corner (kink) marker
    let match_corner = Regex.Match(def, "^" + corner_re)
    let isCorner = match_corner.Success
    if match_corner.Success then
        def <- def.[match_corner.Length ..]

    // optional explicit-joint marker
    let match_joint = Regex.Match(def, "^" + joint_re)
    let isJoint = match_joint.Success
    if match_joint.Success then
        def <- def.[match_joint.Length ..]

    let label = start_def.Substring(0, start_def.Length - def.Length)
    { y = y_coord; x = x_coord; y_fit = y_fit; x_fit = x_fit }, tangent, isCorner, isJoint, label, def

/// Optical overshoot: a round or pointed extreme drawn exactly on a guide reads
/// as *shorter* than a flat letter that stops on the same line, so type
/// designers push it slightly past.  Applied here, after the knots are built, so
/// that both the shape (curve vs. point) and the neighbouring knots are known:
///
///   * a **round** extreme — a knot whose x coordinate is fitted (`t(c)`, the
///     flat top of a bowl) or which has a curve on at least one side — sitting
///     on the top, x-height, baseline or descender guide with both neighbours
///     strictly on one side of it, moves out by `overshoot` (O, S, C, o, e, 6…).
///   * a **pointed** extreme — a corner between two straight lines whose
///     neighbours lie on *opposite* sides horizontally, i.e. a genuine wedge —
///     moves out by `pointedOvershoot` (the apex of A, V, W, and the middle
///     vertex of M/W).
///
/// A corner where the strokes don't converge into a point (the top of M's left
/// stem, N's stem/diagonal junction) is left alone, as is any flat run, so
/// letters that legitimately stop on the guide keep stopping on it.
let private applyOvershoot (glyph: FontMetrics) isClosed (knots: list<Knot>) =
    let n = knots.Length

    if glyph.overshoot = 0.0 || n < 3 then
        knots
    else
        let arr = List.toArray knots
        let near a b = abs (a - b) < 0.001

        let onGuide y =
            near y glyph.T || near y glyph.X || near y glyph.B || near y glyph.D

        [ for i in 0 .. n - 1 do
              let k = arr.[i]
              let isInterior = isClosed || (i > 0 && i < n - 1)

              if not isInterior || not (onGuide k.pt.y) then
                  yield k
              else
                  let prev = arr.[(i + n - 1) % n].pt
                  let next = arr.[(i + 1) % n].pt

                  let dir =
                      if prev.y < k.pt.y && next.y < k.pt.y then 1.0
                      elif prev.y > k.pt.y && next.y > k.pt.y then -1.0
                      else 0.0

                  let isCurved =
                      k.pt.x_fit || k.ty = G2 || k.ty = LineToCurve || k.ty = CurveToLine

                  let isPointed =
                      not isCurved && (prev.x - k.pt.x) * (next.x - k.pt.x) < 0.0

                  let amount =
                      if dir = 0.0 then 0.0
                      elif isCurved then glyph.overshoot
                      elif isPointed then glyph.pointedOvershoot
                      else 0.0

                  if amount = 0.0 then
                      yield k
                  else
                      yield { k with pt = { k.pt with y = k.pt.y + dir * amount } } ]

let parse_curve (glyph: FontMetrics) raw_def debug =
    let mutable pts = []
    let mutable explicit_tangents = []
    let mutable corners = []
    let mutable joints = []
    let mutable labels = []
    let mutable seps_out = []
    let mutable def: string = raw_def

    while def.Length > 0 do
        let pt, tangent, isCorner, isJoint, label, new_def = parse_point glyph def
        def <- new_def

        pts <- pts @ [ pt ]
        explicit_tangents <- explicit_tangents @ [ tangent ]
        corners <- corners @ [ isCorner ]
        joints <- joints @ [ isJoint ]
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
                // An explicit corner keeps both tangents free so the solver picks the
                // curve's own natural direction out of (or into) the kink.
                | None when corners.[i] -> None
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

                  // At a kink the two sides are independent, so an explicit direction
                  // there names the tangent's *axis* and each side is oriented along its
                  // own direction of travel: into the point from the previous knot, out of
                  // it toward the next. `hllrEK` in '3' therefore means "horizontal in and
                  // out", giving a level waist where the stroke doubles back — writing the
                  // same East angle on both sides would instead ask the upper bowl to
                  // arrive travelling east while coming from the east, and it would loop.
                  let orientAtKink t (dx: float) (dy: float) =
                      let horizontal = abs (cos t) >= abs (sin t)
                      if horizontal then
                          if abs dx < 1e-9 then t elif dx > 0.0 then 0.0 else PI
                      else if abs dy < 1e-9 then t
                      elif dy > 0.0 then PI * 0.5
                      else PI * -0.5

                  let tIn, tOut =
                      match explicit_tangents.[i] with
                      | Some t ->
                          if not has_curve_in && not has_curve_out then
                              invalidArg "tangent" "Explicit tangents cannot be applied to points with only straight lines."
                          else
                              let tInAt, tOutAt =
                                  if corners.[i] then
                                      let prev = pts.[if i = 0 then n - 1 else i - 1]
                                      let next = pts.[if i = n - 1 then 0 else i + 1]
                                      orientAtKink t (pts.[i].x - prev.x) (pts.[i].y - prev.y),
                                      orientAtKink t (next.x - pts.[i].x) (next.y - pts.[i].y)
                                  else
                                      t, t

                              (if has_curve_in then Some tInAt else None),
                              (if has_curve_out then Some tOutAt else None)
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
                  // `K` forces a kink: tangent continuity is broken here even though the
                  // separators would otherwise imply a smooth line→curve (or curve→curve)
                  // transition.
                  if corners.[i] then ty <- Corner

                  { pt = pts.[i]; ty = ty; th_in = tIn; th_out = tOut; isJoint = joints.[i]; label = Some labels.[i] } ]
            |> mergeConsecutive
                (fun k -> System.Math.Round(k.pt.x, 3), System.Math.Round(k.pt.y, 3))
                (fun k1 k2 ->
                    let ty = if k1.ty = Corner || k2.ty = Corner then Corner else k2.ty

                    { k2 with
                        ty = ty
                        th_in = Option.orElse k1.th_in k2.th_in
                        th_out = Option.orElse k2.th_out k1.th_out
                        isJoint = k1.isJoint || k2.isJoint
                        label = Option.orElse k1.label k2.label })
            |> applyOvershoot glyph isClosed

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

let stringDefsToElemFromMap (map: Map<char, string>) (glyph: FontMetrics) e debug =
    let def = map.[e]
    assert Regex.IsMatch(def, glyph_re)

    if debug then
        printfn "%A: %A" e def

    parse_curves glyph def debug

let stringDefsToElem (glyph: FontMetrics) e debug =
    stringDefsToElemFromMap glyphMap glyph e debug

let rawDefToElem (glyph: FontMetrics) (rawDef: string) debug =
    try
        parse_curves glyph rawDef debug
    with _ ->
        Dot({ y = glyph.H; x = glyph.C; y_fit = false; x_fit = false })
