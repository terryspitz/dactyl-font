/// Runtime random-glyph generator: Propose -> Filter -> Assemble, sampling
/// from the stroke corpus harvested offline by tools/randomglyphs (see
/// StrokeCorpus.fs and docs/RandomGlyphs.md).
///
/// This module never touches Python or the corpus source files -- it only
/// reads StrokeCorpus.strokes/rolePatterns, so it compiles through Fable like
/// the rest of the generator and produces a fresh alphabet client-side on
/// every call, with no server or precomputed output involved.
module RandomGlyphs

open System.Text.RegularExpressions
open GeneratorTypes
open GlyphStringDefs

// ---------------------------------------------------------------------------
// Decode: corpus stroke definitions -> plain (x, y) points
// ---------------------------------------------------------------------------

/// A stroke reduced to plain (x, y) points and the separator following each
/// one ("" for an open stroke's final point, otherwise "-" or "~").
///
/// Offset ('o'/'e'), direction (N/S/E/W), corner ('K') and joint ('J')
/// modifiers are deliberately dropped here -- this mirrors the validated
/// prototype (docs/RandomGlyphs.md section 2): it costs a little polish (an
/// explicit kink, a flared terminal) but keeps decode and re-encode exactly
/// symmetric, which is what lets every generated point land back on a real
/// token in the corpus's own idiom.
type private DecodedStroke =
    { Points: (float * float) list
      Seps: string list
      Closed: bool }

/// Mirrors GlyphStringDefs.parse_curve's point/separator split: '-' and '~'
/// delimit points, and the trailing separator (if any) marks the stroke closed.
let private splitPoints (stroke: string) : (string * string) list =
    let parts = Regex.Split(stroke, "([-~])")

    [ for i in 0 .. 2 .. parts.Length - 1 do
          if parts.[i] <> "" then
              yield parts.[i], (if i + 1 < parts.Length then parts.[i + 1] else "") ]

/// Resolve one point token's (x, y) position, mirroring
/// GlyphStringDefs.parse_point's y/x weighted-average logic without applying
/// the offset -- see the DecodedStroke doc comment above.
let private decodePoint (metrics: FontMetrics) (tok: string) : (float * float) option =
    let yOf c =
        match c with
        | 't' -> metrics.T
        | 'x' -> metrics.X
        | 'h' -> metrics.H
        | 'b' -> metrics.B
        | 'd' -> metrics.D
        | _ -> nan

    let xOf c =
        match c with
        | 'l' -> metrics.L
        | 'c' -> metrics.C
        | 'r' -> metrics.R
        | 'w' -> metrics.W
        | _ -> nan

    let matchY = Regex.Match(tok, "^" + y_re)

    if not matchY.Success then
        None
    else
        let rest = tok.[matchY.Length ..]

        let rest =
            let matchOffset = Regex.Match(rest, "^" + offset_re)
            if matchOffset.Success then rest.[matchOffset.Length ..] else rest

        let matchX = Regex.Match(rest, "^" + x_re)

        if not matchX.Success then
            None
        else
            let ys = weightedCoords matchY.Value yOf
            let xs = weightedCoords matchX.Value xOf

            if (ys |> List.exists System.Double.IsNaN) || (xs |> List.exists System.Double.IsNaN) then
                None
            else
                Some(List.average xs, List.average ys)

let private decodeStroke (metrics: FontMetrics) (stroke: string) : DecodedStroke option =
    let toks = splitPoints stroke

    if toks.IsEmpty then
        None
    else
        let decoded =
            toks |> List.map (fun (p, s) -> decodePoint metrics p |> Option.map (fun pt -> pt, s))

        if decoded |> List.exists Option.isNone then
            None
        else
            let decoded = decoded |> List.map Option.get
            let pts = decoded |> List.map fst
            let seps = decoded |> List.map snd

            let closed =
                match List.tryLast seps with
                | Some "-" | Some "~" -> true
                | _ -> false

            Some { Points = pts; Seps = seps; Closed = closed }

// ---------------------------------------------------------------------------
// Coordinate codebook: every expression the language can produce, resolved
// against the current metrics, so a moved point always re-encodes as a real,
// already-legal token rather than a raw number.
// ---------------------------------------------------------------------------

let private buildCodebook (letters: (char * float) list) : (string * float) list =
    let sorted = letters |> List.sortBy fst
    let n = sorted.Length

    [ for (c, v) in sorted -> string c, v
      for i in 0 .. n - 1 do
          for j in i + 1 .. n - 1 do
              let a, va = sorted.[i]
              let b, vb = sorted.[j]

              for m in 1 .. 4 do
                  for k in 1 .. 4 do
                      let expr =
                          if m = 1 && k = 1 then sprintf "%c%c" a b
                          elif k = 1 then sprintf "%c%d%c" a m b
                          elif m = 1 then sprintf "%c%c%d" a b k
                          else sprintf "%c%d%c%d" a m b k

                      yield expr, (float m * va + float k * vb) / float (m + k) ]

let private yCodebook (metrics: FontMetrics) =
    buildCodebook [ 't', metrics.T; 'x', metrics.X; 'h', metrics.H; 'b', metrics.B; 'd', metrics.D ]

let private xCodebook (metrics: FontMetrics) =
    buildCodebook [ 'l', metrics.L; 'c', metrics.C; 'r', metrics.R; 'w', metrics.W ]

/// Nearest codebook entry to `target`, preferring the shorter token on a tie
/// (mirrors the validated prototype's snap function exactly).
let private snap (book: (string * float) list) (target: float) : string =
    book |> List.minBy (fun (_, v) -> abs (v - target)) |> fst

// ---------------------------------------------------------------------------
// Propose: sample a role pattern, draw a stroke per role, place it so it
// touches what's already placed (connected by construction), transform.
// ---------------------------------------------------------------------------

type private PlacedStroke =
    { Points: (float * float) list
      Seps: string list
      Closed: bool }

let private buildInventory (metrics: FontMetrics) : Map<string, (string * DecodedStroke)[]> =
    StrokeCorpus.strokes
    |> List.choose (fun (role, source, def) -> decodeStroke metrics def |> Option.map (fun d -> role, (source, d)))
    |> List.groupBy fst
    |> List.map (fun (role, xs) -> role, xs |> List.map snd |> Array.ofList)
    |> Map.ofList

let private choice (rng: System.Random) (xs: 'a[]) = xs.[rng.Next(xs.Length)]

/// Sample a role pattern weighted by how many source glyphs used it, so
/// generated glyphs favour the combinations real letters actually favour
/// (a lone `arc` far more often than a four-stroke pileup).
let private samplePattern (rng: System.Random) (patterns: (string list * int)[]) : string list =
    let total = patterns |> Array.sumBy snd
    let r = rng.Next(total)

    let rec go acc i =
        if i >= patterns.Length - 1 then
            fst patterns.[patterns.Length - 1]
        else
            let _, w = patterns.[i]
            if r < acc + w then fst patterns.[i] else go (acc + w) (i + 1)

    go 0 0

let private mirrorX (metrics: FontMetrics) (pts: (float * float) list) =
    pts |> List.map (fun (x, y) -> metrics.R - x, y)

let private translate (dx, dy) (pts: (float * float) list) =
    pts |> List.map (fun (x, y) -> x + dx, y + dy)

/// Propose one glyph: sample a role pattern, draw a stroke per role from the
/// corpus, and place each one (after the first) so an endpoint lands exactly
/// on a point already placed. That is what makes connectivity a property of
/// construction rather than something the filter has to catch after the
/// fact -- see docs/RandomGlyphs.md section 2.
let private proposeGlyph
    (rng: System.Random)
    (metrics: FontMetrics)
    (inventory: Map<string, (string * DecodedStroke)[]>)
    (patterns: (string list * int)[])
    : PlacedStroke list option =
    let combo = samplePattern rng patterns

    if combo.Length > 3 then
        None
    else
        let mutable ok = true
        let mutable placed: PlacedStroke list = []

        for role in combo do
            if ok then
                match inventory.TryFind role with
                | None -> ok <- false
                | Some options ->
                    let _source, stroke = choice rng options

                    let pts0 =
                        if rng.NextDouble() < 0.35 then
                            mirrorX metrics stroke.Points
                        else
                            stroke.Points

                    match placed with
                    | [] -> placed <- [ { Points = pts0; Seps = stroke.Seps; Closed = stroke.Closed } ]
                    | _ ->
                        let anchors = placed |> List.collect (fun s -> s.Points) |> Array.ofList
                        let anchor = choice rng anchors

                        let srcCandidates =
                            if stroke.Closed then
                                pts0 |> Array.ofList
                            else
                                [| List.head pts0; List.last pts0 |]

                        let src = choice rng srcCandidates
                        let dx, dy = fst anchor - fst src, snd anchor - snd src
                        let moved = translate (dx, dy) pts0
                        placed <- placed @ [ { Points = moved; Seps = stroke.Seps; Closed = stroke.Closed } ]

        if ok then Some placed else None

// ---------------------------------------------------------------------------
// Filter: reject the failure modes actually observed when rendering samples
// (docs/RandomGlyphs.md section 2). Thresholds are the validated prototype's,
// expressed as ratios of the current metrics so they scale with any axes
// rather than being pinned to one default width/height.
// ---------------------------------------------------------------------------

let private segLen (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2.0 + (y2 - y1) ** 2.0)

let private turnAngleDeg (px, py) (qx, qy) (rx, ry) =
    let a = atan2 (qy - py) (qx - px)
    let b = atan2 (ry - qy) (rx - qx)
    let d = abs ((b - a) * 180.0 / System.Math.PI) % 360.0
    min d (360.0 - d)

let private ring (s: PlacedStroke) =
    if s.Closed then s.Points @ [ List.head s.Points ] else s.Points

let private filtersOk (metrics: FontMetrics) (strokes: PlacedStroke list) : bool =
    let allPts = strokes |> List.collect (fun s -> s.Points)

    if allPts.IsEmpty then
        false
    else
        let xs = allPts |> List.map fst
        let ys = allPts |> List.map snd
        let xMin, xMax = List.min xs, List.max xs
        let yMin, yMax = List.min ys, List.max ys
        let bboxSlackX = 0.1 * metrics.R
        let yMinSlack = metrics.T * 20.0 / 600.0
        let yMaxSlack = metrics.T * 40.0 / 600.0

        if xMin < -bboxSlackX || xMax > metrics.W + bboxSlackX then
            false
        elif yMin < metrics.D - yMinSlack || yMax > metrics.T + yMaxSlack then
            false
        elif (xMax - xMin) < 0.2 * metrics.R || (yMax - yMin) < 0.25 * metrics.T then
            false
        else
            let sliverMin = 0.073 * metrics.R
            let segMin = 0.2 * metrics.R
            let mutable ink = 0.0
            let mutable npts = 0
            let mutable bad = false

            for s in strokes do
                npts <- npts + s.Points.Length
                let r = ring s

                for i in 0 .. r.Length - 2 do
                    let l = segLen r.[i] r.[i + 1]
                    if s.Points.Length > 1 && l < sliverMin then bad <- true
                    ink <- ink + l

                for i in 1 .. r.Length - 2 do
                    if
                        turnAngleDeg r.[i - 1] r.[i] r.[i + 1] < 16.0
                        && segLen r.[i - 1] r.[i] > segMin
                        && segLen r.[i] r.[i + 1] > segMin
                    then
                        bad <- true

            if bad then false
            else ink >= 1.4 * metrics.R && npts >= 3 && npts <= 15

// ---------------------------------------------------------------------------
// Assemble: keep an alphabet mutually distinct rather than a bag of
// independent samples (docs/RandomGlyphs.md section 2, stage 3).
// ---------------------------------------------------------------------------

let private signature (metrics: FontMetrics) (strokes: PlacedStroke list) : Set<int * int> =
    let cell = 0.25 * metrics.R
    let step = 0.083 * metrics.R
    let mutable s = Set.empty

    for st in strokes do
        let r = ring st

        for i in 0 .. r.Length - 2 do
            let x1, y1 = r.[i]
            let x2, y2 = r.[i + 1]
            let n = max 2 (int (segLen r.[i] r.[i + 1] / step))

            for k in 0 .. n do
                let t = float k / float n
                let x = x1 + (x2 - x1) * t
                let y = y1 + (y2 - y1) * t
                s <- Set.add (int (floor (x / cell)), int (floor (y / cell))) s

    s

let private jaccard (a: Set<int * int>) (b: Set<int * int>) =
    let union = Set.union a b |> Set.count
    if union = 0 then 0.0 else float (Set.intersect a b |> Set.count) / float union

// ---------------------------------------------------------------------------
// Re-encode: points -> real glyph-string tokens via the codebook.
// ---------------------------------------------------------------------------

let private renderStroke (yBook, xBook) (s: PlacedStroke) : string =
    let toks =
        s.Points
        |> List.mapi (fun i (x, y) ->
            let sep = if i < s.Seps.Length then s.Seps.[i] else ""
            snap yBook y + snap xBook x + sep)

    let joined = String.concat "" toks

    if s.Closed && not (joined.EndsWith("-") || joined.EndsWith("~")) then
        joined + "~"
    else
        joined

let private renderGlyph codebooks (strokes: PlacedStroke list) : string =
    strokes |> List.map (renderStroke codebooks) |> String.concat " "

/// Generate up to `count` fresh, filtered, mutually-distinct glyph
/// definitions sampled from the corpus, at the given axes' scale and RNG
/// seed. May return fewer than `count` if the try budget is exhausted first.
///
/// Every candidate is round-tripped through the real parser
/// (`rawDefToElem`) before acceptance and rejected if it comes back as a
/// `Dot` -- that function swallows every parse exception and returns one, so
/// "parses to a Dot" has to be treated as a rejection rather than a success
/// (docs/RandomGlyphs.md; the same rule StrokeCorpusTests applies to the
/// harvested corpus itself).
let generateAlphabetDefs (axes: Axes.Axes) (seed: int) (count: int) : string list =
    let metrics = FontMetrics(axes)
    let rng = System.Random(seed)
    let inventory = buildInventory metrics

    let patterns =
        StrokeCorpus.rolePatterns
        |> List.filter (fun (p, _) -> p |> List.forall inventory.ContainsKey)
        |> Array.ofList

    if patterns.Length = 0 then
        []
    else
        let codebooks = yCodebook metrics, xCodebook metrics
        let maxTries = max 20000 (count * 800)
        let mutable accepted: string list = []
        let mutable sigs: Set<int * int> list = []
        let mutable tries = 0

        while accepted.Length < count && tries < maxTries do
            tries <- tries + 1

            match proposeGlyph rng metrics inventory patterns with
            | None -> ()
            | Some strokes ->
                if filtersOk metrics strokes then
                    let sg = signature metrics strokes

                    if sigs |> List.forall (fun o -> jaccard sg o <= 0.55) then
                        let def = renderGlyph codebooks strokes

                        if not (System.String.IsNullOrWhiteSpace def) then
                            let elem = GlyphStringDefs.rawDefToElem metrics def false

                            let isDegenerate =
                                match elem with
                                | Dot _ -> true
                                | _ -> false

                            if not isDegenerate then
                                accepted <- accepted @ [ def ]
                                sigs <- sigs @ [ sg ]

        accepted
