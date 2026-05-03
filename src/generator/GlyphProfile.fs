// Edge-profile sampler for optical kerning.
//
// Given a glyph's outline (as the same SVG path string that fontExport.js
// parses), discretise into line segments and bucket the leftmost / rightmost
// ink x-coordinate into y-bands. Pair kerning then minimises the gap across
// bands where both glyphs have ink.
module GlyphProfile

type Cmd =
    | M of float * float
    | L of float * float
    | C of float * float * float * float * float * float
    | Z

/// Parse `M x,y\nL x,y\nC cx1,cy1 cx2,cy2 ex,ey\nZ` into a Cmd list.
/// `Q` quad curves are converted to cubics by elevation.
let parseSvgCommands (path: string) : Cmd list =
    if System.String.IsNullOrWhiteSpace(path) then [] else
    let toks = path.Split([| ' '; '\n'; '\r'; '\t' |], System.StringSplitOptions.RemoveEmptyEntries)
    let parsePt (s: string) =
        let comma = s.IndexOf(',')
        if comma < 0 then 0.0, 0.0
        else
            let xs = s.Substring(0, comma)
            let ys = s.Substring(comma + 1)
            float xs, float ys
    let mutable i = 0
    let result = ResizeArray<Cmd>()
    while i < toks.Length do
        match toks.[i] with
        | "M" | "m" when i + 1 < toks.Length ->
            let x, y = parsePt toks.[i+1]
            i <- i + 2
            result.Add(M(x, y))
        | "L" | "l" when i + 1 < toks.Length ->
            let x, y = parsePt toks.[i+1]
            i <- i + 2
            result.Add(L(x, y))
        | "C" | "c" when i + 3 < toks.Length ->
            let x1, y1 = parsePt toks.[i+1]
            let x2, y2 = parsePt toks.[i+2]
            let x3, y3 = parsePt toks.[i+3]
            i <- i + 4
            result.Add(C(x1, y1, x2, y2, x3, y3))
        | "Z" | "z" ->
            i <- i + 1
            result.Add(Z)
        | _ -> i <- i + 1
    List.ofSeq result

type GlyphProfile =
    { BandY0: float
      BandHeight: float
      BandCount: int
      LeftEdges: float[]   // min-x at band b, +inf if empty
      RightEdges: float[]  // max-x at band b, -inf if empty
      HasInk: bool }

let private posInf = System.Double.PositiveInfinity
let private negInf = System.Double.NegativeInfinity

let inline private updateBand (lefts: float[]) (rights: float[]) (b: int) (x: float) =
    if x < lefts.[b] then lefts.[b] <- x
    if x > rights.[b] then rights.[b] <- x

/// Sample one line segment (x0,y0)->(x1,y1) into the band arrays.
let private sampleSegment (bandY0: float) (bandHeight: float) (bandCount: int)
                          (lefts: float[]) (rights: float[])
                          (x0: float) (y0: float) (x1: float) (y1: float) =
    let yLo = min y0 y1
    let yHi = max y0 y1
    let bLo = max 0 (int (floor ((yLo - bandY0) / bandHeight)))
    let bHi = min (bandCount - 1) (int (floor ((yHi - bandY0) / bandHeight)))
    if bLo > bHi then ()
    elif y0 = y1 then
        // horizontal: contribute both endpoints
        let bb = max 0 (min (bandCount - 1) bLo)
        updateBand lefts rights bb x0
        updateBand lefts rights bb x1
    else
        for b in bLo .. bHi do
            let yMid = bandY0 + (float b + 0.5) * bandHeight
            if yMid >= yLo && yMid <= yHi then
                let t = (yMid - y0) / (y1 - y0)
                let x = x0 + t * (x1 - x0)
                updateBand lefts rights b x
        // also include endpoints (catches band-aligned vertices)
        let bb0 = max 0 (min (bandCount - 1) (int (floor ((y0 - bandY0) / bandHeight))))
        let bb1 = max 0 (min (bandCount - 1) (int (floor ((y1 - bandY0) / bandHeight))))
        updateBand lefts rights bb0 x0
        updateBand lefts rights bb1 x1

/// Build a profile from parsed commands. y-range is [bandY0, bandY1].
let sampleProfile (bandY0: float) (bandY1: float) (bandCount: int) (cmds: Cmd list) : GlyphProfile =
    let bandHeight = (bandY1 - bandY0) / float bandCount
    let lefts = Array.create bandCount posInf
    let rights = Array.create bandCount negInf
    let mutable hasInk = false
    let mutable cx, cy = 0.0, 0.0
    let mutable startX, startY = 0.0, 0.0
    let cubicSamples = 12
    let bezierAt x0 y0 x1 y1 x2 y2 x3 y3 (t: float) =
        let mt = 1.0 - t
        let mt2 = mt * mt
        let t2 = t * t
        let x = mt2*mt*x0 + 3.0*mt2*t*x1 + 3.0*mt*t2*x2 + t2*t*x3
        let y = mt2*mt*y0 + 3.0*mt2*t*y1 + 3.0*mt*t2*y2 + t2*t*y3
        x, y
    for cmd in cmds do
        match cmd with
        | M(x, y) ->
            cx <- x; cy <- y
            startX <- x; startY <- y
        | L(x, y) ->
            sampleSegment bandY0 bandHeight bandCount lefts rights cx cy x y
            hasInk <- true
            cx <- x; cy <- y
        | C(x1, y1, x2, y2, x3, y3) ->
            let mutable px, py = cx, cy
            for i in 1 .. cubicSamples do
                let t = float i / float cubicSamples
                let nx, ny = bezierAt cx cy x1 y1 x2 y2 x3 y3 t
                sampleSegment bandY0 bandHeight bandCount lefts rights px py nx ny
                px <- nx; py <- ny
            hasInk <- true
            cx <- x3; cy <- y3
        | Z ->
            sampleSegment bandY0 bandHeight bandCount lefts rights cx cy startX startY
            cx <- startX; cy <- startY
    { BandY0 = bandY0
      BandHeight = bandHeight
      BandCount = bandCount
      LeftEdges = lefts
      RightEdges = rights
      HasInk = hasInk }

/// Optical kern between two glyphs.
/// Caller passes the advance of the left glyph; we shift the right glyph by
/// `kern` so the minimum band-wise gap equals `target`.
///
/// Geometry: with B's origin at x = advanceA + kern and bands b in [0, n):
///   gap(b) = (advanceA + kern + B.LeftEdges[b]) - A.RightEdges[b]
/// Solving min_b gap(b) = target gives:
///   kern = target - advanceA - min_b (B.LeftEdges[b] - A.RightEdges[b])
let pairKern (target: float) (advanceA: float) (a: GlyphProfile) (b: GlyphProfile) : int =
    if not a.HasInk || not b.HasInk || a.BandCount <> b.BandCount then 0
    else
        let mutable deltaMin = posInf
        for i in 0 .. a.BandCount - 1 do
            let ra = a.RightEdges.[i]
            let lb = b.LeftEdges.[i]
            if ra > negInf && lb < posInf then
                let d = lb - ra
                if d < deltaMin then deltaMin <- d
        if System.Double.IsPositiveInfinity(deltaMin) then 0
        else
            let raw = target - advanceA - deltaMin
            // clip to a sane range so a degenerate profile can't push glyphs through each other
            let clipped = max -200.0 (min 80.0 raw)
            int (System.Math.Round(clipped))
