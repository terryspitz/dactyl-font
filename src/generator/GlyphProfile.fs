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

// ---------------------------------------------------------------------------
// Per-glyph optical spacing.
//
// Kerning is meant to be a sparse *correction*; the bulk of the work belongs in
// each glyph's own sidebearings, which live in the advance width and therefore
// survive into consumers that never apply kerning at all. Measuring both sides
// of a glyph once (O(n)) rather than every pair (O(n²)) is also what keeps the
// exported kern/GPOS table small.
//
// Model (as in area-based auto-spacers such as HTLetterspacer): the whitespace
// a reader perceives beside a glyph is the physical gap *plus* whatever the
// glyph's own silhouette gives away by receding from its extreme edge. Hold
// that sum constant and both `H` (flat, recedes nothing) and `A` (recedes a
// lot up top) end up looking equally spaced.
// ---------------------------------------------------------------------------

/// Extra room a *broad* contact needs over the bare target — see the long
/// comment on pairKern below. Defined up here because per-glyph spacing has to
/// aim at the same effective gap the pairwise model does, or every pair is left
/// with a constant residual and nothing is ever sparse.
let private maxSlackForBroadContact = 60.0
let private nearMinTolerance = 20.0

/// Depth past which a concavity stops counting. A deep notch — C's aperture,
/// the underside of T's arms — reads as enclosed counter-space rather than as
/// sidebearing, and must not buy unlimited tightening.
let private maxRecessionDepth = 120.0

/// How much of a side's own recession is charged against its sidebearing.
///
/// 0.5 is an empirical optimum, not a guess: swept over 0.0–1.5 against the
/// full charset, it minimises both the residual kern count and mean |kern|,
/// and both neighbours (0.35, 0.75) are worse on every measure. Turning it up
/// so per-glyph spacing does *more* of the work makes agreement worse, not
/// better — the pairwise model's broad-contact rule and a per-side recession
/// measure genuinely disagree about diagonals (A's right side recedes, yet
/// A/V reads as a broad contact needing more room, not less), and no single
/// weight reconciles them. That irreducible disagreement is precisely the
/// part that has to stay in the kern table.
let private recessionWeight = 0.5

/// Ceiling on what one side may give away, as a fraction of the target gap.
///
/// Without it the model has no floor: each side's recession is subtracted
/// independently, so two heavily-receding sides can between them subtract more
/// than the whole gap and the ink collides. Measured at spacing=100 before this
/// cap, T gave away 57.1 either side and I 48.6, closing T|T to -14.3, I|T to
/// -5.7 and L|I to exactly 0. Capping at a third-ish keeps the worst case
/// (two maximally-receding sides) at 1 - 2*0.35 = 30% of the target still
/// clear, and scales with `spacing` rather than being an absolute floor that
/// would go negative at tight settings.
let private maxGiveFraction = 0.35

/// What one side actually gives up: its weighted recession, but never more
/// than `maxGiveFraction` of the gap it is giving it up from.
let private sideGive (target: float) (recession: float) =
    min (recessionWeight * recession) (maxGiveFraction * target)

let private inkBands (p: GlyphProfile) =
    [| for i in 0 .. p.BandCount - 1 do
           if p.LeftEdges.[i] < posInf && p.RightEdges.[i] > negInf then yield i |]

/// (leftmost ink x, rightmost ink x, mean left recession, mean right recession).
/// Recessions are depth-limited means over bands carrying ink, measured back
/// from that side's own extreme — 0 for a flat side, large for a receding one.
let sideMetrics (p: GlyphProfile) : (float * float * float * float) option =
    if not p.HasInk then None
    else
        let bands = inkBands p
        if bands.Length = 0 then None
        else
            let inkLeft = bands |> Array.map (fun i -> p.LeftEdges.[i]) |> Array.min
            let inkRight = bands |> Array.map (fun i -> p.RightEdges.[i]) |> Array.max
            let lRec = bands |> Array.averageBy (fun i -> min maxRecessionDepth (p.LeftEdges.[i] - inkLeft))
            let rRec = bands |> Array.averageBy (fun i -> min maxRecessionDepth (inkRight - p.RightEdges.[i]))
            Some(inkLeft, inkRight, lRec, rRec)

/// How far to move a glyph off its spine origin so its left sidebearing is
/// optical rather than accidental: normalise the leftmost ink to x=0, then
/// inset by the whitespace the left silhouette already provides.
let opticalShift (target: float) (p: GlyphProfile) : float =
    match sideMetrics p with
    | None -> 0.0
    | Some(inkLeft, _, lRec, _) -> -inkLeft - sideGive target lRec

/// Advance width for a glyph spaced optically on both sides, so that a pair of
/// them placed by advance alone leaves this much *perceived* white:
///   gap = target - w*rRec(left) - w*lRec(right)
///
/// `target` is the flat-sided reference: two glyphs that recede nothing (H/H)
/// sit exactly `target` apart, and that is also what the pairwise model wants
/// for them, so they need no kern at all. Receding sides then give some back,
/// which is the optical idea.
let opticalAdvance (target: float) (p: GlyphProfile) : float option =
    match sideMetrics p with
    | None -> None
    | Some(inkLeft, inkRight, lRec, rRec) ->
        Some((inkRight - inkLeft) + target - sideGive target lRec - sideGive target rRec)

/// Optical kern between two glyphs.
/// Caller passes the advance of the left glyph; we shift the right glyph by
/// `kern` so the closest band-wise gap equals an *effective* target that
/// scales with how much of the profile is actually in close contact.
///
/// `target` is the `spacing` axis. That matters: because the returned kern is
/// `target - advanceA - deltaMin`, the placed position `advanceA + kern`
/// is independent of `advanceA` — every term the caller folded into the
/// advance width (spacing, sidebearing) is algebraically cancelled. Feeding
/// `spacing` back in as the target is what makes it control the gap again,
/// and it drops out of the kern *value* (the `+spacing` here cancels the one
/// inside `advanceA`), so changing tracking doesn't churn the kern table.
///
/// Geometry: with B's origin at x = advanceA + kern and bands b in [0, n):
///   gap(b) = (advanceA + kern + B.LeftEdges[b]) - A.RightEdges[b]
///
/// A hard min over gap(b) conflates two visually different situations: a
/// long parallel contact (e.g. the A/V diagonals, where most of the
/// overlapping bands sit at nearly the same close gap) reads as needing
/// MORE separation than a single tangent point (e.g. f's crossbar briefly
/// nearing j, where only one or two bands are that close) — perceived
/// space between glyphs is closer to an area than a single distance.
///
/// Fix: after finding the true minimum gap (deltaMin), measure what
/// fraction of the vertically-overlapping bands are "near" it (within
/// nearMinTolerance). A broad parallel contact (AV/AW diagonals, most of
/// the overlap is near-minimum) needs EXTRA room on top of `target` to
/// avoid reading as a collision; an isolated tangent point (fj/rn/fl,
/// only a band or two is that close) is fine at the bare `target` — adding
/// slack there would undo kerning that was already correct.
/// (nearMinTolerance / maxSlackForBroadContact are defined above, since
/// per-glyph optical spacing has to aim at the same effective target.)

/// Where B's origin wants to sit relative to A's, before either glyph's
/// optical shift — the placement that puts the closest band-wise gap at the
/// effective target. Independent of A's advance width by construction, which
/// is exactly why the advance has to be subtracted back off by the caller.
let private desiredOffset (target: float) (a: GlyphProfile) (b: GlyphProfile) : float option =
    if not a.HasInk || not b.HasInk || a.BandCount <> b.BandCount then None
    else
        let gaps =
            [| for i in 0 .. a.BandCount - 1 do
                   let ra = a.RightEdges.[i]
                   let lb = b.LeftEdges.[i]
                   if ra > negInf && lb < posInf then
                       yield lb - ra |]
        if gaps.Length = 0 then None
        else
            let deltaMin = Array.min gaps
            let nearMinCount = gaps |> Array.filter (fun g -> g <= deltaMin + nearMinTolerance) |> Array.length
            let fractionNearMin = float nearMinCount / float gaps.Length
            // Anchored at the FLAT end: a broad parallel contact gets the bare
            // `target`, and room is taken *away* as contact narrows to a point.
            // Anchoring at the point end instead made `spacing` mean the gap for
            // a tangent pair, so a flat pair sat at spacing+slack — i.e. the axis
            // meant something different here than on the fixed-spacing path.
            let effectiveTarget = target - maxSlackForBroadContact * (1.0 - fractionNearMin)
            Some(effectiveTarget - deltaMin)

/// Clip so a degenerate profile can't push glyphs through each other.
/// Widened from (-200, +80): the old ceiling was routinely binding on
/// flat-sided pairs (H/H wanted more than +80) because the kern was also
/// having to cancel `spacing` out of the advance. Now that `spacing` is the
/// target instead, the kern only covers real shape variation.
let private clipKern (raw: float) = int (System.Math.Round(max -250.0 (min 150.0 raw)))

let pairKern (target: float) (advanceA: float) (a: GlyphProfile) (b: GlyphProfile) : int =
    match desiredOffset target a b with
    | None -> 0
    | Some desired -> clipKern (desired - advanceA)

/// Pair kern remaining once per-glyph optical spacing has done its part.
/// Both glyphs are drawn at their own `opticalShift`, so the pair's ideal
/// origin separation is `desired + shiftA - shiftB`, of which the advance
/// width already delivers `advanceA`. Only the difference needs a kern.
///
/// Residuals under `threshold` are dropped to zero. That pruning is the point:
/// per-glyph spacing is supposed to leave most pairs needing nothing, and a
/// kern of a couple of units is below what anyone can see but still costs a
/// table entry in every exported font.
let residualKern (target: float) (advanceA: float) (shiftA: float) (shiftB: float)
                 (threshold: float) (a: GlyphProfile) (b: GlyphProfile) : int =
    match desiredOffset target a b with
    | None -> 0
    | Some desired ->
        let raw = desired + shiftA - shiftB - advanceA
        if abs raw < threshold then 0 else clipKern raw
