module GeneratorTypes

open SpiroPointType
open SpiroSegment
open SpiroControlPoint


type Point =
    { y: float
      x: float
      y_fit: bool
      x_fit: bool }

    member this.GetXY = this.x, this.y

    static member (+)(lhs: Point, rhs: Point) =
        { y = lhs.y + rhs.y
          x = lhs.x + rhs.x
          y_fit = lhs.y_fit || rhs.y_fit
          x_fit = lhs.x_fit || rhs.x_fit }

    static member (-)(lhs: Point, rhs: Point) =
        { y = lhs.y - rhs.y
          x = lhs.x - rhs.x
          y_fit = lhs.y_fit || rhs.y_fit
          x_fit = lhs.x_fit || rhs.x_fit }



/// Class defining important Font guidelines (x, y points) based on axes.
type FontMetrics(axes: Axes.Axes) =
    // X axis guides, from left
    member this.L = 0.0 // Left
    member this.R = float axes.width // Right = standard glyph width
    member this.N = float (axes.width * 4 / 5) // Narrow glyph width
    member this.C = float axes.width / 2.0 // Centre
    member this.monospaceWidth = this.N
    member this.W = float (axes.width * 3 / 2) // Wide glyph width

    // Y axis guides, from bottom-up
    member this.B = 0.0 // Bottom
    member this.X = axes.x_height * float axes.height // x-height
    member this.M = this.X / 2.0 // Midway down from x-height
    member this.T = float axes.height // Top = standard glyph caps height
    member this.H = this.T / 2.0 // Half total height
    member this.D = -axes.descender_depth * float axes.height // descender height

    member this.offset = float axes.roundedness // offset from corners
    member this.dotHeight = max ((this.X + this.T) / 2.0) (this.X + float axes.weight * 3.0)

    /// Optical correction "overshoot": how far a round or pointed extreme should
    /// project past a flat guide (top, x-height, baseline, descender) so that it
    /// doesn't read as shorter than a flat-topped letter beside it.
    member this.overshoot = float axes.overshoot

    /// Overshoot for extremes that converge to a point (the apex of A/V/W/M)
    /// rather than a curve.  Points look smaller still than circles, so they
    /// customarily get a little more of it.
    member this.pointedOvershoot = this.overshoot * 1.5

    /// Optical correction "balance": how far to raise a height that lies
    /// *between* the guides — a crossbar or a bowl waist — above where the
    /// arithmetic puts it, so the letter doesn't look bottom-heavy.  Heights
    /// on a guide itself never move; the raise fades smoothly to nothing at the
    /// baseline and at cap height, and peaks at the half height, so a bar drawn
    /// at `h` moves by the full amount and one at `bh` by about 70% of it.
    member this.balanceRaise(y: float) =
        if axes.balance = 0 || y <= this.B || y >= this.T then
            0.0
        else
            float axes.balance * sin (System.Math.PI * (y - this.B) / (this.T - this.B))

    member this.thickness = float axes.weight

type Knot =
    { pt: Point
      ty: SpiroPointType
      th_in: float option
      th_out: float option
      // Explicitly-declared interior joint (via the `J` marker in the glyph
      // string language). When true, an open-stroke endpoint at this knot
      // suppresses its cap (serif/flare/bulb) regardless of the geometric
      // joint heuristic — see Font.isJointRaw.
      isJoint: bool
      label: string option }

type SCP = SpiroControlPoint

type Element =
    | Glyph of c: char
    // Curve with optional incoming/outgoing tangent pair per knot and open/closed flag
    | Curve of knots: list<Knot> * isClosed: bool
    | Dot of Point
    | EList of list<Element>
    | Space

// Helpers to create Curve from simpler point lists
let withNoTangents pts =
    List.map (fun (p, t) -> { pt = p; ty = t; th_in = None; th_out = None; isJoint = false; label = None }) pts

let openCurve pts = Curve(withNoTangents pts, false)
let closedCurve pts = Curve(withNoTangents pts, true)

let mergeConsecutive keyFn mergeFn list =
    let rec loop =
        function
        | x :: y :: rest ->
            if keyFn x = keyFn y then
                loop ((mergeFn x y) :: rest)
            else
                x :: loop (y :: rest)
        | other -> other

    loop list

let rec movePoints fn (fnTan: (float -> float) option) (e: Element) =
    match e with
    | Curve(pts, isClosed) ->
        Curve(
            [ for k in pts do
                  { k with
                      pt = fn k.pt
                      th_in = k.th_in |> Option.map (fun t -> match fnTan with Some f -> f t | None -> t)
                      th_out = k.th_out |> Option.map (fun t -> match fnTan with Some f -> f t | None -> t) } ],
            isClosed
        )
    | Dot(p) -> Dot(fn p)
    | EList(elems) -> EList(List.map (movePoints fn fnTan) elems)
    | Space -> Space
    | Glyph(_) -> e // Cannot move points of an unreduced glyph without font metrics

let applyIf b f = if b then f else id

type SpiroElement =
    | SpiroOpenCurve of segments: list<SpiroSegment>
    | SpiroClosedCurve of segments: list<SpiroSegment>
    | SpiroDot of Point
    | SpiroSpace

/// Minimal solved-knot data needed for outline offsetting.
/// Populated either from Spiro/Spline2 (via SpiroSegment) or DactylSpline (via BezierPoint).
type Segment =
    { X: float
      Y: float
      tangentStart: float // entry tangent at this knot
      tangentEnd: float // exit tangent from this knot
      seg_ch: float // chord length to next knot
      Type: SpiroPointType }

// Bring into local namespace, and rename if nec.
let CurveToLine = SpiroPointType.Left
let LineToCurve = SpiroPointType.Right
let G2 = SpiroPointType.G2
let G4 = SpiroPointType.G4
let Corner = SpiroPointType.Corner
let Anchor = SpiroPointType.Anchor
let Handle = SpiroPointType.Handle

/// Validates that consecutive knot types agree on whether each connecting segment
/// is a curve or a straight line. Throws ArgumentException on any mismatch.
/// isClosed: if true, also checks the wrap-around from the last knot back to the first.
let validateKnotSequence (knots: list<Knot>) (isClosed: bool) =
    let n = knots.Length
    if n >= 2 then
        // None = unconstrained (Corner, Anchor, Handle…) — any segment kind is acceptable.
        let departing ty =
            if ty = G2 || ty = G4 || ty = LineToCurve then Some true  // curve departs
            elif ty = CurveToLine then Some false                      // line departs
            else None

        let arriving ty =
            if ty = G2 || ty = G4 || ty = CurveToLine then Some true  // curve arrives
            elif ty = LineToCurve then Some false                      // line arrives
            else None

        let typeName ty =
            if ty = CurveToLine then "CurveToLine"
            elif ty = LineToCurve then "LineToCurve"
            else sprintf "%A" ty

        let checkPair i j =
            let ki = knots.[i]
            let kj = knots.[j]
            match departing ki.ty, arriving kj.ty with
            | Some true, Some false ->
                invalidArg "knots"
                    (sprintf "Invalid knot sequence at %d→%d: %s departs a curve but %s expects a line to arrive"
                        i j (typeName ki.ty) (typeName kj.ty))
            | Some false, Some true ->
                invalidArg "knots"
                    (sprintf "Invalid knot sequence at %d→%d: %s departs a line but %s expects a curve to arrive"
                        i j (typeName ki.ty) (typeName kj.ty))
            | _ -> ()

        for i in 0 .. n - 2 do
            checkPair i (i + 1)
        if isClosed then
            checkPair (n - 1) 0

let rec bounds elem =
    let dummy = -999.0
 
    let safeMinMax mm x y =
        if x = dummy then y
        elif y = dummy then x
        else mm x y
 
    let bound3 minmax fstsnd (pts: Knot list) =
        List.fold minmax dummy (List.map (fun k -> fstsnd (k.pt.x, k.pt.y)) pts)
 
    let combineBounds (l1, r1, b1, t1) (l2, r2, b2, t2) =
        safeMinMax min l1 l2, safeMinMax max r1 r2, safeMinMax min b1 b2, safeMinMax max t1 t2
 
    match elem with
    | Curve(pts: list<Knot>, _) ->
        bound3 (safeMinMax min) fst pts,
        bound3 (safeMinMax max) fst pts,
        bound3 (safeMinMax min) snd pts,
        bound3 (safeMinMax max) snd pts
    | Dot(p) -> p.x, p.x, p.y, p.y
    | EList(elems) -> List.fold combineBounds (dummy, dummy, dummy, dummy) (List.map bounds elems)
    | Space -> 0.0, 0.0, 0.0, 0.0
    | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

let translateBy dx dy =
    // Preserve x_fit/y_fit: a free (to-be-solved) coordinate must stay free after a
    // translation. Clobbering the flags to false froze free coordinates at their
    // bracket-placeholder value, so glyphs with free coords (e.g. 'c') were never
    // solved in the Font/Glyph/Proofs tabs (which translate via translateByThickness),
    // while the Splines tab — which skips translation — solved them correctly.
    let shift (p: Point) =
        { p with
            y = p.y + dy
            x = p.x + dx }

    movePoints shift None


/// Everything known about one arc-length sample of a solved spine, handed to the
/// pen so a width or displacement term can depend on where it is along the
/// stroke, which way the stroke is heading, how sharply it is turning, and
/// which of the two edges is being emitted.
type PenSample =
    { /// arc length from the start of the curve, in font units
      s: float
      /// s as a fraction of the curve's total length
      sFrac: float
      /// spine tangent angle at this sample
      th: float
      /// signed curvature (1/radius) of the spine here
      curvature: float
      /// +1 on the outer edge, -1 on the inner one; lets a term differ per side
      side: float }

/// A pen is what turns a solved spine into ribbons.  Every artistic axis that
/// varies along a stroke is a term in one of these functions, so they compose by
/// construction: width terms multiply, displacement terms add.
///
/// Built per curve rather than per font because several terms need the curve's
/// total arc length (see Pen.ofAxes).
type Pen =
    { /// Perpendicular displacement of the spine itself, in font units, paired
      /// with its slope d(offset)/ds.  The slope is what keeps the offsets
      /// perpendicular to the *displaced* spine rather than the original one.
      displace: PenSample -> float * float
      /// Ribbon half-width before any per-side jitter.  Separate from halfWidth
      /// because the mobius panels scale this rather than the roughened width.
      baseHalfWidth: PenSample -> float
      /// Ribbon half-width as the body edges use it: baseHalfWidth plus any
      /// per-side jitter.
      halfWidth: PenSample -> float
      /// Ribbon twist angle; apparent width scales by |cos twist|.
      twist: PenSample -> float
      /// One entry per parallel ribbon: its lateral centre offset from the spine
      /// (font units), its width as a fraction of the pen's, and how much arc
      /// length to trim off each of its ends.  A solid stroke is the single
      /// entry (0, 1, 0), so the renderer only has the one code path.
      traces: (float * float * float) list }

module Pen =

    let private wobbleWavelength = 200.0
    let private roughnessWavelength = 40.0
    let private mobiusHalfTwistLen = 300.0

    /// Deterministic pseudo-noise from a few incommensurate sine harmonics, so a
    /// stroke edge jitters without a repeating pattern.  `side` shifts the phase
    /// so the two edges jitter independently, giving an uneven width (unlike
    /// wobble, which displaces the whole spine and keeps the width constant).
    let noise (sLen: float) (side: float) =
        let s = sLen + side * 129.0
        let n1 = sin (2.0 * System.Math.PI * s / roughnessWavelength)
        let n2 = sin (2.0 * System.Math.PI * s * 2.19 / roughnessWavelength + 1.3)
        let n3 = sin (2.0 * System.Math.PI * s * 4.73 / roughnessWavelength + 3.1)
        (n1 + 0.5 * n2 + 0.25 * n3) / 1.75

    /// Number of mobius half-twists over a curve of this length.
    let halfTwists (mobius: float) (totalLen: float) =
        max 1 (int (System.Math.Round(totalLen * mobius / mobiusHalfTwistLen)))

    /// Build the pen implied by an axis set, for one curve.
    ///
    /// `totalLen` and `isClosed` describe the curve the pen will be run along:
    /// taper is a fraction of the whole stroke, and wobble fits a whole number of
    /// cycles into it so the displacement vanishes at open ends (letting caps
    /// meet the body) and stays continuous around closed ones.
    let ofAxes (axes: Axes.Axes) (thickness: float) (totalLen: float) (isClosed: bool) : Pen =
        let PI = System.Math.PI
        let nib = axes.nib
        let nibAngle = float axes.nib_angle * PI / 180.0
        let taper = axes.taper
        let taperEnd = axes.taper_end
        let wobble = axes.wobble
        let roughness = axes.roughness
        let mobius = axes.mobius
        let pressure = axes.pressure
        let inkSpread = axes.ink_spread
        let gravity = axes.gravity

        let wobbleCycles = max 1.0 (System.Math.Round(totalLen / wobbleWavelength))

        // wobble=1.0 displaces the spine by half a stroke-thickness at the peaks.
        let amp = wobble * thickness * 0.5

        // How far a sagging stroke droops at its middle, at full gravity.
        let sagAmp = gravity * thickness * 0.9

        let displace (p: PenSample) =
            let wobbleD, wobbleSlope =
                if wobble = 0.0 then
                    0.0, 0.0
                else
                    let phase = 2.0 * PI * wobbleCycles * p.s / totalLen
                    amp * sin phase, amp * 2.0 * PI * wobbleCycles / totalLen * cos phase

            let gravityD, gravitySlope =
                if gravity = 0.0 then
                    0.0, 0.0
                else
                    // Sag is downward in the glyph, not perpendicular to the stroke, so
                    // project world-down onto the perpendicular: the normal is
                    // (-sin th, cos th), so a downward pull of g contributes -g cos th.
                    // A horizontal stroke sags fully, a vertical one not at all — which
                    // is what a tiring hand actually does. The tangential remainder only
                    // slides points along the curve and cannot change its shape.
                    let profile = sin (PI * p.sFrac)
                    let dProfile = PI / totalLen * cos (PI * p.sFrac)
                    -sagAmp * profile * cos p.th, -sagAmp * dProfile * cos p.th

            wobbleD + gravityD, wobbleSlope + gravitySlope

        /// Broad-nib width factor for a stroke running at angle th: 1.0 when the
        /// stroke is perpendicular to the nib, shrinking toward 0.05 when it runs
        /// along the nib.  Clamped so the outline never degenerates completely.
        let nibFactor th = max (1.0 - nib * (1.0 - abs (sin (th - nibAngle)))) 0.05

        let baseHalfWidth (p: PenSample) =
            let mutable w = thickness

            if nib > 0.0 then
                w <- w * nibFactor p.th

            if taper > 0.0 && not isClosed then
                // Brush ends: narrow over the first and last (taper/2) fraction of
                // the stroke, down to taper_end of full width at the very tips
                // (taper_end = 0 gives a sharp point).
                let ramp = 0.5 * taper
                let rampF = min 1.0 (min p.sFrac (1.0 - p.sFrac) / ramp)
                w <- w * (taperEnd + (1.0 - taperEnd) * rampF)

            if pressure > 0.0 then
                // A brush pressed into a curve lays down more ink, so width follows how
                // tightly the spine is turning. Scaled against the stroke's own width —
                // a bend is "tight" relative to how fat the pen is — and passed through
                // tanh so a hairpin cannot run away with the width.
                w <- w * (1.0 + pressure * tanh (abs p.curvature * thickness))

            if inkSpread > 0.0 then
                // Ink wicking into paper: a mostly even bleed outward, with a fine
                // irregular edge from the fibres. Additive rather than multiplicative,
                // because bleed is a property of the paper, not of how hard you pressed —
                // it spreads a hairline as much as a broad stroke.
                let fibre = 0.75 + 0.25 * noise (p.s * 3.7) (p.side + 17.0)
                w <- w + inkSpread * thickness * 0.18 * fibre

            w

        let halfWidth (p: PenSample) =
            let w = baseHalfWidth p
            if roughness = 0.0 then w
            else max (0.15 * thickness) (w + roughness * thickness * 0.4 * noise p.s p.side)

        let twist =
            if mobius <= 0.0 then
                fun (_: PenSample) -> 0.0
            else
                let n = halfTwists mobius totalLen
                fun (p: PenSample) -> PI * float n * p.s / totalLen

        // Parallel traces.  Centres are spread evenly across `trace_spread`
        // thicknesses, so spread = 2 puts the outermost pair exactly where a solid
        // stroke's two edges would be.  Jitter is deterministic per trace index —
        // a font must render the same way every time — and shifts each trace
        // sideways and shortens it a little, which is what makes repeated
        // hand-drawn passes read as separate strokes rather than a printed rule.
        let traces =
            // Rounded because the UI slider hands us a float: a fractional count
            // would corrupt both the loop bound and the even spacing below.
            let n = max 1 (int (round (float axes.traces)))
            if n <= 1 then
                [ 0.0, 1.0, 0.0 ]
            else
                [ for i in 0 .. n - 1 ->
                    let frac = float i / float (n - 1) - 0.5
                    let centre = thickness * axes.trace_spread * frac
                    let jitter =
                        if axes.trace_jitter <= 0.0 then 0.0, 0.0
                        else
                            // Two decorrelated deterministic values in [-1, 1].
                            let r k = 2.0 * (let v = sin (float i * 127.1 + k) * 43758.5453 in v - floor v) - 1.0
                            r 311.7, r 74.7
                    let dCentre = fst jitter * axes.trace_jitter * thickness * 0.5
                    let dTrim = abs (snd jitter) * axes.trace_jitter * totalLen * 0.08
                    centre + dCentre, axes.trace_weight, dTrim ]

        { displace = displace
          baseHalfWidth = baseHalfWidth
          halfWidth = halfWidth
          twist = twist
          traces = traces }
