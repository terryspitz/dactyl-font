// Functional Fonts by terryspitz
// Mar 2020-

module Font

open System
open System.Security.Cryptography
open System.Text

open GeneratorTypes
open GlyphStringDefs
open GeneratorTypes
open SpiroPointType
open SpiroSegment
open SpiroControlPoint
open PathBezierContext
open Axes
open Curves
open DactylSpline
open SpiroCombContext
open SvgHelpers


// Attach extension method to segment class
type SpiroSegment with
    member this.Point =
        { y = this.Y
          x = this.X
          y_fit = false
          x_fit = false }

let PI = Math.PI

let hypot = SpiroImpl.hypot

///normalise angle to between PI/2 and -PI/2
let norm th =
    if th > PI then th - PI * 2.0
    else if th < (-PI) then th + PI * 2.0
    else th

let toPolar dx dy = hypot dx dy, atan2 dy dx

/// Strip tangents from a Curve element. Used when outline curves flow back through
/// the Spiro renderer, which would misinterpret tangent values as extra control points.
let clearElemTangents elem =
    match elem with
    | Curve(pts, isClosed) -> Curve(List.map (fun k -> { k with th_in = None; th_out = None }) pts, isClosed)
    | other -> other

let dotToClosedCurve x y r =
    closedCurve
        [ ({ y = y - r
             x = x
             y_fit = false
             x_fit = false },
           G2)
          ({ y = y
             x = x + r
             y_fit = false
             x_fit = false },
           G2)
          ({ y = y + r
             x = x
             y_fit = false
             x_fit = false },
           G2)
          ({ y = y
             x = x - r
             y_fit = false
             x_fit = false },
           G2) ]



//class
type Font(axes: Axes, ?showCombOpt: bool) =
    //basic manipulation using class variables

    let showComb = defaultArg showCombOpt false
    let _metrics = FontMetrics(axes)
    let thickness = _metrics.thickness

    ///Move point XY by dist at angle theta (from clockwise from X axis)
    /// Constrast axis makes verticals thicker (tweaks the X coord)
    let addPolarContrast X Y theta dist =
        { y = Y + dist * sin (theta)
          x = X + (dist + axes.contrast * thickness) * cos (theta)
          y_fit = false
          x_fit = false }

    let segmentAddPolar (seg: Segment) theta dist = addPolarContrast seg.X seg.Y theta dist

    /// Rotate dx,dy offsets by theta and add to XY (shared by cap logic)
    let offsetPointRotated X Y theta dx dy =
        addPolarContrast X Y (theta + atan2 dy dx) (hypot dx dy)

    let toSegment (seg: SpiroSegment) : Segment =
        { X = seg.X
          Y = seg.Y
          tangentStart = seg.tangent1
          tangentEnd = seg.tangent2
          seg_ch = seg.seg_ch
          Type = seg.Type }

    let rec elementToSpiros elem =
        let makeSCP (pt: Point, ty) = { SCP.X = pt.x; Y = pt.y; Type = ty }

        match elem with
        | Curve(pts, isClosed) ->
            validateKnotSequence pts isClosed
            // Revert to simpler logic that doesn't add handle points for tangents,
            // as this was breaking legacy Spiro path for some glyphs.
            let scps =
                [| for k in pts do
                       yield makeSCP (k.pt, k.ty) |]

            match Spiro.SpiroCPsToSegments scps isClosed with
            | Some segs ->
                // SpiroCPsToSegments returns n+1 elements for closed curves: the solver
                // appends a wrap-around copy of the first point whose ks values are not
                // solved (all zeroes). SpirosToBezier's comment says callers must strip
                // this final repeated point. Without stripping, the unsolved segment ends
                // up in the outline offset pass with a bogus tangent (tangentStart = 0),
                // offsetting it in the wrong direction and producing a distorted outline.
                let segments =
                    if isClosed then Array.toList segs.[0..scps.Length - 1]
                    else Array.toList segs
                if isClosed then
                    [ SpiroClosedCurve(segments) ]
                else
                    [ SpiroOpenCurve(segments) ]
            | None ->
                [ SpiroDot(
                      { y = _metrics.H
                        x = _metrics.C
                        y_fit = false
                        x_fit = false }
                  ) ]
        | Dot(p) -> [ SpiroDot(p) ]
        | EList(elems) -> List.collect elementToSpiros elems
        | Space -> [ SpiroSpace ]
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

    let toSpline2ControlPoints (pts: list<Knot>) =
        let pts =
            mergeConsecutive
                (fun k -> k.pt.x, k.pt.y)
                (fun k1 k2 ->
                    let t_in =
                        if Option.isNone k1.th_in || Option.isNone k2.th_in then
                            None
                        else
                            k1.th_in

                    let t_out =
                        if Option.isNone k1.th_out || Option.isNone k2.th_out then
                            None
                        else
                            k1.th_out

                    { k2 with th_in = t_in; th_out = t_out })
                pts

        [| for i in 0 .. pts.Length - 1 do
               let k = pts.[i]

               if k.ty = SpiroPointType.Anchor then
                   let kNext = pts.[i + 1]
                   let diff = kNext.pt - k.pt
                   let rth = atan2 diff.y diff.x

                   yield
                       Spline2ControlPoint(
                           { x = k.pt.x; y = k.pt.y },
                           (if axes.smooth then
                                SplinePointType.Smooth
                            else
                                SplinePointType.Corner),
                           None,
                           Some rth
                       )
               elif k.ty <> SpiroPointType.Handle then
                   let ty =
                       match k.ty with
                       | SpiroPointType.Corner
                       | SpiroPointType.OpenContour
                       | SpiroPointType.EndOpenContour
                       | SpiroPointType.End ->
                           if axes.smooth then
                               SplinePointType.Smooth
                           else
                               SplinePointType.Corner
                       | SpiroPointType.Left ->
                           if axes.smooth then
                               SplinePointType.Smooth
                           else
                               SplinePointType.CurveToLine
                       | SpiroPointType.Right ->
                           if axes.smooth then
                               SplinePointType.Smooth
                           else
                               SplinePointType.LineToCurve
                       | SpiroPointType.G2
                       | SpiroPointType.G4 -> SplinePointType.Smooth
                       | _ -> invalidArg "ty" (sprintf "Unexpected SpiroPointType %A" k.ty)
                   // yield SplineControlPoint({x=float x;y=float y}, ty)
                   yield
                       Spline2ControlPoint(
                           { x = k.pt.x; y = k.pt.y },
                           ty,
                           (if ty = SplinePointType.Smooth then k.th_in else None),
                           k.th_out
                       ) |]

    let rec elementToSpline2 elem =
        match elem with
        | Curve(pts, isClosed) ->
            validateKnotSequence pts isClosed
            let ctrlPts = toSpline2ControlPoints pts
            let spline = Spline2(ctrlPts, isClosed)
            spline.solve (axes.max_spline_iter)
            let types = pts |> List.map (fun k -> k.ty)

            let segs =
                [ for i in 0 .. ctrlPts.Length - 1 do
                      let pt = spline.ctrlPts.[i]

                      let pt1 =
                          if not isClosed && i = ctrlPts.Length - 1 then
                              pt // endpoint stub: last point of open curve (matches Spiro's EndOpenContour)
                          else
                              spline.ctrlPts.[(i + 1) % spline.ctrlPts.Length]

                      { SpiroSegment.X = pt.pt.x
                        Y = pt.pt.y
                        Type = types.[i]
                        bend_th = nan
                        ks = Array.empty
                        seg_ch = hypot (pt1.pt.x - pt.pt.x) (pt1.pt.y - pt.pt.y)
                        seg_th = nan
                        tangent1 = pt.rTh
                        tangent2 = pt1.lTh } ]

            if isClosed then
                [ SpiroClosedCurve(segs) ]
            else
                [ SpiroOpenCurve(segs) ]
        | Dot(p) -> [ SpiroDot(p) ]
        | EList(elems) -> List.collect elementToSpline2 elems
        | Space -> [ SpiroSpace ]
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

    let elementToSpiroSegments elem =
        let spiroElems =
            if axes.spline2 then
                elementToSpline2 elem
            else
                elementToSpiros elem

        let debug = false

        if debug then
            for seg in spiroElems do
                printfn "segments"

                match seg with
                | SpiroOpenCurve(segs) ->
                    printfn "Open curve"

                    for seg in segs do
                        printfn "%f,%f %f %f %c" seg.X seg.Y seg.tangent1 seg.tangent2 (ToChar seg.Type)
                | SpiroClosedCurve(segs) ->
                    printfn "Closed curve"

                    for seg in segs do
                        printfn "%f,%f %f %f %c" seg.X seg.Y seg.tangent1 seg.tangent2 (ToChar seg.Type)
                | _ -> ()

        spiroElems

    let spline2ctrlPtsToSvg ctrlPts isClosed =
        let spline = Spline2(ctrlPts, isClosed)
        spline.solve (axes.max_spline_iter)
        ([ spline.renderSvg ], [], if axes.show_tangents then spline.renderExplicitTangents else [])

    let spline2ptsToSvg pts isClosed =
        spline2ctrlPtsToSvg (pts |> withNoTangents |> toSpline2ControlPoints) isClosed

    let combineSvgResults results =
        List.fold
            (fun (accSvg, accComb, accTan) (svg, comb, tan) -> (accSvg @ svg, accComb @ comb, accTan @ tan))
            ([], [], [])
            results

    let rec elementToSpline2Svg elem =
        match elem with
        | Curve(pts, isClosed) -> spline2ctrlPtsToSvg (toSpline2ControlPoints pts) isClosed
        | Dot(p) -> (svgCircle p.x p.y thickness, [], [])
        | EList(elems) -> combineSvgResults (List.map elementToSpline2Svg elems)
        | Space -> ([], [], [])
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

    let toDactylSplineControlPoints (pts: list<Knot>) =
        [| for i in 0 .. pts.Length - 1 do
               let k = pts.[i]
               assert (k.ty <> SpiroPointType.Anchor && k.ty <> SpiroPointType.Handle)

               let ty =
                   match k.ty with
                   | SpiroPointType.Corner
                   | SpiroPointType.OpenContour
                   | SpiroPointType.EndOpenContour
                   | SpiroPointType.End ->
                       if axes.smooth then
                           SplinePointType.Smooth
                       else
                           SplinePointType.Corner
                   | SpiroPointType.Left ->
                       if axes.smooth then
                           SplinePointType.Smooth
                       else
                           SplinePointType.CurveToLine
                   | SpiroPointType.Right ->
                       if axes.smooth then
                           SplinePointType.Smooth
                       else
                           SplinePointType.LineToCurve
                   | SpiroPointType.G2
                   | SpiroPointType.G4 -> SplinePointType.Smooth
                   | _ -> invalidArg "ty" (sprintf "Unexpected SpiroPointType %A" k.ty)

               let x_opt = if k.pt.x_fit then None else Some k.pt.x
               let y_opt = if k.pt.y_fit then None else Some k.pt.y

               yield
                   { ty = ty
                     x = x_opt
                     y = y_opt
                     th_in = k.th_in
                     th_out = k.th_out } |]

    let rec elementToDactylSvg (elem: Element) =
        if axes.debug then
            printfn "elementToDactylSvg %A" elem

        let ctrlPtsToSvg ctrlPts isClosed =
            let spline = DactylSpline(ctrlPts, isClosed)

            spline.solveAndRenderSvg (
                axes.max_spline_iter,
                axes.flatness,
                axes.end_flatness,
                debug = axes.debug,
                showComb = showComb,
                showTangents = axes.show_tangents
            )

        match elem with
        | Curve(pts, isClosed) -> ctrlPtsToSvg (toDactylSplineControlPoints pts) isClosed
        | Dot(p) -> (svgCircle p.x p.y thickness, [], [])
        | EList(elems) -> combineSvgResults (List.map elementToDactylSvg elems)
        | Space -> ([], [], [])
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

    //apply a transformation fn: Spiros -> Elements
    let applyToSegments fn elem =
        let elems = elementToSpiroSegments elem |> List.collect fn
        if elems.Length > 1 then EList(elems) else elems.[0]

    //cache fn results to avoid recalcs
    //inside Font class, so it gets rebuilt for each new instance with different axes
    let memoize fn =
        let cache = new System.Collections.Generic.Dictionary<_, _>()

        (fun x ->
            match cache.TryGetValue x with
            | true, v -> v
            | false, _ ->
                let v = fn x
                cache.Add(x, v)
                v)


    ///align an angle to horizontal or vertical axis
    let align angle =
        let angle = norm angle

        if abs angle < PI / 4.0 then 0.0
        elif abs angle > PI * 3.0 / 4.0 then PI
        elif angle > 0.0 then PI / 2.
        else -PI / 2.

    //MEMBERS

    member this.metrics = _metrics

    member this.axes = axes

    // Cached Font with smooth=false for rendering outline knots (all sampled Corner points).
    // Avoids O(n²) NelderMead when the user has smooth=true: we solve the spine with smooth
    // and render the outline polyline with smooth=false so DactylSpline stays O(n).
    member val private outlineFontCachedOpt : Font option =
        (if axes.smooth then Some(Font({ axes with smooth = false })) else None)
        with get

    member this.outlineFont =
        match this.outlineFontCachedOpt with
        | Some f -> f
        | None -> this

    member this.reduce(e: Element) =
        match e with
        | Glyph(ch) ->
            if Map.containsKey ch GlyphStringDefs.glyphMap then
                memoize stringDefsToElem _metrics ch axes.debug
            else
                Dot(
                    { y = _metrics.H
                      x = _metrics.C
                      y_fit = false
                      x_fit = false }
                )
        | Curve(pts, isClosed) -> e
        | Dot(p) -> e
        | EList(elems) -> EList(List.map this.reduce elems)
        | Space -> e

    member this.elemWidth e =
        let maxX2 (pts: Knot list) =
            List.fold max 0.0 (List.map (fun k -> k.pt.x) pts)

        match e with
        | Curve(pts, _) -> maxX2 pts
        | Dot(p) -> p.x
        | EList(elems) -> List.fold max 0.0 (List.map this.elemWidth elems)
        | Space ->
            let space = axes.height / 4 //according to https://en.wikipedia.org/wiki/Whitespace_character#Variable-width_general-purpose_space

            (1.0 - axes.monospace) * float space + axes.monospace * _metrics.monospaceWidth
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" e)

    member this.monospace =
        let mono e =
            let w = this.elemWidth e

            if w = 0.0 then
                e
            else
                let monoFn (p: Point) =
                    let full_scale = _metrics.monospaceWidth / w
                    let x_scale = (1.0 - axes.monospace) + axes.monospace * full_scale
                    { p with x = p.x * x_scale }

                movePoints monoFn None e

        applyIf (axes.monospace > 0.0) mono

    member this.charHeight =
        float this.axes.height - _metrics.D + thickness * 2.0 + float this.axes.leading

    ///distance from bottom of descenders to baseline ()
    member this.yBaselineOffset = -_metrics.D + thickness

    member this.isJointRaw elem X Y =
        let checkXYColinearPoints (pts: list<Point * SpiroPointType>) =
            List.fold
                (||)
                false
                [ for i in 0 .. pts.Length - 2 do
                      let p1, p2 = fst pts.[i], fst pts.[i + 1]
                      let x1, y1, x2, y2 = p1.x, p1.y, p2.x, p2.y

                      if (x1 = X && y1 = Y) || (x2 = X && y2 = Y) then
                          false
                      elif
                          not (
                              (x1 <= X && X <= x2 && y1 <= Y && Y <= y2)
                              || (x2 <= X && X <= x1 && y2 <= Y && Y <= y1)
                          )
                      then
                          false
                      else
                          let perpX, perpY = (y2 - y1), -(x2 - x1)
                          assert ((x1 * perpX + y1 * perpY) - (x2 * perpX + y2 * perpY) < 1.0)
                          let perpDist = (X * perpX + Y * perpY) - (x1 * perpX + y1 * perpY)
                          (perpDist > -thickness) && (perpDist < thickness) ]
        //TODO: check joints on curves, or mark manually in reduce fn.
        let rec checkElem e =
            match e with
            | Curve(knots, _) ->
                let ptsTy = knots |> List.map (fun k -> k.pt, k.ty)
                checkXYColinearPoints ptsTy
            | Dot(p) -> false
            | EList(elems) -> List.fold (||) false (List.map checkElem elems)
            | Space -> false
            | _ -> invalidArg "e" (sprintf "Unreduced element %A" e)

        checkElem elem

    member this.isJoint elem X Y =
        if axes.joints then
            memoize this.isJointRaw elem X Y
        else
            false

    member this.maybeAlign angle =
        if this.axes.axis_align_caps then align angle else angle

    /// Shared cap geometry: given stroke endpoint (X,Y), outgoing angle theta, joint flag,
    /// point-type for the join, and pre-computed tangent pairs for the first and last junction points.
    /// Intermediate cap points get (None, None).
    member this.cap X Y theta isJoint ty firstThIn firstThOut lastThIn lastThOut shouldAlign =
        let fthickness = float thickness
        let serif = float this.axes.serif
        let thetaAligned = if shouldAlign then this.maybeAlign theta else theta
        let nnIn, nnOut = None, None

        if this.axes.serif <> 0 && not isJoint then
            //make serif on endcap
            let serifDist, serifAng = toPolar (serif + fthickness) fthickness

            [ { pt = addPolarContrast X Y (thetaAligned + PI * 0.75) (fthickness * sqrt 2.0)
                ty = Corner
                th_in = firstThIn
                th_out = firstThOut
                label = None }
              { pt = addPolarContrast X Y (thetaAligned + PI * 0.5 + serifAng) serifDist
                ty = Corner
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = addPolarContrast X Y (thetaAligned + PI * 0.5 - serifAng) serifDist
                ty = Corner
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = addPolarContrast X Y (thetaAligned - PI * 0.5 + serifAng) serifDist
                ty = Corner
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = addPolarContrast X Y (thetaAligned - PI * 0.5 - serifAng) serifDist
                ty = Corner
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = addPolarContrast X Y (thetaAligned - PI * 0.75) (fthickness * sqrt 2.0)
                ty = ty
                th_in = lastThIn
                th_out = lastThOut
                label = None } ]
        elif this.axes.flare <> 0.0 && not isJoint then
            //make flared endcap
            let preflareDist, preflareAng = toPolar -(fthickness * 0.80) fthickness
            let flareDist, flareAng = toPolar ((this.axes.flare + 1.0) * fthickness) fthickness

            [ { pt = addPolarContrast X Y (theta + PI * 0.75) (fthickness * sqrt 2.0)
                ty = Corner
                th_in = firstThIn
                th_out = firstThOut
                label = None }
              { pt = addPolarContrast X Y (theta + preflareAng) preflareDist
                ty = LineToCurve
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = addPolarContrast X Y (thetaAligned + PI * 0.5 - flareAng) flareDist
                ty = Corner
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = addPolarContrast X Y (thetaAligned - PI * 0.5 + flareAng) flareDist
                ty = Corner
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = addPolarContrast X Y (theta - preflareAng) preflareDist
                ty = CurveToLine
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = addPolarContrast X Y (theta - PI * 0.75) (fthickness * sqrt 2.0)
                ty = ty
                th_in = lastThIn
                th_out = lastThOut
                label = None } ]
        elif this.axes.end_bulb <> 0.0 && not isJoint then
            [ { pt = offsetPointRotated X Y thetaAligned (fthickness * (1. - this.axes.end_bulb)) fthickness
                ty = Corner
                th_in = firstThIn
                th_out = firstThOut
                label = None }
              { pt = offsetPointRotated X Y thetaAligned fthickness 0.
                ty = G2
                th_in = nnIn
                th_out = nnOut
                label = None }
              { pt = offsetPointRotated X Y thetaAligned (fthickness * (1. - this.axes.end_bulb)) -fthickness
                ty = ty
                th_in = lastThIn
                th_out = lastThOut
                label = None } ]
        elif isJoint then // try forcing alignment
            [ { pt = offsetPointRotated X Y (align theta) fthickness fthickness
                ty = Corner
                th_in = firstThIn
                th_out = firstThOut
                label = Some "joint" }
              { pt = offsetPointRotated X Y (align theta) fthickness -fthickness
                ty = ty
                th_in = lastThIn
                th_out = lastThOut
                label = Some "joint" } ]
        else
            [ { pt = offsetPointRotated X Y thetaAligned fthickness fthickness
                ty = Corner
                th_in = firstThIn
                th_out = firstThOut
                label = None }
              { pt = offsetPointRotated X Y thetaAligned fthickness -fthickness
                ty = ty
                th_in = lastThIn
                th_out = lastThOut
                label = None } ]

    /// Start-cap knots: points at the beginning of an open stroke.
    /// Outline path: ... revOffset → startCap[first] → ... → startCap[last] → fwdOffset ...
    /// First point faces reversed offset (tangent flipped), last faces forward offset.
    member this.startCap (seg: Segment) elem shouldAlign ty =
        let bt = seg.tangentStart

        this.cap
            seg.X
            seg.Y
            (seg.tangentStart + PI)
            (this.isJoint elem seg.X seg.Y)
            ty
            (Some(norm (bt + PI)))
            None
            None
            (Some bt)
            shouldAlign

    /// End-cap knots: points at the beginning of an open stroke.
    /// Outline path: ... fwdOffset → endCap[first] → ... → endCap[last] → revOffset ...
    /// First point faces forward offset, last faces reversed offset (tangent flipped).
    member this.endCap (seg: Segment) (lastSeg: Segment) elem shouldAlign ty =
        let bt = lastSeg.tangentEnd

        this.cap
            seg.X
            seg.Y
            lastSeg.tangentEnd
            (this.isJoint elem seg.X seg.Y)
            ty
            (Some bt)
            None
            None
            (Some(norm (bt + PI)))
            shouldAlign

    member this.reverseSegments(segments: SpiroSegment list) =
        [ for seg in List.rev segments do
              let newType =
                  match seg.Type with
                  | SpiroPointType.OpenContour -> SpiroPointType.EndOpenContour
                  | SpiroPointType.EndOpenContour -> SpiroPointType.OpenContour
                  | SpiroPointType.Left -> SpiroPointType.Right
                  | SpiroPointType.Right -> SpiroPointType.Left
                  | _ -> seg.Type

              { seg with Type = newType } ]

    member this.offsetSegment (seg: Segment) (lastSeg: Segment) reverse dist =
        let newType =
            match seg.Type, reverse with
            | SpiroPointType.Left, true -> SpiroPointType.Right
            | SpiroPointType.Right, true -> SpiroPointType.Left
            | SpiroPointType.EndOpenContour, false -> Corner
            | _ -> seg.Type

        let angle = if reverse then -PI / 2. else PI / 2.

        match seg.Type with
        | SpiroPointType.Corner ->
            let th1, th2 = norm (lastSeg.tangentEnd + angle), norm (seg.tangentStart + angle)
            let bend = norm (th2 - th1)
            let alpha = bend / 2.0

            let isSharperThanRight = abs bend >= 2.0 // Approx 115 degrees. 90 deg is 1.57.
            let isOuterBend = (not reverse && bend < -PI / 8.0) || (reverse && bend > PI / 8.0)

            if isOuterBend && isSharperThanRight then
                // REVERT: Use two points at the same location (the miter point) for sharp corners
                [ { pt = segmentAddPolar seg (this.maybeAlign th1 - angle / 2.0) (dist * sqrt 2.0)
                    ty = newType
                    th_in = Some lastSeg.tangentEnd
                    th_out = None
                    label = None }
                  { pt = segmentAddPolar seg (this.maybeAlign th2 + angle / 2.0) (dist * sqrt 2.0)
                    ty = newType
                    th_in = None
                    th_out = Some seg.tangentStart
                    label = None } ]
            else
                // 90 degree corners (and shallower/inner) are sharp miters (single knot)
                let offset = min (min (dist / cos alpha) seg.seg_ch) lastSeg.seg_ch
                let sharpPt = addPolarContrast seg.X seg.Y (th1 + alpha) offset

                [ { pt = sharpPt
                    ty = newType
                    th_in = Some lastSeg.tangentEnd
                    th_out = Some seg.tangentStart
                    label = None } ]
        | SpiroPointType.Right ->
            let t = Some seg.tangentStart

            [ { pt = segmentAddPolar seg (norm (lastSeg.tangentEnd + angle)) dist
                ty = newType
                th_in = t
                th_out = t
                label = None } ]
        | SpiroPointType.Left ->
            let t = Some seg.tangentStart

            [ { pt = segmentAddPolar seg (norm (seg.tangentStart + angle)) dist
                ty = newType
                th_in = t
                th_out = t
                label = None } ]
        | _ ->
            let t = Some seg.tangentStart

            [ { pt = segmentAddPolar seg (seg.tangentStart + angle) dist
                ty = newType
                th_in = t
                th_out = t
                label = None } ]

    member this.offsetSegments (segments: list<Segment>) start endP reverse closed dist =
        [ for i in start..endP do
              let seg = segments.[i]
              let angle = if reverse then -PI / 2. else PI / 2.

              if i = 0 then
                  if closed then
                      let lastSeg = segments.[segments.Length - 1]
                      yield! this.offsetSegment seg lastSeg reverse dist
                  else
                      yield
                          { pt = segmentAddPolar seg (seg.tangentStart + angle) dist
                            ty = seg.Type
                            th_in = Some seg.tangentStart
                            th_out = Some seg.tangentStart
                            label = None }
              elif i = segments.Length - 1 && not closed then
                  let lastSeg = segments.[i - 1]

                  yield
                      { pt = segmentAddPolar seg (lastSeg.tangentEnd + angle) dist
                        ty = seg.Type
                        th_in = Some lastSeg.tangentEnd
                        th_out = Some lastSeg.tangentEnd
                        label = None }
              else
                  let lastSeg = segments.[i - 1]
                  yield! this.offsetSegment seg lastSeg reverse dist ]

    /// Flip tangent by PI for reversed-path points (path direction reversal).
    /// Also swaps th_in and th_out since path direction reverses.
    /// Note: type swapping (LineToCurve↔CurveToLine) is already handled by offsetSegment(reverse=true),
    /// so flipTangent must NOT change ty.
    static member flipTangent(k: Knot) =
        { k with
            th_in = k.th_out |> Option.map (fun t -> norm (t + PI))
            th_out = k.th_in |> Option.map (fun t -> norm (t + PI)) }

    /// Replace sharp Corner knots with small arcs to produce rounded corners.
    /// The radius is proportional to soft_corners * thickness, clamped to 40% of each
    /// adjacent segment so short segments (end caps) are never over-consumed.
    /// Corners labelled "joint" (where two strokes meet) are left untouched.
    /// The arc point types are adapted to the neighbour context: LineToCurve/CurveToLine
    /// when the adjacent segment is a straight line, G2 when it is a smooth curve.
    member this.roundCorners (pts: Knot list) (isClosed: bool) : Knot list =
        let radius = axes.soft_corners * thickness

        if radius < 1.0 || pts.Length < 3 then
            pts
        else
            let n = pts.Length
            let arr = Array.ofList pts
            let result = System.Collections.Generic.List<Knot>()

            for i in 0 .. n - 1 do
                let k = arr.[i]

                if k.ty <> Corner || k.label = Some "joint" then
                    result.Add(k)
                else
                    // Get prev/next indices, wrapping for closed curves
                    let hasPrev = i > 0 || isClosed
                    let hasNext = i < n - 1 || isClosed

                    if not hasPrev || not hasNext then
                        result.Add(k)
                    else
                        let prevIdx = if i > 0 then i - 1 else n - 1
                        let nextIdx = if i < n - 1 then i + 1 else 0
                        let prev = arr.[prevIdx]
                        let next = arr.[nextIdx]
                        let dxPrev = prev.pt.x - k.pt.x
                        let dyPrev = prev.pt.y - k.pt.y
                        let dxNext = next.pt.x - k.pt.x
                        let dyNext = next.pt.y - k.pt.y
                        let distPrev = hypot dxPrev dyPrev
                        let distNext = hypot dxNext dyNext
                        // Clamp radius so we don't consume more than 40% of either adjacent segment
                        let r = min radius (min (distPrev * 0.4) (distNext * 0.4))

                        if r < 1.0 then
                            result.Add(k)
                        else
                            // Pull-back points along incoming and outgoing directions
                            let inPt = { y = k.pt.y + r * dyPrev / distPrev
                                         x = k.pt.x + r * dxPrev / distPrev
                                         y_fit = false; x_fit = false }
                            let outPt = { y = k.pt.y + r * dyNext / distNext
                                          x = k.pt.x + r * dxNext / distNext
                                          y_fit = false; x_fit = false }
                            // Choose point types based on whether the adjacent segment is a line or curve.
                            // The checks are intentionally asymmetric — LineToCurve/CurveToLine encode
                            // WHICH SIDE of the point the straight section is on:
                            //   CurveToLine at prev → segment prev→corner is a line → inPt gets LineToCurve
                            //     (preprocessLineTangents then sets th_out = line direction, anchoring the arc start)
                            //   LineToCurve at next → segment corner→next is a line → outPt gets CurveToLine
                            //     (preprocessLineTangents sets th_out = line direction, anchoring the arc end)
                            //   Corner at either side → Corner-Corner pairs with colinear tangents are lines too
                            // When the neighbour is a smooth curve (G2/G4) we use G2 so the spline solver
                            // preserves smooth continuity rather than converting the curved approach into a line.
                            let inPtTy  = if prev.ty = CurveToLine || prev.ty = Corner then LineToCurve else G2
                            let outPtTy = if next.ty = LineToCurve  || next.ty = Corner then CurveToLine else G2
                            result.Add({ k with pt = inPt; ty = inPtTy; th_out = k.th_in })
                            result.Add({ k with pt = outPt; ty = outPtTy; th_in = k.th_out })
            List.ofSeq result

    /// Apply roundCorners to an Element (Curve or EList of Curves).
    member this.applySoftCorners elem =
        if axes.soft_corners <= 0.0 then
            elem
        else
            let rec apply e =
                match e with
                | Curve(pts, isClosed) -> Curve(this.roundCorners pts isClosed, isClosed)
                | EList(elems) -> EList(List.map apply elems)
                | other -> other

            apply elem

    member this.strokeSegments
        (segs: Segment list)
        (fthickness: float)
        (startCapFn: Segment -> Knot list)
        (endCapFn: Segment -> Segment -> Knot list)
        (closed: bool)
        =
        if not closed then
            let offsetMidSegments (segs: Segment list) reverse =
                this.offsetSegments segs 1 (segs.Length - 2) reverse false fthickness

            let points =
                startCapFn segs.[0]
                @ offsetMidSegments segs false
                @ endCapFn segs.[segs.Length - 1] segs.[segs.Length - 2]
                @ (offsetMidSegments segs true |> List.rev |> List.map Font.flipTangent)

            [ Curve(points, true) ]
        else
            [ Curve(this.offsetSegments segs 0 (segs.Length - 1) false true fthickness, true)
              Curve(
                  this.offsetSegments segs 0 (segs.Length - 1) true true fthickness
                  |> List.rev
                  |> List.map Font.flipTangent,
                  true
              ) ]

    member this.getSpiroSansOutlines e =
        let fthickness = float thickness
        let capType (seg: Segment) = if seg.Type = SpiroPointType.Anchor then SpiroPointType.Anchor else Corner
        let isFreeCurveEnd ty = ty = G2 || ty = G4

        let rec spiroToOutlines elem =
            match elem with
            | Curve(pts, isClosed) ->
                let startAlign = pts.IsEmpty || isClosed || not (isFreeCurveEnd pts.[0].ty)
                let endAlign = pts.IsEmpty || isClosed || not (isFreeCurveEnd (List.last pts).ty)
                let startCap (seg: Segment) = this.startCap seg e startAlign (capType seg)
                let endCap (seg: Segment) (lastSeg: Segment) = this.endCap seg lastSeg e endAlign (capType seg)
                elementToSpiroSegments (Curve(pts, isClosed))
                |> List.collect (fun spiro ->
                    match spiro with
                    | SpiroOpenCurve(segments) ->
                        let segs = List.map toSegment segments
                        this.strokeSegments segs fthickness startCap endCap false
                        |> List.map clearElemTangents
                        |> List.map this.applySoftCorners
                    | SpiroClosedCurve(segments) ->
                        let segs = List.map toSegment segments
                        this.strokeSegments segs fthickness startCap endCap true
                        |> List.map clearElemTangents
                        |> List.map this.applySoftCorners
                    | SpiroDot(p) -> [ dotToClosedCurve p.x p.y (thickness + 5.0) ]
                    | SpiroSpace -> [ Space ]
                )
            | Dot(p) -> [ dotToClosedCurve p.x p.y (thickness + 5.0) ]
            | EList(elems) -> List.collect spiroToOutlines elems
            | Space -> [ Space ]
            | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

        let results = spiroToOutlines e
        if results.Length > 1 then EList(results) else results.[0]

    member this.spiroToLines lines spiro =
        let thicknessby3 = float thickness / 3.0

        match spiro with
        | SpiroOpenCurve(segments) ->
            let segs = List.map toSegment segments

            [ for i in 0 .. lines - 1 do
                  let offset =
                      if lines <= 1 then
                          0.0
                      else
                          (float thickness) * (float i / float (lines - 1) - 0.5) * 2.0

                  clearElemTangents (Curve(this.offsetSegments segs 0 (segs.Length - 1) false false offset, false)) ]
        | SpiroClosedCurve(segments) ->
            let segs = List.map toSegment segments

            [ for i in 0 .. lines - 1 do
                  let offset =
                      if lines <= 1 then
                          0.0
                      else
                          (float thickness) * (float i / float (lines - 1) - 0.5) * 2.0

                  clearElemTangents (Curve(this.offsetSegments segs 0 (segs.Length - 1) false true offset, true)) ]
        | SpiroDot(p) ->
            [ dotToClosedCurve p.x p.y thickness
              dotToClosedCurve p.x p.y (thickness / 2.0) ]
        | SpiroSpace -> [ Space ]

    member this.getStroked =
        applyToSegments (this.spiroToLines 4)
        >> Font({ Axes.DefaultAxes with thickness = 2 }).getSpiroSansOutlines

    member this.getScratches e =
        let spiroToScratchOutlines spiro =
            let thicknessby3 = float thickness / 3.0

            let offsetPointCap X Y theta =
                addPolarContrast X Y theta (thicknessby3 * sqrt 2.0)

            let startCap (seg: Segment) =
                let t = Some seg.tangentStart

                [ { pt = segmentAddPolar seg (seg.tangentStart - PI * 0.90) (thicknessby3 * 3.0)
                    ty = Corner
                    th_in = t
                    th_out = t
                    label = None }
                  { pt = offsetPointCap seg.X seg.Y (seg.tangentStart + PI * 0.75)
                    ty = Corner
                    th_in = t
                    th_out = t
                    label = None } ]

            let endCap (seg: Segment) (lastSeg: Segment) =
                let t = Some lastSeg.tangentEnd

                [ { pt = offsetPointCap seg.X seg.Y lastSeg.tangentEnd
                    ty = Corner
                    th_in = t
                    th_out = t
                    label = None } ]

            match spiro with
            | SpiroOpenCurve(segments) ->
                let segs = List.map toSegment segments

                this.strokeSegments segs thicknessby3 startCap endCap false
                |> List.map clearElemTangents
            | SpiroClosedCurve(segments) ->
                let segs = List.map toSegment segments

                this.strokeSegments segs (thicknessby3 / 3.) startCap endCap true
                |> List.map clearElemTangents
            | SpiroDot(p) -> [ Dot(p) ]
            | SpiroSpace -> [ Space ]

        applyToSegments (this.spiroToLines 3) e
        |> applyToSegments spiroToScratchOutlines

    member this.italicisePt(p: Point) =
        { p with
            x = p.x + this.axes.italic * p.y }

    member this.italiciseTan th =
        let italic = this.axes.italic

        if italic = 0.0 then
            th
        else
            let dx = cos th + italic * sin th
            let dy = sin th
            atan2 dy dx

    /// Italicising the outlines leads to strange curves.  Attempt to subdivide the original curves so the
    /// italicised versions are closer to a shear. Note: there is a performance cost for more detailed curves.
    member this.subdivide e =
        let splitOneSegment (seg1: SpiroSegment) (seg2: SpiroSegment) =
            if
                (seg1.Type = Corner || seg1.Type = CurveToLine)
                && (seg2.Type = Corner || seg2.Type = LineToCurve)
            then
                [ ({ y = seg1.Y
                     x = seg1.X
                     y_fit = false
                     x_fit = false },
                   seg1.Type) ]
            elif seg1.Type = Handle then
                []
            else
                let scale, rot = SpiroImpl.get_scale_rot (seg2.X - seg1.X) (seg2.Y - seg1.Y) seg1.ks
                let ksub = Array.create 4 0.0

                let xmid, ymid =
                    SpiroImpl.get_mid seg1.X seg1.Y seg2.X seg2.Y scale rot seg1.ks ksub

                let midType =
                    match seg1.Type, seg2.Type with
                    | (SpiroPointType.OpenContour, t) -> t
                    | (SpiroPointType.EndOpenContour, _) -> G2
                    | (_, SpiroPointType.G2)
                    | (_, SpiroPointType.Left) -> G2
                    | (SpiroPointType.G2, _)
                    | (SpiroPointType.Right, _) -> G2
                    | (SpiroPointType.Anchor, _) -> G2
                    | (_, _) -> seg1.Type

                [ ({ y = seg1.Y
                     x = seg1.X
                     y_fit = false
                     x_fit = false },
                   seg1.Type)
                  ({ y = ymid
                     x = xmid
                     y_fit = false
                     x_fit = false },
                   midType) ]

        let splitSegments spiros =
            match spiros with
            | SpiroOpenCurve(segments) ->
                Curve(
                    withNoTangents (
                        [ for i in 0 .. segments.Length - 2 do
                              yield! splitOneSegment segments.[i] segments.[i + 1] ]
                        @ [ let seg = segments.[segments.Length - 1] in

                            { y = seg.Y
                              x = seg.X
                              y_fit = false
                              x_fit = false },
                            seg.Type ]
                    ),
                    false
                )
            | SpiroClosedCurve(segments) ->
                Curve(
                    withNoTangents
                        [ for i in 0 .. segments.Length - 1 do
                              yield! splitOneSegment segments.[i] segments.[(i + 1) % segments.Length] ],
                    true
                )
            | SpiroDot(p) -> Dot(p)
            | SpiroSpace -> Space

        EList(elementToSpiroSegments e |> List.map splitSegments)

    /// Post-solve italic applies to the DactylSpline-based outline paths (DactylSans and
    /// constant-offset), but only when generating outlines.  In that mode the upright spine is
    /// solved first and the shear is applied to the dense solved points (see shearBezPts), so the
    /// knot pre-shear below is skipped.  Spiro and Spline2, and all non-outline (backbone) renders,
    /// keep the original pre-solve knot shear.
    member this.usePostSolveItalic =
        this.axes.outline && (this.axes.dactyl_spline || this.axes.constant_offset)

    member this.italicise =
        applyIf
            (this.axes.italic <> 0.0 && not this.usePostSolveItalic)
            ((applyIf (not (this.axes.spline2 || this.axes.dactyl_spline)) this.subdivide)
             >> (movePoints this.italicisePt (Some this.italiciseTan)))

    /// Post-solve italic shear of an already-solved DactylSpline spine.  Shearing the dense solved
    /// bezier points (rather than the sparse pre-solve knots) is an exact affine transform, so it
    /// avoids the non-affine curve distortion that the knot pre-shear works around via subdivide.
    /// The perpendicular stroke offset is then taken in slanted space, giving even stroke weight.
    /// Mutates and returns the array.  No-op when italic = 0.
    member this.shearBezPts(bezPts: BezierPoint array) =
        let italic = this.axes.italic

        if italic <> 0.0 then
            // Scale of a unit direction vector at angle th under the shear [[1, italic],[0,1]].
            let shearScale th = hypot (cos th + italic * sin th) (sin th)

            for bp in bezPts do
                // Scale control-point distances using the original tangents so the reconstructed
                // cubics (ld/rd + th) stay an exact shear of the upright cubics.
                bp.ld <- bp.ld * shearScale bp.th_in
                bp.rd <- bp.rd * shearScale bp.th_out
                bp.x <- bp.x + italic * bp.y
                bp.th_in <- this.italiciseTan bp.th_in
                bp.th_out <- this.italiciseTan bp.th_out

        bezPts

    member this.getDactylSansOutlines e =
        let fthickness = float thickness
        let isFreeCurveEnd ty = ty = G2 || ty = G4

        // Post-solve italic shears the solved spine, so joint detection (which reads knot
        // positions from the element) must compare against a shear of the same element.
        let eForJoints =
            applyIf (this.axes.italic <> 0.0) (movePoints this.italicisePt None) e

        let splineTypeToSpiroType ty =
            match ty with
            | SplinePointType.Corner -> SpiroPointType.Corner
            | SplinePointType.CurveToLine -> CurveToLine
            | SplinePointType.LineToCurve -> LineToCurve
            | _ -> SpiroPointType.G2

        let solveCurveSegs (pts: Knot list) isClosed =
            if axes.debug then
                printfn "solveCurveSegs pts: %A" pts

            let ctrlPts = toDactylSplineControlPoints pts

            if axes.debug then
                printfn "solveCurveSegs ctrlPts: %A" ctrlPts

            let spline = DactylSpline(ctrlPts, isClosed)

            let bezPts =
                spline.solveAndGetPoints (axes.max_spline_iter, axes.flatness, axes.end_flatness, axes.debug)
                |> this.shearBezPts

            let n = bezPts.Length

            [ for i in 0 .. n - 1 do
                  let nextI = (i + 1) % n

                  let seg_ch =
                      if i = n - 1 && not isClosed then
                          1.0
                      else
                          let dx = bezPts.[nextI].x - bezPts.[i].x
                          let dy = bezPts.[nextI].y - bezPts.[i].y
                          Math.Sqrt(dx * dx + dy * dy)

                  { X = bezPts.[i].x
                    Y = bezPts.[i].y
                    tangentStart = bezPts.[i].th_out
                    tangentEnd = bezPts.[nextI].th_in
                    seg_ch = seg_ch
                    Type = splineTypeToSpiroType ctrlPts.[i].ty } ]

        let rec dactylToOutline elem =
            match elem with
            | Curve(pts, isClosed) ->
                validateKnotSequence pts isClosed
                let startAlign = pts.IsEmpty || isClosed || not (isFreeCurveEnd pts.[0].ty)
                let endAlign = pts.IsEmpty || isClosed || not (isFreeCurveEnd (List.last pts).ty)
                let startCap (seg: Segment) = this.startCap seg eForJoints startAlign Corner
                let endCap (seg: Segment) (lastSeg: Segment) = this.endCap seg lastSeg eForJoints endAlign Corner
                let segs = solveCurveSegs pts isClosed

                if axes.debug then
                    printfn "dactylToOutline curve: %A" segs

                this.strokeSegments segs fthickness startCap endCap isClosed
                |> List.map this.applySoftCorners
            | Dot(p) ->
                let p = this.italicisePt p
                [ dotToClosedCurve p.x p.y (thickness + 5.0) ]
            | EList(elems) -> List.collect dactylToOutline elems
            | Space -> [ Space ]
            | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

        let results = dactylToOutline (this.reduce e)
        if results.Length = 1 then results.[0] else EList(results)

    /// Outline generation using dense sampling at a constant perpendicular distance
    /// from the DactylSpline-solved spine. Walks each cubic bezier at 8 t-steps,
    /// offsets every sample by ±thickness perpendicular to its local tangent, and
    /// emits the result as Corner knots (straight-line segments between samples).
    /// Reuses the same cap/joint/serif/flare/soft_corners logic as getDactylSansOutlines.
    member this.getDactylConstantOffsetOutlines e =
        let fthickness = float thickness
        let nib = axes.nib
        let nibAngle = float axes.nib_angle * PI / 180.0
        let taper = axes.taper
        let taperEnd = axes.taper_end
        let wobble = axes.wobble
        let wobbleWavelength = 200.0
        let mobius = axes.mobius
        let mobiusHalfTwistLen = 300.0
        // Axes whose width or displacement varies with arc length need interior samples
        // even on straight spine segments.
        let widthVariesAlongStroke = nib > 0.0 || taper > 0.0 || wobble > 0.0 || mobius > 0.0
        let samplesPerSeg = 16
        let isFreeCurveEnd ty = ty = G2 || ty = G4

        // Post-solve italic shears the solved spine, so joint detection (which reads knot
        // positions from the element) must compare against a shear of the same element.
        let eForJoints =
            applyIf (this.axes.italic <> 0.0) (movePoints this.italicisePt None) e

        // Build Segment for use with existing cap functions.
        // Only X, Y, tangentStart, and tangentEnd are read by startCap/endCap.
        let makeCapSeg (bp: BezierPoint) tStart tEnd =
            { X = bp.x; Y = bp.y
              tangentStart = tStart
              tangentEnd = tEnd
              seg_ch = 1.0
              Type = Corner }

        let buildOutlineFromBez (bezPts: BezierPoint array) (isClosed: bool) (startAlign: bool) (endAlign: bool) =
            if bezPts.Length < 2 then [] else

            let n = bezPts.Length
            let segCount = if isClosed then n else n - 1
            let plainKnot pt =
                { pt = pt; ty = Corner; th_in = None; th_out = None; label = None }

            // --- Spine sampling with cumulative arc length ---

            /// Position and tangent of the spine cubic for segment segIdx at parameter t.
            let bezEval (segIdx: int) (t: float) =
                let p1 = bezPts.[segIdx]
                let p2 = bezPts.[(segIdx + 1) % n]
                let cp1, cp2 = p1.rpt(), p2.lpt()
                let x, y, dx, dy = getBezPtAndTangent (p1.x, p1.y) (cp1.x, cp1.y) (cp2.x, cp2.y) (p2.x, p2.y) t
                let th =
                    if dx*dx + dy*dy < 1e-12 then atan2 (p2.y - p1.y) (p2.x - p1.x)
                    else atan2 dy dx
                x, y, th

            // A straight-line spine segment (both tangents colinear with the chord) has
            // a parallel-line perpendicular offset, so interior samples would be
            // collinear with the endpoint offsets and add no information. Skip them
            // (the chord is also the exact arc length in that case).
            let isStraightSeg (segIdx: int) =
                let p1 = bezPts.[segIdx]
                let p2 = bezPts.[(segIdx + 1) % n]
                let dxc = p2.x - p1.x
                let dyc = p2.y - p1.y
                if dxc * dxc + dyc * dyc < 1e-12 then true
                else
                    let chordAngle = atan2 dyc dxc
                    abs (norm (p1.th_out - chordAngle)) < 1e-4
                    && abs (norm (p2.th_in - chordAngle)) < 1e-4

            // Interior samples (x, y, th, arc length within segment) for each spine
            // segment, plus the chord-sum arc length of the whole segment.
            let segData =
                [| for segIdx in 0 .. segCount - 1 ->
                       let p1 = bezPts.[segIdx]
                       let p2 = bezPts.[(segIdx + 1) % n]
                       let interior =
                           if isStraightSeg segIdx && not widthVariesAlongStroke then [||]
                           else [| for s in 1 .. samplesPerSeg - 1 -> bezEval segIdx (float s / float samplesPerSeg) |]
                       let mutable len = 0.0
                       let mutable px, py = p1.x, p1.y
                       let withLen =
                           [| for (x, y, th) in interior do
                                  len <- len + hypot (x - px) (y - py)
                                  px <- x
                                  py <- y
                                  yield (x, y, th, len) |]
                       withLen, len + hypot (p2.x - px) (p2.y - py) |]

            /// Cumulative arc length at each bez point (entry segCount = whole-curve length).
            let cumLenAtBez =
                let arr = Array.zeroCreate (segCount + 1)
                for i in 0 .. segCount - 1 do
                    arr.[i + 1] <- arr.[i] + snd segData.[i]
                arr

            let totalLen = max cumLenAtBez.[segCount] 1e-9

            let wobbleCycles = max 1.0 (Math.Round(totalLen / wobbleWavelength))

            /// Hand-drawn waviness: displace a spine sample perpendicular to its tangent.
            /// An integer cycle count makes the displacement vanish at open-stroke
            /// endpoints (so caps fit) and stay continuous around closed curves. The
            /// tangent is corrected by the displacement slope so offsets remain
            /// perpendicular to the displaced spine.
            let displace (x: float, y: float, th: float, sLen: float) =
                if wobble = 0.0 then
                    (x, y, th, sLen)
                else
                    // wobble=1.0 displaces the spine by half a stroke-thickness at the wave peaks.
                    let amp = wobble * fthickness * 0.5
                    let phase = 2.0 * PI * wobbleCycles * sLen / totalLen
                    let d = amp * sin phase
                    let slope = amp * 2.0 * PI * wobbleCycles / totalLen * cos phase
                    // Offset perpendicular to the tangent: rotate (d, 0) by th + 90° = (-d sin th, d cos th).
                    (x - d * sin th, y + d * cos th, th + atan slope, sLen)

            /// Spine samples (x, y, th, arc length from curve start), shared by both sides.
            let spineSamples =
                [| for segIdx in 0 .. segCount - 1 ->
                       [| for (x, y, th, localLen) in fst segData.[segIdx] ->
                              displace (x, y, th, cumLenAtBez.[segIdx] + localLen) |] |]

            /// Bez points displaced like the interior samples; (x, y, tangent delta, arc
            /// length). Corners use the tangent bisector as the displacement direction so
            /// both sides share a single displaced point.
            let dispBez =
                [| for i in 0 .. n - 1 ->
                       let bp = bezPts.[i]
                       let sLen = cumLenAtBez.[min i segCount]

                       let thMid =
                           if not isClosed && i = 0 then bp.th_out
                           elif not isClosed && i = n - 1 then bp.th_in
                           else norm (bp.th_in + norm (bp.th_out - bp.th_in) / 2.0)

                       let (x, y, th, _) = displace (bp.x, bp.y, thMid, sLen)
                       (x, y, th - thMid, sLen) |]

            /// Broad-nib width factor for a stroke running at angle th: 1.0 when the stroke
            /// is perpendicular to the nib, shrinking toward 0.05 when it runs along the nib.
            let nibFactor th = max (1.0 - nib * (1.0 - abs (sin (th - nibAngle)))) 0.05

            /// Stroke half-width at a given arc length and spine tangent direction.
            let widthAt (sLen: float) (th: float) =
                let mutable w = fthickness

                if nib > 0.0 then
                    // Broad-nib pen: the ribbon width is the projection of the nib onto the
                    // stroke's perpendicular, so strokes along the nib angle nearly vanish.
                    // Clamp to 5% so the outline never degenerates completely.
                    w <- w * nibFactor th

                if taper > 0.0 && not isClosed then
                    // Brush ends: narrow over the first and last (taper/2) fraction of the
                    // stroke's arc length, down to taper_end of full width at the very tips
                    // (taper_end = 0 gives a sharp point).
                    let sFrac = sLen / totalLen
                    let ramp = 0.5 * taper
                    let rampF = min 1.0 (min sFrac (1.0 - sFrac) / ramp)
                    w <- w * (taperEnd + (1.0 - taperEnd) * rampF)

                w

            // Build one side (outer when sign=+1, inner when sign=-1) of the offset polyline.
            // Smooth bezier points emit a single perpendicular offset. Corner bezier points
            // (th_in ≠ th_out) reuse the offsetSegment-Corner logic: outer convex bends get a
            // pair of miter points, inner convex bends collapse to a single bisector point
            // clamped to the chord lengths, avoiding self-intersection on sharp inner corners.
            let buildSide (sign: float) (includeEnds: bool) =
                let reverse = sign < 0.
                let perpAngle = sign * PI / 2.
                let knots = System.Collections.Generic.List<Knot>()

                let emitPerp x y th sLen =
                    knots.Add(plainKnot (addPolarContrast x y (th + perpAngle) (widthAt sLen th)))

                let emitAtBezPt (i: int) =
                    let bp = bezPts.[i]
                    let (bx, by, dTh, sLen) = dispBez.[i]
                    let isCorner = abs (norm (bp.th_out - bp.th_in)) > 1e-3
                    if not isCorner then
                        emitPerp bx by (bp.th_in + dTh) sLen
                    else
                        let wIn  = widthAt sLen (bp.th_in  + dTh)
                        let wOut = widthAt sLen (bp.th_out + dTh)
                        let prev = bezPts.[(i - 1 + n) % n]
                        let nxt  = bezPts.[(i + 1) % n]
                        let prevLen = hypot (bp.x - prev.x) (bp.y - prev.y)
                        let nextLen = hypot (nxt.x - bp.x) (nxt.y - bp.y)
                        let th1 = norm (bp.th_in + dTh + perpAngle)
                        let th2 = norm (bp.th_out + dTh + perpAngle)
                        let bend = norm (th2 - th1)
                        if abs (wIn - wOut) > 1e-6 then
                            // Nib (or other direction-varying width): the two sides of the
                            // corner have different half-widths, so a single-width bisector
                            // miter doesn't meet the wider side's body edge (it leaves a
                            // notch/bevel). Use the exact intersection of the two offset
                            // edge lines — incoming edge offset by wIn, outgoing by wOut.
                            let p1 = addPolarContrast bx by th1 wIn
                            let p2 = addPolarContrast bx by th2 wOut
                            let d1x, d1y = cos (bp.th_in + dTh), sin (bp.th_in + dTh)
                            let d2x, d2y = cos (bp.th_out + dTh), sin (bp.th_out + dTh)
                            let det = -d1x * d2y + d2x * d1y
                            if abs det < 1e-9 then
                                knots.Add(plainKnot (addPolarContrast bx by th1 ((wIn + wOut) / 2.0)))
                            else
                                let s = (-(p2.x - p1.x) * d2y + d2x * (p2.y - p1.y)) / det
                                let mx = p1.x + s * d1x
                                let my = p1.y + s * d1y
                                // Clamp the miter length to the chord lengths (as the
                                // equal-width path does) to avoid self-intersection on
                                // short segments / very sharp corners.
                                let mdist = hypot (mx - bx) (my - by)
                                let maxd = min (min prevLen nextLen) (4.0 * fthickness)
                                if mdist > maxd && mdist > 1e-9 then
                                    let k = maxd / mdist
                                    knots.Add(plainKnot { x = bx + (mx - bx) * k; y = by + (my - by) * k; x_fit = false; y_fit = false })
                                else
                                    knots.Add(plainKnot { x = mx; y = my; x_fit = false; y_fit = false })
                        else
                        let w = wIn
                        let isSharperThanRight = abs bend >= 2.0
                        let isOuter =
                            (not reverse && bend < -PI / 8.0)
                            || (reverse && bend > PI / 8.0)
                        if isOuter && isSharperThanRight then
                            // Two miter points (mirrors offsetSegment Corner outer-bend case).
                            let pa = addPolarContrast bx by (this.maybeAlign th1 - perpAngle / 2.0) (w * sqrt 2.0)
                            let pb = addPolarContrast bx by (this.maybeAlign th2 + perpAngle / 2.0) (w * sqrt 2.0)
                            knots.Add(plainKnot pa)
                            knots.Add(plainKnot pb)
                        else
                            // Single sharp miter, clamped to chord lengths to avoid overshoot.
                            let alpha = bend / 2.0
                            let offset = min (min (w / cos alpha) prevLen) nextLen
                            let p = addPolarContrast bx by (th1 + alpha) offset
                            knots.Add(plainKnot p)

                let emitMidSamples (segIdx: int) =
                    for (x, y, th, sLen) in spineSamples.[segIdx] do
                        emitPerp x y th sLen

                if isClosed then
                    for i in 0 .. n - 1 do
                        emitAtBezPt i
                        emitMidSamples i
                else
                    // Open: caps own bp[0] and bp[n-1]; emit only interior bezier points,
                    // unless the end style needs the body to reach the stroke endpoints.
                    if includeEnds then
                        let (bx, by, dTh, _) = dispBez.[0]
                        emitPerp bx by (bezPts.[0].th_out + dTh) 0.0
                    emitMidSamples 0
                    for i in 1 .. n - 2 do
                        emitAtBezPt i
                        emitMidSamples i
                    if includeEnds then
                        let (bx, by, dTh, _) = dispBez.[n - 1]
                        emitPerp bx by (bezPts.[n - 1].th_in + dTh) totalLen

                List.ofSeq knots

            // For open strokes, reuse the same cap/joint/serif/flare logic.
            // startCap and endCap both need a Segment describing the endpoint.
            // endCap also needs the penultimate Segment for its tangentEnd.
            //
            // The cap function sets th_in on the first knot and th_out on the last knot
            // to the spine tangent so the OLD outline can render a smooth cap-to-body
            // cubic over the whole adjacent segment. In the polyline approach those same
            // tangents create a tight S-curve over the short chord from cap to first/last
            // body sample (the "extra sharp points" at curve endings on glyphs like 'c').
            // Strip them so the cap-to-body edges are straight lines, matching the body
            // polyline. Intermediate cap knots (serif/flare/bulb) are untouched.
            let makeCaps () =
                let firstSeg     = makeCapSeg bezPts.[0]   bezPts.[0].th_out   bezPts.[0].th_out
                let endpointSeg  = makeCapSeg bezPts.[n-1] bezPts.[n-1].th_in  bezPts.[n-1].th_in
                // penultimateSeg.tangentEnd is the incoming tangent at the final point
                let penultSeg    = makeCapSeg bezPts.[n-2] bezPts.[n-2].th_out bezPts.[n-1].th_in

                let stripBoundaryTangents (caps: Knot list) =
                    match caps with
                    | [] -> []
                    | _ ->
                        let lastIdx = caps.Length - 1
                        caps |> List.mapi (fun i k ->
                            if i = 0 && i = lastIdx then { k with th_in = None; th_out = None }
                            elif i = 0 then { k with th_in = None }
                            elif i = lastIdx then { k with th_out = None }
                            else k)

                // Use original element (before reduce) so isJoint detects stroke intersections.
                let startCapKnots = this.startCap firstSeg eForJoints startAlign Corner |> stripBoundaryTangents
                let endCapKnots   = this.endCap endpointSeg penultSeg eForJoints endAlign Corner |> stripBoundaryTangents
                startCapKnots, endCapKnots

            // --- Möbius ribbon panels ---
            // The stroke is drawn as a ribbon twisting about its spine: in 2D projection
            // the apparent width is |cos θ(s)| of the full width, where the twist angle θ
            // advances by π per half-twist along the arc length. Each span between the
            // zero-width pinch points becomes its own closed panel, so the glyph reads as
            // a ribbon seen alternately from the front and the back (and the panels are
            // ready for per-panel front/back shading later).
            let buildMobiusPanels () =
                let halfTwists = max 1 (int (Math.Round(totalLen * mobius / mobiusHalfTwistLen)))

                let widthMobius sLen th =
                    let theta = PI * float halfTwists * sLen / totalLen
                    // Keep a hairline width right at the pinch so panels never degenerate.
                    widthAt sLen th * max (abs (cos theta)) 0.02

                // Ordered spine samples over the whole curve. Corners contribute one
                // entry per tangent so panel edges turn with the corner.
                let flatSamples =
                    let bezEntries (i: int) =
                        let bp = bezPts.[i]
                        let (bx, by, dTh, sLen) = dispBez.[i]
                        let isCorner = abs (norm (bp.th_out - bp.th_in)) > 1e-3
                        if isCorner then
                            [ (bx, by, bp.th_in + dTh, sLen); (bx, by, bp.th_out + dTh, sLen) ]
                        else
                            [ (bx, by, bp.th_in + dTh, sLen) ]

                    if isClosed then
                        [ for i in 0 .. n - 1 do
                              yield! bezEntries i
                              yield! spineSamples.[i] ]
                    else
                        [ let (bx, by, dTh, _) = dispBez.[0]
                          yield (bx, by, bezPts.[0].th_out + dTh, 0.0)
                          yield! spineSamples.[0]
                          for i in 1 .. n - 2 do
                              yield! bezEntries i
                              yield! spineSamples.[i]
                          let (ex, ey, edTh, _) = dispBez.[n - 1]
                          yield (ex, ey, bezPts.[n - 1].th_in + edTh, totalLen) ]

                let samplesArr = Array.ofList flatSamples

                /// Spine point at arc length target, interpolated between adjacent samples.
                let pinchKnotAt (target: float) =
                    let m = samplesArr.Length
                    let sOf j = let (_, _, _, s) = samplesArr.[j] in s
                    let mutable i = -1
                    let mutable j = 0
                    while i < 0 && j < m - 1 do
                        if sOf j <= target && target <= sOf (j + 1) then i <- j
                        j <- j + 1
                    let (x1, y1, _, s1), (x2, y2, _, s2) =
                        if i < 0 then
                            // wrap (closed curves): between the last sample and the curve start
                            let (x1, y1, th1, s1) = samplesArr.[m - 1]
                            let (x2, y2, th2, _) = samplesArr.[0]
                            (x1, y1, th1, s1), (x2, y2, th2, totalLen)
                        else
                            samplesArr.[i], samplesArr.[i + 1]
                    let t = if s2 - s1 < 1e-9 then 0.5 else (target - s1) / (s2 - s1)
                    plainKnot { x = x1 + (x2 - x1) * t; y = y1 + (y2 - y1) * t; x_fit = false; y_fit = false }

                let edgeKnots sign samples =
                    [ for (x, y, th, s) in samples ->
                          plainKnot (addPolarContrast x y (th + sign * PI / 2.0) (widthMobius s th)) ]

                let pinches =
                    [ for j in 0 .. halfTwists - 1 -> (float j + 0.5) / float halfTwists * totalLen ]

                if not isClosed then
                    // Standard caps only fit a full-width stroke end; pinch to the spine
                    // endpoint instead when nib/taper have altered the end width.
                    let startCapKnots, endCapKnots =
                        if nib > 0.0 || taper > 0.0 then [], [] else makeCaps ()

                    let boundsList = 0.0 :: pinches @ [ totalLen ]

                    [ for pi in 0 .. List.length boundsList - 2 do
                          let a = boundsList.[pi]
                          let b = boundsList.[pi + 1]

                          let inside =
                              flatSamples
                              |> List.filter (fun (_, _, _, s) -> s > a + 1e-6 && s < b - 1e-6)

                          if not inside.IsEmpty then
                              let startKnots =
                                  if pi = 0 && not startCapKnots.IsEmpty then startCapKnots
                                  else [ pinchKnotAt a ]

                              let endKnots =
                                  if pi = List.length boundsList - 2 && not endCapKnots.IsEmpty then endCapKnots
                                  else [ pinchKnotAt b ]

                              yield
                                  Curve(
                                      startKnots
                                      @ edgeKnots 1.0 inside
                                      @ endKnots
                                      @ (edgeKnots -1.0 inside |> List.rev),
                                      true
                                  ) ]
                else
                    // Panels wrap around the closed curve from pinch to pinch.
                    [ for pi in 0 .. halfTwists - 1 do
                          let a = pinches.[pi]
                          let b =
                              if pi = halfTwists - 1 then pinches.[0] + totalLen
                              else pinches.[pi + 1]

                          let inside =
                              flatSamples
                              |> List.map (fun (x, y, th, s) ->
                                  (x, y, th, s, (if s > a + 1e-6 then s else s + totalLen)))
                              |> List.filter (fun (_, _, _, _, s') -> s' > a + 1e-6 && s' < b - 1e-6)
                              |> List.sortBy (fun (_, _, _, _, s') -> s')
                              |> List.map (fun (x, y, th, s, _) -> (x, y, th, s))

                          if not inside.IsEmpty then
                              let bWrapped = if b > totalLen then b - totalLen else b

                              yield
                                  Curve(
                                      [ pinchKnotAt a ]
                                      @ edgeKnots 1.0 inside
                                      @ [ pinchKnotAt bWrapped ]
                                      @ (edgeKnots -1.0 inside |> List.rev),
                                      true
                                  ) ]

            // Squeeze a cap's knots perpendicular to the stroke tangent so the cap width
            // matches the nib-modulated body width at that endpoint, while keeping the cap's
            // extension along the tangent. This gives nib strokes the same end length and cap
            // style (axis-align/serif/flare/bulb) as normal strokes, just at the nib width —
            // instead of stopping short at the spine endpoint or protruding as full-width ears.
            let squeezeCapToNib (cx: float) (cy: float) (th: float) (knots: Knot list) =
                let nf = nibFactor th
                knots
                |> List.map (fun k ->
                    let dx = k.pt.x - cx
                    let dy = k.pt.y - cy
                    let along = dx * cos th + dy * sin th
                    let perp = -dx * sin th + dy * cos th
                    let perp' = perp * nf
                    { k with
                        pt =
                            { k.pt with
                                x = cx + along * cos th - perp' * sin th
                                y = cy + along * sin th + perp' * cos th } })

            if mobius > 0.0 then
                buildMobiusPanels () |> List.map this.applySoftCorners
            elif isClosed then
                let outer = buildSide  1.0 false
                let inner = buildSide -1.0 false |> List.rev
                [ Curve(outer, true); Curve(inner, true) ]
                |> List.map this.applySoftCorners
            elif taper > 0.0 && taperEnd <= 0.0 then
                // Pointed ends: the width shrinks to nothing at the spine endpoints, so
                // the two sides simply meet there — a pointed-brush lift. No cap geometry.
                let startPt = plainKnot { x = bezPts.[0].x; y = bezPts.[0].y; x_fit = false; y_fit = false }
                let endPt = plainKnot { x = bezPts.[n - 1].x; y = bezPts.[n - 1].y; x_fit = false; y_fit = false }
                let outer = buildSide  1.0 false
                let inner = buildSide -1.0 false |> List.rev
                [ Curve([ startPt ] @ outer @ [ endPt ] @ inner, true) ]
                |> List.map this.applySoftCorners
            elif taper > 0.0 then
                // Flat ends at the (reduced) end width: close the outline with a straight
                // edge across each endpoint. The body reaches the endpoints at the tapered
                // width via widthAt. Used when taper_end > 0 (tapered but not to a point).
                let outer = buildSide  1.0 true
                let inner = buildSide -1.0 true |> List.rev
                [ Curve(outer @ inner, true) ]
                |> List.map this.applySoftCorners
            elif nib > 0.0 then
                // Broad-nib caps: use the normal cap geometry (so ends extend to the same
                // length as a non-nib stroke), then squeeze each cap to the nib width so it
                // matches the body and doesn't protrude.
                let startCapKnots, endCapKnots = makeCaps ()
                let startCapKnots = squeezeCapToNib bezPts.[0].x bezPts.[0].y bezPts.[0].th_out startCapKnots
                let endCapKnots   = squeezeCapToNib bezPts.[n - 1].x bezPts.[n - 1].y bezPts.[n - 1].th_in endCapKnots
                let outer = buildSide  1.0 false
                let inner = buildSide -1.0 false |> List.rev
                [ Curve(startCapKnots @ outer @ endCapKnots @ inner, true) ]
                |> List.map this.applySoftCorners
            else
                let startCapKnots, endCapKnots = makeCaps ()
                let outer = buildSide  1.0 false
                let inner = buildSide -1.0 false |> List.rev
                // Path: startCap → outer body → endCap → inner body (reversed)
                [ Curve(startCapKnots @ outer @ endCapKnots @ inner, true) ]
                |> List.map this.applySoftCorners

        let solveAndOffset (pts: Knot list) isClosed =
            validateKnotSequence pts isClosed
            let startAlign = pts.IsEmpty || isClosed || not (isFreeCurveEnd pts.[0].ty)
            let endAlign   = pts.IsEmpty || isClosed || not (isFreeCurveEnd (List.last pts).ty)
            let ctrlPts = toDactylSplineControlPoints pts
            let spline = DactylSpline(ctrlPts, isClosed)
            let bezPts =
                spline.solveAndGetPoints (axes.max_spline_iter, axes.flatness, axes.end_flatness, axes.debug)
                |> this.shearBezPts
            buildOutlineFromBez bezPts isClosed startAlign endAlign

        let rec dactylToOutline elem =
            match elem with
            | Curve(pts, isClosed) -> solveAndOffset pts isClosed
            | Dot(p) ->
                let p = this.italicisePt p
                [ dotToClosedCurve p.x p.y (thickness + 5.0) ]
            | EList(elems) -> List.collect dactylToOutline elems
            | Space -> [ Space ]
            | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

        let results = dactylToOutline (this.reduce e)
        if results.Length = 0 then Space
        elif results.Length = 1 then results.[0]
        else EList(results)

    member this.getOutline =
        if this.axes.stroked then
            this.getStroked
        elif this.axes.scratches then
            this.getScratches
        elif this.axes.outline then
            (if axes.constant_offset || (axes.dactyl_spline && axes.sampledArtistic) then
                 this.getDactylConstantOffsetOutlines
             elif axes.dactyl_spline then
                 this.getDactylSansOutlines
             else
                 this.getSpiroSansOutlines)
            >> applyIf axes.constraints this.constrainTangents
        else
            id

    ///Ensure tangents fall within bounds of glpyh. Note we need the pre-constrained tangents in order
    /// to do this reliably.
    member this.constrainTangents elem =
        let l, r, b, t = bounds elem

        let tangentAngle (point: Point) type_ angle =
            let x, y = point.x, point.y
            let angle = norm angle

            if type_ <> G2 then
                None
            elif x = l && y = t then
                if angle < -PI / 4. then Some(-PI / 2.) else Some 0.
            elif x = l && y = b then
                if angle < PI / 4. then Some 0. else Some(PI / 2.)
            elif x = r && y = t then
                if angle < -PI * 3. / 4. then Some -PI else Some(-PI / 2.)
            elif x = r && y = b then
                if angle < PI * 3. / 4. then Some(PI / 2.) else Some PI
            elif x = l || x = r then
                if angle < 0. then Some(-PI / 2.) else Some(PI / 2.)
            elif y = t then
                if angle < -PI / 2. || angle > PI / 2. then
                    Some -PI
                else
                    Some 0.
            elif y = b then
                if angle < PI / 2. && angle > -PI / 2. then
                    Some 0.
                else
                    Some PI
            else
                None

        let constrainSegment (segment: SpiroSegment) =
            let pt = segment.Point
            let tangent = tangentAngle pt segment.Type segment.tangent1

            { pt = pt
              ty = segment.Type
              th_in = tangent
              th_out = tangent
              label = None }

        let spiroConstrain spiro =
            match spiro with
            | SpiroOpenCurve(segments) -> [ Curve(List.map constrainSegment segments, false) ]
            //   @ [
            //     let angle = PI - segments.[segments.Length-2].tangent2 in
            //      let lastSeg = segments.[segments.Length-1] in
            //     (lastSeg.Point, lastSeg.Type, (tangentAngle lastSeg.Point lastSeg.Type angle))], false)]
            | SpiroClosedCurve(segments) -> [ Curve(List.map constrainSegment segments, true) ]
            | SpiroDot(p) -> [ Dot(p) ]
            | SpiroSpace -> [ Space ]

        applyToSegments spiroConstrain elem

    ///Convert spiro curves to bezier SVG curves, returning (pathSvg, combSvg)
    member this.spiroToSvgWithComb spiro =
        match spiro with
        | SpiroOpenCurve(segs) ->
            let bc = SpiroCombContext(showComb, segs.Length * 20)
            Spiro.SpirosToBezier (Array.ofList segs) false bc |> ignore
            ([ bc.GetPathData ], bc.GetCombSvg)
        | SpiroClosedCurve(segs) ->
            let bc = SpiroCombContext(showComb, segs.Length * 20)
            Spiro.SpirosToBezier (Array.ofList segs) true bc |> ignore
            ([ bc.GetPathData ], bc.GetCombSvg)
        | SpiroDot(p) -> (svgCircle p.x p.y thickness, [])
        | SpiroSpace -> ([], [])

    ///Convert spiro curves to bezier SVG curves (path only, for callers that don't need comb)
    member this.spiroToSvg spiro =
        match spiro with
        | SpiroOpenCurve(segs) ->
            let bc = PathBezierContext()
            Spiro.SpirosToBezier (Array.ofList segs) false bc |> ignore
            [ bc.ToString ]
        | SpiroClosedCurve(segs) ->
            let bc = PathBezierContext()
            Spiro.SpirosToBezier (Array.ofList segs) true bc |> ignore
            [ bc.ToString ]
        | SpiroDot(p) -> svgCircle p.x p.y thickness
        | SpiroSpace -> []

    ///Convert element to bezier SVG curves
    member this.elementToSvg elem =
        if axes.spline2 then
            elementToSpline2Svg elem
        else if axes.dactyl_spline then
            elementToDactylSvg elem
        else
            let segs = elementToSpiroSegments elem
            let results = segs |> List.map this.spiroToSvgWithComb
            let svgPaths = results |> List.collect fst
            let combPaths = results |> List.collect snd
            (svgPaths, combPaths, [])

    member this.elementToSvgPath (element: Element) (offsetX: float) (offsetY: float) (strokeWidth: float) fillColour (suppressTangents: bool) =
        let GetStableHash (str: string) =
#if FABLE_COMPILER
            let mutable hash = 5381

            for i in 0 .. str.Length - 1 do
                hash <- ((hash <<< 5) + hash) + int str.[i]

            sprintf "%x" hash
#else
            use sha256 = System.Security.Cryptography.SHA256.Create()
            let bytes = System.Text.Encoding.UTF8.GetBytes(str)
            let hash = sha256.ComputeHash(bytes)
            // convert first 8 bytes to hex string
            System.BitConverter.ToString(hash, 0, 8).Replace("-", "").ToLowerInvariant()
#endif

        let fillrule = "nonzero"

        let fillStyle =
            if this.axes.outline && this.axes.filled then
                fillColour
            else
                "none"

        let svg, combSvg, tangentSvg = this.elementToSvg element
        let guid = GetStableHash(String.concat "\n" svg)

        [ if axes.clip_rect then
              sprintf "<clipPath id='clip_%s'>" guid
              let margin = thickness * 2.0 // Changed to float multiplication

              sprintf
                  "<rect x='%.0f' y='%.0f' width='%.0f' height='%.0f'/>"
                  (-margin)
                  (_metrics.D - margin)
                  (float (this.width element) + margin)
                  (this.charHeight + margin)

              "</clipPath>"
          "<path "
          "d='" ]
        @ svg
        @ [ "'"
            sprintf "transform='translate(%.0f,%.0f) scale(1,-1)'" (float offsetX) (float offsetY)
            sprintf
                "style='fill:%s;fill-rule:%s;stroke:%s;stroke-width:%.0f'"
                fillStyle
                fillrule
                fillColour
                (float strokeWidth)
            sprintf "clip-path='url(#clip_%s)'" guid
            "/>" ]
        // Add separate path for comb if present
        @ if List.isEmpty combSvg then
              []
          else
              // Spiro combs are rendered in very light grey; other curves use black
              let combStroke =
                  if axes.dactyl_spline then "#000000" else "#808080"
              [ "<g class='comb-layer'>"; "<path "; "d='" ]
              @ combSvg
              @ [ "'"
                  sprintf "transform='translate(%.0f,%.0f) scale(1,-1)'" (float offsetX) (float offsetY)
                  sprintf "style='fill:none;stroke:%s;stroke-width:1'" combStroke
                  "/>"
                  "</g>" ]
        // Add separate path for tangents if present (suppressed for outline/inferred elements)
        @ if suppressTangents || List.isEmpty tangentSvg then
              []
          else
              [ "<g class='tangent-layer'>"; "<path "; "d='" ]
              @ tangentSvg
              @ [ "'"
                  sprintf "transform='translate(%.0f,%.0f) scale(1,-1)'" (float offsetX) (float offsetY)
                  sprintf "style='fill:none;stroke:#e00000;stroke-width:2'" // Red
                  "/>"
                  "</g>" ]


    member this.translateByThickness = translateBy thickness thickness

    member this.charToElem ch =
        Glyph(ch)
        |> this.reduce
        |> applyIf axes.constraints this.constrainTangents
        |> this.monospace
        |> this.translateByThickness
        |> this.italicise

    member this.charToSvg ch offsetX offsetY colour =
        if axes.debug then
            printfn "%c" ch

        (sprintf "<!-- %c -->" ch)
        :: let backbone = this.charToElem ch in
           let knotColour = if this.axes.outline then lightBlue else pink in
           let knotSize = if this.axes.outline then 4.0 else 20.0 in

           // outlineFont has smooth=false so DactylSpline stays O(n) on the dense outline
           // Corner knots; cached on the Font instance to avoid per-character allocation.
           let outlineFont = this.outlineFont in

           try
               // Spine is solved with smooth (this), outline knots rendered without smooth (outlineFont).
               let outline = this.getOutline backbone

               if axes.debug then
                   printfn "outline: %A" outline

               (outlineFont.elementToSvgPath outline offsetX offsetY 5.0 colour false)
               @ (if this.axes.show_knots && this.axes.outline then
                      outline
                      |> SvgHelpers.getSvgKnots offsetX offsetY knotSize knotColour outlineFont.isJoint
                  else
                      [])
           with ex ->
               printfn "EXCEPTION IN getOutline: %O\nFallback backbone only" ex

               outlineFont.elementToSvgPath
                   (Dot(
                       { y = _metrics.H
                         x = _metrics.C
                         y_fit = false
                         x_fit = false }
                   ))
                   offsetX
                   offsetY
                   5.0
                   red
                   false
               @ (if this.axes.show_knots then
                      backbone |> SvgHelpers.getSvgKnots offsetX offsetY knotSize knotColour this.isJoint
                  else
                      [])

    member this.width e =
        (e |> this.reduce |> this.monospace |> this.elemWidth)
        + float this.axes.tracking
        + ((1.0 + this.axes.contrast) * thickness * 2.0 + float this.axes.serif)

    member this.charWidth ch = this.width (Glyph(ch))

    member this.charWidths str =
        Seq.map this.charWidth str |> List.ofSeq

    member this.stringWidth str = List.sum (this.charWidths str)

    member this.stringToSvgLineInternal
        (lines: string list)
        (offsetX: float)
        (offsetY: float)
        colour
        (progress: (float -> unit) option)
        =
        let totalChars = lines |> List.sumBy (fun s -> s.Length)
        let mutable charCount = 0

        let svg, lineWidths =
            List.unzip
                [ for i in 0 .. lines.Length - 1 do
                      let str = lines.[i]
                      let widths = this.charWidths str
                      let offsetXs = List.scan (+) offsetX widths

                      let lineOffset =
                          offsetY + this.charHeight * float (i + 1) - this.yBaselineOffset + thickness

                      let svg =
                          [ for c in 0 .. str.Length - 1 do
                                charCount <- charCount + 1

                                match progress with
                                | Some p -> p (float charCount / float totalChars)
                                | None -> ()

                                yield! this.charToSvg str.[c] (offsetXs.[c]) lineOffset colour ]

                      (svg, List.sum widths) ]

        (List.collect id svg, lineWidths)

    member this.stringToSvgLines (lines: string list) (offsetX: float) (offsetY: float) colour =
        fst (this.stringToSvgLineInternal lines offsetX offsetY colour None)

    member this.stringToSvg
        (lines: string list)
        (offsetX: float)
        (offsetY: float)
        autoscale
        colour
        (progress: (float -> unit) option)
        =
        let margin = 50.0

        let svg, lineWidths =
            this.stringToSvgLineInternal lines offsetX offsetY colour progress

        let w, h =
            if autoscale then
                (float (List.max lineWidths) + margin), (this.charHeight * float lineWidths.Length + margin)
            else
                6000.0, 6000.0

        toSvgDocument -margin -margin w h svg

    member this.CharToOutline ch = this.charToElem ch |> this.getOutline

    /// Returns the solved backbone (x, y) positions for every Curve in the glyph.
    /// Uses the DactylSpline solver, so call this with dactyl_spline = true.
    member this.charToSolvedBackbonePoints ch =
        let elem = this.charToElem ch
        let rec collect elem =
            match elem with
            | Curve(pts, isClosed) ->
                let ctrlPts = toDactylSplineControlPoints pts
                let spline = DactylSpline(ctrlPts, isClosed)
                let bezPts = spline.solveAndGetPoints(axes.max_spline_iter, axes.flatness, axes.end_flatness, false)
                bezPts |> Array.toList |> List.map (fun bp -> bp.x, bp.y)
            | EList(elems) -> List.collect collect elems
            | _ -> []
        collect elem

    member this.Spline2PtsToSvg pts isClosed = spline2ptsToSvg pts isClosed
