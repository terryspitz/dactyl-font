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
type Font(axes: Axes) =
    //basic manipulation using class variables

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
        ([ spline.renderSvg ], [], if axes.show_tangents then spline.renderTangents else [])

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
                debug = axes.debug,
                showComb = axes.show_comb,
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
            let monoFn (p: Point) =
                let full_scale = _metrics.monospaceWidth / (this.elemWidth e)
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
                          assert ((x1 * perpX + y1 * perpY) = (x2 * perpX + y2 * perpY))
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
    member this.cap X Y theta isJoint ty firstThIn firstThOut lastThIn lastThOut =
        let fthickness = float thickness
        let serif = float this.axes.serif
        let thetaAligned = this.maybeAlign theta
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
                label = None }
              { pt = offsetPointRotated X Y (align theta) fthickness -fthickness
                ty = ty
                th_in = lastThIn
                th_out = lastThOut
                label = None } ]
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
    member this.startCap (seg: Segment) elem ty =
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

    /// End-cap knots: points at the beginning of an open stroke.
    /// Outline path: ... fwdOffset → endCap[first] → ... → endCap[last] → revOffset ...
    /// First point faces forward offset, last faces reversed offset (tangent flipped).
    member this.endCap (seg: Segment) (lastSeg: Segment) elem ty =
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
            if reverse then
                match seg.Type with
                | SpiroPointType.Left -> SpiroPointType.Right
                | SpiroPointType.Right -> SpiroPointType.Left
                | _ -> seg.Type
            else
                match seg.Type with
                | SpiroPointType.EndOpenContour -> Corner
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
    static member flipTangent(k: Knot) =
        { k with
            th_in = k.th_out |> Option.map (fun t -> norm (t + PI))
            th_out = k.th_in |> Option.map (fun t -> norm (t + PI)) }

    /// Replace sharp Corner knots with small arcs (LineToCurve → CurveToLine)
    /// to produce rounded corners. The radius is proportional to soft_corners * thickness.
    /// Short segments (like end caps) are protected by clamping the radius.
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

                if k.ty <> Corner then
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
                            result.Add({ k with pt = inPt; ty = CurveToLine
                                                th_out = k.th_in })
                            result.Add({ k with ty = G2
                                                th_in = None; th_out = None })
                            result.Add({ k with pt = outPt; ty = LineToCurve
                                                th_in = k.th_out })
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

        let startCap (seg: Segment) =
            let ty =
                if seg.Type = SpiroPointType.Anchor then
                    SpiroPointType.Anchor
                else
                    Corner

            this.startCap seg e ty

        let endCap (seg: Segment) (lastSeg: Segment) =
            let ty =
                if seg.Type = SpiroPointType.Anchor then
                    SpiroPointType.Anchor
                else
                    Corner

            this.endCap seg lastSeg e ty

        let spiroToOutline spiro =
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

        applyToSegments spiroToOutline e

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
                  let offset = (float thickness) * (float i / float (lines - 1) - 0.5) * 2.0
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

    member this.italicise =
        applyIf
            (this.axes.italic <> 0.0)
            ((applyIf (not (this.axes.spline2 || this.axes.dactyl_spline)) this.subdivide)
             >> (movePoints this.italicisePt (Some this.italiciseTan)))

    member this.getDactylSansOutlines e =
        let fthickness = float thickness

        let startCap (seg: Segment) = this.startCap seg e Corner
        let endCap (seg: Segment) (lastSeg: Segment) = this.endCap seg lastSeg e Corner

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
                spline.solveAndGetPoints (axes.max_spline_iter, axes.flatness, axes.debug)

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
                let segs = solveCurveSegs pts isClosed

                if axes.debug then
                    printfn "dactylToOutline curve: %A" segs

                this.strokeSegments segs fthickness startCap endCap isClosed
                |> List.map this.applySoftCorners
            | Dot(p) -> [ dotToClosedCurve p.x p.y (thickness + 5.0) ]
            | EList(elems) -> List.collect dactylToOutline elems
            | Space -> [ Space ]
            | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

        let results = dactylToOutline (this.reduce e)
        if results.Length = 1 then results.[0] else EList(results)

    member this.getOutline =
        if this.axes.stroked then
            this.getStroked
        elif this.axes.scratches then
            this.getScratches
        elif this.axes.outline then
            (if axes.dactyl_spline then
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

    ///Convert spiro curves to bezier SVG curves
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
            (elementToSpiroSegments elem |> List.collect this.spiroToSvg, [], [])

    member this.elementToSvgPath (element: Element) (offsetX: float) (offsetY: float) (strokeWidth: float) fillColour =
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
              [ "<g class='comb-layer'>"; "<path "; "d='" ]
              @ combSvg
              @ [ "'"
                  sprintf "transform='translate(%.0f,%.0f) scale(1,-1)'" (float offsetX) (float offsetY)
                  sprintf "style='fill:none;stroke:#000000;stroke-width:1'" // Black, min thickness (1px)
                  "/>"
                  "</g>" ]
        // Add separate path for tangents if present
        @ if List.isEmpty tangentSvg then
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
           let knotSize = if this.axes.outline then 10.0 else 20.0 in

           try
               // render outline glyph
               let outline = this.getOutline backbone

               if axes.debug then
                   printfn "outline: %A" outline

               (this.elementToSvgPath outline offsetX offsetY 5.0 colour)
               @ (if this.axes.show_knots && this.axes.outline then
                      outline
                      |> SvgHelpers.getSvgKnots offsetX offsetY knotSize knotColour this.isJoint
                  else
                      [])
           with ex ->
               printfn "EXCEPTION IN getOutline: %O\nFallback backbone only" ex

               this.elementToSvgPath
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
    member this.Spline2PtsToSvg pts isClosed = spline2ptsToSvg pts isClosed
