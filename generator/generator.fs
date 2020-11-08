// Functional Fonts by terryspitz
// Mar 2020-

//TODOs:
// move outline point inward only
// improve serifs
// join lines properly
// correct tight bend in '5'
// render animation
// try merging with https://magenta.tensorflow.org/svg-vae
// add punctuation chars
// 'bowtie' where lines all cross
// mark joins to remove serifs
// generate proofs, ideally using @font-face
// calculate kerning
// from https://www.typography.com/blog/typographic-illusions: 
//  overshoot
//  balance (mid height > 1/2)
// Add help tooltips on explorer sliders
// fix fontforge errors: direction, non-integral coords
// fix serifs: curve joints, check Y{}, spacing
// Caustics overlay
// Optional debug mode to show coordinates/curves

//Features :
// Backscratch font (made of 4 parallel lines)
// Generated FontForge fonts
// Variable font explorer: https://terryspitz.github.io/dactyl-font/
// Mono (fixed-width) font
// Horiz/vertical endcaps using axis_align_caps
// Randomise in explorer
// Contrast (horiz vs vert stroke width ratio)
// Flared endcaps
// Constrain tangents to horiz/vertical
// Italics subdivide splines to ensure better fit
// Dactyl-smooth which has no corners

module Generator


open System
open System.Collections

open GlyphDefs
open GlyphStringDefs
open GeneratorTypes
open SpiroPointType
open SpiroSegment
open SpiroControlPoint
open PathBezierContext
open Axes
open Curves


// Attach extension method to segment class
type SpiroSegment with 
    member this.Point = YX(int this.Y, int this.X)

let PI = Math.PI        

let svgCircle x y r = 
    [
        sprintf "M %d,%d" (x-r) y
        sprintf "C %d,%d %d,%d %d,%d" (x-r) (y+r/2) (x-r/2) (y+r) x (y+r)
        sprintf "C %d,%d %d,%d %d,%d" (x+r/2) (y+r) (x+r) (y+r/2) (x+r) y
        sprintf "C %d,%d %d,%d %d,%d" (x+r) (y-r/2) (x+r/2) (y-r) x (y-r)
        sprintf "C %d,%d %d,%d %d,%d" (x-r/2) (y-r) (x-r) (y-r/2) (x-r) y
        "Z"
    ]

let svgSemiCircle x y r (ch : char) =
    assert "udlr".Contains(ch)
    [
        if ch = 'u' || ch = 'd' then
            sprintf "M %d,%d" (x-r) y
        else 
            sprintf "M %d,%d" x (y-r)
        if ch = 'u' then
            sprintf "C %d,%d %d,%d %d,%d" (x-r) (y+r) (x+r) (y+r) (x+r) y
        elif ch = 'd' then
            sprintf "C %d,%d %d,%d %d,%d" (x-r) (y-r) (x+r) (y-r) (x+r) y
        elif ch = 'l' then
            sprintf "C %d,%d %d,%d %d,%d" (x-r) (y-r) (x-r) (y+r) x (y+r)
        elif ch = 'r' then
            sprintf "C %d,%d %d,%d %d,%d" (x+r) (y-r) (x+r) (y+r) x (y+r)
        "Z"
    ]

let svgText x y text =
    sprintf "<text x='%d' y='%d' font-size='200'>%s</text>" x y text

let toSvgDocument left bottom width height svg =
    [
        "<svg xmlns='http://www.w3.org/2000/svg'"
        sprintf "viewBox='%d %d %d %d'>" left bottom width height
        "<g id='layer1'>"
    ] @ svg @ [
        "</g>"
        "</svg>"
    ]

let toHtmlDocument left bottom width height svg =
    [
        "<body>"
        //from https://www.cssscript.com/svg-pan-zoom-container/
        "<script src='https://cdn.jsdelivr.net/npm/svg-pan-zoom-container@0.1.2'></script>"
        "<div data-zoom-on-wheel='' data-pan-on-drag=''>"
    ] @ toSvgDocument left bottom width height svg @ [
        "</div>"
        "</body>"
    ]

///normalise angle to between PI/2 and -PI/2
let norm th = if th>PI then th-PI*2.0 else if th<(-PI) then th+PI*2.0 else th

let toPolar dy dx = SpiroImpl.hypot dx dy, atan2 dy dx

let black = "#000000"
let red = "#e00000"


//class
type Font (axes: Axes) =
    //basic manipulation using class variables

    let _axes = axes
    let _glyphDefs = GlyphDefs(axes)
    let thickness = axes.thickness
    let getXY = _glyphDefs._getXY

    ///Constrast axis makes verticals thicker, by tweaking the offset in polar coordinates
    let addPolarContrast X Y theta dist =
        YX(int(Y + dist * sin(theta)), int(X + (dist + _axes.contrast * float thickness) * cos(theta)))
    
    let segAddPolar (seg : SpiroSegment) theta dist = 
            addPolarContrast seg.X seg.Y theta dist

    let rec elementToSpiros elem =
        let makeSCP (p, t) = let x,y = getXY p in { SCP.X=float(x); Y=float(y); Type=t}
        match elem with
        | OpenCurve(pts) ->
            match Spiro.SpiroCPsToSegments (List.map makeSCP pts |> Array.ofList) false with
            | Some segs -> [SpiroOpenCurve(Array.toList segs)]
            | None -> [SpiroDot(BC)]
        | ClosedCurve(pts) ->
            match Spiro.SpiroCPsToSegments (List.map makeSCP pts |> Array.ofList) true with
            | Some segs -> [SpiroClosedCurve(Array.toList segs)]
            | None -> [SpiroDot(BC)]
        | Dot(p) -> [SpiroDot(p)]
        | EList(elems) -> List.collect elementToSpiros elems
        | Space -> [SpiroSpace]
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem) 

    let toSplineControlPoints (pts : list<Point * SpiroPointType * float option>) =
        [|for i in 0..pts.Length-1 do
            let p, spiroType, tangent = pts.[i]
            let x,y = getXY p
            if spiroType = SpiroPointType.Anchor then
                let p1, _, _ = pts.[i+1]
                let x2,y2 = getXY (p1 - p)
                let rth = atan2 (float y2) (float x2)
                yield SplineControlPoint(   {x=float x;y=float y},
                                            (if axes.smooth then SplinePointType.Smooth else SplinePointType.Corner),
                                            None, Some rth), spiroType
            else if spiroType <> SpiroPointType.Handle then
                let ty = match spiroType with 
                            | SpiroPointType.Corner 
                            | SpiroPointType.OpenContour | SpiroPointType.EndOpenContour | SpiroPointType.End
                                -> if axes.smooth then SplinePointType.Smooth else SplinePointType.Corner
                            | SpiroPointType.Left 
                                -> if axes.smooth then SplinePointType.Smooth else SplinePointType.CurveToLine
                            | SpiroPointType.Right 
                                -> if axes.smooth then SplinePointType.Smooth else SplinePointType.LineToCurve
                            | SpiroPointType.G2 | SpiroPointType.G4
                                -> SplinePointType.Smooth
                            | _ -> invalidArg "ty" (sprintf "Unexpected SpiroPointType %A" spiroType) 
                // yield SplineControlPoint({x=float x;y=float y}, ty), spiroType
                yield SplineControlPoint({x=float x; y=float y}, ty, tangent, tangent), spiroType
        |]

    let rec elementToSpline elem =
        let toSegs2 (pts : list<Point * SpiroPointType * float option>) isClosed =
            let ctrlPts = toSplineControlPoints pts
            let spline = Spline(Array.map fst ctrlPts, isClosed)
            spline.solve(axes.max_spline_iter)
            // printfn "spline"
            // for pt in spline.ctrlPts do
            //     printfn "%f %f %f %f" pt.pt.x pt.pt.y pt.lTh pt.rTh
            // assert (spline.ctrlPts.Length = pts.Length)  // other than Handles
            [for i in 0..ctrlPts.Length-1 do
                let ty = snd ctrlPts.[i]
                let pt = spline.ctrlPts.[i]
                let pt1 = spline.ctrlPts.[(i+1) % spline.ctrlPts.Length]
                {
                    SpiroSegment.X = pt.pt.x; Y = pt.pt.y; Type = ty
                    bend_th = 0.
                    ks = Array.empty
                    seg_ch = hypot(pt1.pt.x-pt.pt.x,pt1.pt.y-pt.pt.y)
                    seg_th = 0.
                    //spiro has tangents at start/end of segment pointing along curve,
                    //while spline has tangents at point, facing left/right
                    tangent1 = pt.rTh
                    tangent2 = pt1.lTh
                }
            ]
        let toSegs (pts : list<Point * SpiroPointType>) isClosed =
            (pts |> List.map (fun (pt, ty) -> (pt, ty, None)) |> toSegs2) isClosed

        match elem with
        | OpenCurve(pts) ->
            [SpiroOpenCurve(toSegs pts false)]
        | ClosedCurve(pts) ->
            let segs = toSegs pts true
            [SpiroClosedCurve(segs @ [segs.[0]])]
        | TangentCurve(pts, isClosed) -> 
            let segs = toSegs2 pts isClosed
            if isClosed then
                [SpiroClosedCurve(segs @ [segs.[0]])]
            else
                [SpiroOpenCurve(segs)]
        | Dot(p) -> [SpiroDot(p)]
        | EList(elems) -> List.collect elementToSpline elems
        | Space -> [SpiroSpace]
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem) 

    let elementToSpiroSegments elem =
        let spiroElems = 
            if axes.spline_not_spiro then
                elementToSpline elem
            else
                elementToSpiros elem
        let debug = true
        if debug then
            for seg in spiroElems do
                printfn "segments"
                match seg with
                | SpiroOpenCurve(segs) ->
                    for seg in segs do
                        printfn "%f,%f %f %f" seg.X seg.Y seg.tangent1 seg.tangent2
                | SpiroClosedCurve(segs) ->
                    for seg in segs do
                        printfn "%f,%f %f %f" seg.X seg.Y seg.tangent1 seg.tangent2
                | _ -> ()
        spiroElems

    let rec elementToSplineSvg elem =
        let toSvg pts isClosed =
            let ctrlPts = pts |> List.map (fun (pt, ty) -> (pt, ty, None)) |>toSplineControlPoints 
            let spline = Spline(Array.map fst ctrlPts, isClosed)
            spline.solve(axes.max_spline_iter)
            [spline.renderSvg(axes.show_tangents) ]
        match elem with
        | OpenCurve(pts) -> toSvg pts false
        | ClosedCurve(pts) -> toSvg pts true
        | Dot(p) -> let x, y = getXY p in svgCircle x y thickness
        | EList(elems) -> List.collect elementToSplineSvg elems
        | Space -> []
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem) 

    //apply a transformation fn: Spiros -> Elements
    let applyToSegments fn elem = 
        let elems = elementToSpiroSegments elem |> List.collect fn
        if elems.Length > 1 then EList(elems) else elems.[0]

    //cache fn results to avoid recalcs
    //inside Font class, so it gets rebuilt for each new instance with different axes
    let memoize fn =
      let cache = new System.Collections.Generic.Dictionary<_,_>()
      (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ -> let v = fn x
                      cache.Add(x,v)
                      v)

    let rec bounds elem =
        let dummy = -999
        let safeMinMax mm x y = if x = dummy then y else mm x y
        let bound minmax fstsnd pts = List.fold minmax dummy (List.map (fst >> getXY >> fstsnd) pts)
        match elem with
        | OpenCurve(pts) | ClosedCurve(pts) -> bound (safeMinMax min) fst pts, bound (safeMinMax max) fst pts, bound (safeMinMax min) snd pts, bound (safeMinMax max) snd pts
        | Dot(p) -> let x,y = getXY p in x,x,y,y
        | EList(elems) -> 
            List.fold (fun (x1,x2,y1,y2) (x3,x4,y3,y4) -> safeMinMax min x1 x3, safeMinMax max x2 x4, safeMinMax min y1 y3, safeMinMax max y2 y4)
                (dummy,dummy,dummy,dummy) (List.map bounds elems)
        | Space -> 0,0,0,0
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)

    //MEMBERS

    member this.axes = axes

    member this.reduce e = 
        match e with
        | Glyph(ch) ->
            if axes.new_definitions then
                memoize stringDefsToElem _glyphDefs ch
            else 
                _glyphDefs.getGlyph e |> this.reduce
        | EList(elems) -> EList(List.map this.reduce elems)
        | _ -> _glyphDefs.reduce e

    static member dotToClosedCurve x y r =
        ClosedCurve([(YX(y-r,x), G2); (YX(y,x+r), G2);
                     (YX(y+r,x), G2); (YX(y,x-r), G2)])

    //Straights: AEFHIKLMNTVWXYZklvwxyz147/=[]\`|*"'
    //Dots: ij:;!?
    //Curves: COScos36890()~
    //LeftUpright: BDPRb mnpr 
    //RightUpright: GJadgq
    //Other: QUefhtu25@#$€£_&-+{}%

    member this.elemWidth e =
        let maxX pts = List.fold max 0 (List.map (fst >> getXY >> fst) pts)
        match e with
        | OpenCurve(pts) -> maxX pts
        | ClosedCurve(pts) -> maxX pts
        | Dot(p) -> fst (getXY p)
        | EList(elems) -> List.fold max 0 (List.map this.elemWidth elems)
        | Space -> 
            let space = axes.height / 4  //according to https://en.wikipedia.org/wiki/Whitespace_character#Variable-width_general-purpose_space
            int ((1.0-axes.monospace) * float space + axes.monospace * float _glyphDefs._monospaceWidth)
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" e)

    ///Scale width of glyph to monospace width based on axis monospace fraction
    member this.monospace =
        let mono e =
            let nonMono = GlyphDefs({axes with monospace=0.0})
            let mono p = let x,y = getXY p
                         let full_scale = float _glyphDefs._monospaceWidth / float (this.elemWidth e)
                         let x_scale = (1.0 - axes.monospace) + axes.monospace * full_scale
                         YX(y, int (float x * x_scale))
            movePoints mono e
        applyIf (axes.monospace > 0.0) mono

    member this.charHeight = this.axes.height - _glyphDefs._D + thickness * 2 + this.axes.leading

    ///distance from bottom of descenders to baseline ()
    member this.yBaselineOffset = - _glyphDefs._D + thickness

    member this.isJointRaw (ch, X, Y) =
        let checkXYColinearPoints (pts : list<Point * SpiroPointType>) =
            List.fold (||) false 
                [for i in 0..pts.Length-2 do
                    let (x1,y1),(x2,y2) = getXY (fst pts.[i]),getXY (fst pts.[i+1])
                    if (x1=X && y1=Y) || (x2=X && y2=Y) then
                        false
                    elif not ((x1 <= X && X <= x2 && y1 <= Y && Y <= y2)
                              || (x2 <= X && X <= x1 && y2 <= Y && Y <= y1)) then
                        false
                    else
                        let perpX, perpY = (y2-y1), -(x2-x1)
                        assert ((x1*perpX + y1*perpY) = (x2*perpX + y2*perpY))
                        let perpDist = (X*perpX + Y*perpY) - (x1*perpX + y1*perpY)
                        (perpDist > -thickness) && (perpDist < thickness)
                ]
        //TODO: check joints on curves, or mark manually in reduce fn.
        let rec checkElem e =
            match e with
            | OpenCurve(pts) | ClosedCurve(pts) -> checkXYColinearPoints pts
            | Dot(p) -> false
            | EList(elems) -> List.fold (||) false (List.map checkElem elems)
            | Space -> false
            | _ -> invalidArg "e" (sprintf "Unreduced element %A" e)

        Glyph(ch) |> this.reduce |> checkElem

    member this.isJoint ch (seg : SpiroSegment) = 
        if axes.joints then
            memoize this.isJointRaw (ch, int seg.X, int seg.Y)
        else
            false

    ///align an angle to horizontal or vertical axis
    member this.align angle =
        if this.axes.axis_align_caps then
            let angle = norm angle
            if abs angle < PI / 4.0 then
                0.0
            elif abs angle > PI * 3.0 / 4.0 then
                PI
            elif angle > 0.0 then
                PI/2.
            else
                -PI/2.                                
        else
            angle        

    member this.reverseSegments (segments : SpiroSegment list) =
        [for seg in List.rev segments do
            let newType = 
                match seg.Type with
                | SpiroPointType.OpenContour -> SpiroPointType.EndOpenContour
                | SpiroPointType.EndOpenContour -> SpiroPointType.OpenContour
                | SpiroPointType.Left -> SpiroPointType.Right
                | SpiroPointType.Right -> SpiroPointType.Left
                | _ -> seg.Type
            {seg with Type=newType}
        ]

    member this.offsetSegment (seg : SpiroSegment) (lastSeg : SpiroSegment) reverse dist =
        let segType = if reverse then lastSeg.Type else seg.Type
        let newType = if reverse then 
                        match seg.Type with
                        | SpiroPointType.Left -> SpiroPointType.Right
                        | SpiroPointType.Right -> SpiroPointType.Left
                        | _ -> seg.Type
                       else 
                        match seg.Type with
                        | SpiroPointType.EndOpenContour -> Corner
                        | _ -> seg.Type
        let angle = if reverse then -PI/2. else PI/2.
        match seg.Type with
        | SpiroPointType.Corner ->
            let th1, th2, bend = norm(lastSeg.tangent2 + angle), norm(seg.tangent1 + angle), norm(seg.tangent1 - lastSeg.tangent2)
            if (not reverse && bend < -PI/8.0) || (reverse && bend > PI/8.0) then
                //two points on sharp outer bend
                [(segAddPolar seg (this.align th1 - angle/2.) (dist * sqrt 2.0), newType);
                 (segAddPolar seg (this.align th2 + angle/2.) (dist * sqrt 2.0), newType)]
            else //single point for right angle or more on outer bend or any inner bend
                let offset = min (min (dist/cos (bend/2.0)) seg.seg_ch) lastSeg.seg_ch
                // if (dist/cos (bend/2.0)) > seg.seg_ch || (dist/cos (bend/2.0)) > lastSeg.seg_ch then
                //     if Set.ofList [seg.Type; lastSeg.Type] = Set.ofList [Corner; G2] then
                //         printfn "corner/curve bend"
                //     printfn "inner bend %f offset %f chords %f %f " bend (dist/cos (bend/2.0)) seg.seg_ch lastSeg.seg_ch
                [(addPolarContrast seg.X seg.Y (th1 + bend/2.0) offset, newType)]
        | SpiroPointType.Right ->
            //weirdly, asserts in Fable's js but not in F# run
            //assert ((abs (lastSeg.seg_th - seg.tangent1)) < 1e-5)
            [(segAddPolar seg (norm (lastSeg.tangent2 + angle)) dist, newType)]
        | SpiroPointType.Left ->
            [(segAddPolar seg (norm (seg.tangent1 + angle)) dist, newType)]
        | SpiroPointType.Anchor when reverse -> []  //reverse both points below
        | SpiroPointType.Handle when reverse ->
            //assert (lastSeg.Type = SpiroPointType.Anchor)
            let oldAnchor = segAddPolar lastSeg (lastSeg.tangent1 + angle) dist
            let oldHandle = segAddPolar seg (seg.tangent1 + angle) dist
            let newHandle = oldAnchor + (oldAnchor - oldHandle)
            [(newHandle, SpiroPointType.Handle); (oldAnchor, SpiroPointType.Anchor)]
        | _ ->
            [(segAddPolar seg (seg.tangent1 + angle) dist, newType)]

    member this.offsetSegments (segments : list<SpiroSegment>) start endP reverse closed dist =
        [for i in start .. endP do
            let seg = segments.[i]
            let angle = if reverse then -PI/2. else PI/2.
            if i = 0 then
                if closed then
                    let lastSeg = segments.[segments.Length-2]
                    yield! this.offsetSegment seg lastSeg reverse dist
                else
                    (segAddPolar seg (seg.tangent1 + angle) dist, seg.Type)
            elif i = segments.Length-1 && not closed then
                let lastSeg = segments.[i-1]
                (segAddPolar seg (lastSeg.tangent2 + angle) dist, seg.Type)
            else
                let lastSeg = segments.[i-1]
                yield! this.offsetSegment seg lastSeg reverse dist
        ]

    member this.getSansOutlines ch e = 
        let fthickness = float thickness
        let serif = float this.axes.serif
        let offsetPointCap X Y theta = addPolarContrast X Y theta (fthickness * sqrt 2.0)
        let offsetMidSegments segments reverse =
            this.offsetSegments segments 1 (segments.Length-2) reverse false fthickness
        let cap X Y theta isJoint ty =
            let thetaAligned = this.align theta
            //make serif on endcap
            if serif > 0.0 && not isJoint then
                let serifDist = SpiroImpl.hypot (serif + fthickness) fthickness
                let serifAng = atan2 fthickness (serif + fthickness)
                [(addPolarContrast X Y (thetaAligned + PI * 0.75) (fthickness * sqrt 2.0), Corner);
                 (addPolarContrast X Y (thetaAligned + PI * 0.5 + serifAng) serifDist, Corner);
                 (addPolarContrast X Y (thetaAligned + PI * 0.5 - serifAng) serifDist, Corner);
                 (addPolarContrast X Y (thetaAligned - PI * 0.5 + serifAng) serifDist, Corner);
                 (addPolarContrast X Y (thetaAligned - PI * 0.5 - serifAng) serifDist, Corner);
                 (addPolarContrast X Y (thetaAligned - PI * 0.75) (fthickness * sqrt 2.0), ty)]
            //make flared endcap
            elif this.axes.flare <> 0.0 && not isJoint then
                let preflareDist, preflareAng = toPolar fthickness -(fthickness*0.80)
                let flareDist, flareAng = toPolar fthickness ((this.axes.flare + 1.0) * fthickness)
                [(addPolarContrast X Y (theta + PI * 0.75) (fthickness * sqrt 2.0), Corner);
                 (addPolarContrast X Y (theta + preflareAng) preflareDist, LineToCurve);
                 (addPolarContrast X Y (thetaAligned + PI * 0.5 - flareAng) flareDist, Corner);
                 (addPolarContrast X Y (thetaAligned - PI * 0.5 + flareAng) flareDist, Corner);
                 (addPolarContrast X Y (theta - preflareAng) preflareDist, CurveToLine);
                 (addPolarContrast X Y (theta - PI * 0.75) (fthickness * sqrt 2.0), ty)]
            elif this.axes.end_bulb <> 0.0 || isJoint then
                [(offsetPointCap X Y (thetaAligned + PI * 0.25), Corner);
                 (addPolarContrast X Y thetaAligned (fthickness * (1.0+this.axes.end_bulb)), G2);
                 (offsetPointCap X Y (thetaAligned - PI * 0.25), ty)]
            else
                [(offsetPointCap X Y (thetaAligned + PI * 0.25), Corner);
                 (offsetPointCap X Y (thetaAligned - PI * 0.25), ty)]
        let startCap (seg : SpiroSegment) =
            let ty = if seg.Type = SpiroPointType.Anchor then SpiroPointType.Anchor else SpiroPointType.Corner
            cap seg.X seg.Y (seg.tangent1 + PI) (this.isJoint ch seg) ty
        let endCap (seg : SpiroSegment) (lastSeg : SpiroSegment) = 
            let ty = if seg.Type = SpiroPointType.Anchor then SpiroPointType.Anchor else SpiroPointType.Corner
            cap seg.X seg.Y (lastSeg.tangent2) (this.isJoint ch seg) ty
        let spiroToOutline spiro =
            match spiro with
            | SpiroOpenCurve(segments) ->
                let points = startCap segments.[0]
                             @ offsetMidSegments segments false
                             @ endCap segments.[segments.Length-1] segments.[segments.Length-2]
                             @ (offsetMidSegments segments true |> List.rev)
                             // reversed segments can't properly calculate last chord theta
                             //  @ (offsetMidSegments (this.reverseSegments segments) false)                             
                [ClosedCurve(points)]
            | SpiroClosedCurve(segments) ->
                [ClosedCurve(this.offsetSegments segments 0 (segments.Length-2) false true fthickness)
                 ClosedCurve(this.offsetSegments segments 0 (segments.Length-2) true true fthickness |> List.rev)]
            | SpiroDot(p) ->
                let x,y = getXY p
                [Font.dotToClosedCurve x y (thickness + 5)]
            | SpiroSpace -> [Space]
        applyToSegments spiroToOutline e

    member this.spiroToLines lines spiro =
        let thicknessby3 = float thickness / 3.0
        match spiro with
        | SpiroOpenCurve(segments) ->
            [for i in 0..lines-1 do
                let offset = (float thickness) * (float i / float (lines-1) - 0.5) * 2.0
                OpenCurve(this.offsetSegments segments 0 (segments.Length-1) false false offset)]
        | SpiroClosedCurve(segments) ->
            [for i in 0..lines-1 do
                let offset = (float thickness) * (float i / float (lines-1) - 0.5) * 2.0
                ClosedCurve(this.offsetSegments segments 0 (segments.Length-2) false true offset)]
        | SpiroDot(p) ->
            let x,y = getXY p
            [Font.dotToClosedCurve x y thickness; Font.dotToClosedCurve x y (thickness/2)]
        | SpiroSpace -> [Space]

    member this.getStroked = 
        applyToSegments (this.spiroToLines 4) >>
            let dummyChar = ' '
            Font({Axes.DefaultAxes with thickness = 2}).getSansOutlines dummyChar

    member this.getScratches e = 
        let spiroToScratchOutlines spiro =
            let thicknessby3 = float thickness/3.0
            let offsetPointCap X Y theta = addPolarContrast X Y theta (thicknessby3 * sqrt 2.0)
            let offsetMidSegments segments reverse =
                this.offsetSegments segments 1 (segments.Length-2) reverse false thicknessby3
            let startCap (seg : SpiroSegment) =
                [(segAddPolar seg (seg.tangent1 - PI * 0.90) (thicknessby3*3.0), Corner);
                //[(offsetPointCap seg.X seg.Y (seg.tangent1 - PI * 0.75), Corner);
                 //(offsetPointCap seg.X seg.Y (seg.tangent1 + PI), Corner);
                 (offsetPointCap seg.X seg.Y (seg.tangent1 + PI * 0.75), Corner)]
            let endCap (seg : SpiroSegment) (lastSeg : SpiroSegment) = 
                [(offsetPointCap seg.X seg.Y lastSeg.tangent2, Corner)]
            match spiro with
            | SpiroOpenCurve(segments) ->
                let points = startCap segments.[0]
                             @ offsetMidSegments segments false
                             @ endCap segments.[segments.Length-1] segments.[segments.Length-2]
                             @ (offsetMidSegments segments true |> List.rev)
                [ClosedCurve(points)]
            | SpiroClosedCurve(segments) ->
                let fthickness = thicknessby3/3.
                [ClosedCurve(this.offsetSegments segments 0 (segments.Length-2) false true fthickness)
                 ClosedCurve(this.offsetSegments segments 0 (segments.Length-2) true true fthickness |> List.rev)]
            | SpiroDot(p) -> [Dot(p)]
            | SpiroSpace -> [Space]
        applyToSegments (this.spiroToLines 3) e |> applyToSegments spiroToScratchOutlines

    member this.italiciseP p =
        let x,y = getXY p
        YX(y, x + int(this.axes.italic * float y))

    /// Italicising the outlines leads to strange curves.  Attempt to subdivide the original curves so the
    /// italicised versions are closer to a shear. Note: there is a performance cost for more detailed curves.
    member this.subdivide e =
        let splitSegments (seg1 : SpiroSegment) (seg2 : SpiroSegment) =
            if (seg1.Type=Corner || seg1.Type=CurveToLine) && (seg2.Type=Corner || seg2.Type=LineToCurve) then
                [(YX(int seg1.Y, int seg1.X), seg1.Type)]
            elif seg1.Type = Handle then
                []
            else        
                let scale, rot = SpiroImpl.get_scale_rot (seg2.X - seg1.X) (seg2.Y - seg1.Y) seg1.ks
                let ksub = Array.create 4 0.0
                let xmid, ymid = SpiroImpl.get_mid seg1.X seg1.Y seg2.X seg2.Y scale rot seg1.ks ksub
                let midType = 
                    match seg1.Type, seg2.Type with
                    | (SpiroPointType.OpenContour, t) -> t
                    | (SpiroPointType.EndOpenContour, _) -> G2
                    | (_, SpiroPointType.G2) | (_, SpiroPointType.Left) -> G2
                    | (SpiroPointType.G2, _) | (SpiroPointType.Right, _) -> G2
                    | (SpiroPointType.Anchor, _) -> G2
                    | (_, _) -> seg1.Type
                [(YX(int seg1.Y, int seg1.X), seg1.Type); (YX(int ymid, int xmid), midType)]
        let splitSegment spiros =
            match spiros with
            | SpiroOpenCurve(segments) ->
                OpenCurve(
                    [for i in 0..segments.Length-2 do
                        yield! splitSegments segments.[i] segments.[i+1]
                    ] @
                    [let seg = segments.[segments.Length-1] in YX(int seg.Y, int seg.X), seg.Type])
            | SpiroClosedCurve(segments) ->
                //note Spiro has added first point at end for closed curves
                ClosedCurve(
                    [for i in 0..segments.Length-2 do
                        yield! splitSegments segments.[i] segments.[i+1]
                    ])
            | SpiroDot(p) -> Dot(p)
            | SpiroSpace -> Space
        EList(elementToSpiroSegments e |> List.map splitSegment)

    member this.italicise e = 
        if this.axes.italic>0.0 && not this.axes.spline_not_spiro then
            e |> this.subdivide |> movePoints this.italiciseP
        else
            e        

    member this.getOutline ch =
        if this.axes.stroked then
            this.getStroked
        elif this.axes.scratches then
            this.getScratches
        elif this.axes.outline then
            this.getSansOutlines ch
        else 
            id
        >> this.italicise

    ///Ensure tangents fall within bounds of glpyh
    member this.constrainTangents elem = 
        let l,r,t,b = bounds elem
        let tangent (point : Point) type_ angle =
            let x, y = getXY point
            let angle = norm angle
            if type_ <> G2 then
                None
            elif x = l && y = t then
                if angle < -PI/4. then Some (-PI/2.) else Some 0.
            elif x = l && y = b then
                if angle < PI/4. then Some 0. else Some (PI/2.)
            elif x = r && y = t then
                if angle < -PI*3./4. then Some -PI else Some (-PI/2.)
            elif x = r && y = b then
                if angle < PI*3./4. then Some (PI/2.) else Some PI
            elif x = l || x = r then
                if angle < 0. then Some (-PI/2.) else Some (PI/2.)
            elif y = t then
                if angle < -PI/2. then Some -PI else Some 0.
            elif y = b then
                if angle < PI/2. then Some 0. else Some PI
            else
                None
        let constrainStart (segment : SpiroSegment) =
            let tangent = tangent segment.Point segment.Type segment.tangent1
            (segment.Point, segment.Type, tangent)
        let spiroConstrain spiro =
            match spiro with
            | SpiroOpenCurve(segments) ->
                let angle = PI - segments.[segments.Length-2].tangent2 
                let lastSeg = segments.[segments.Length-1] in            
                [TangentCurve(List.map constrainStart (segments.[0..segments.Length-2])
                              @ [(lastSeg.Point, lastSeg.Type, (tangent lastSeg.Point lastSeg.Type angle))], false)]
            | SpiroClosedCurve(segments) ->
                [TangentCurve(List.map constrainStart segments, true)]
            | SpiroDot(p) -> [Dot(p)]
            | SpiroSpace -> [Space]
        applyToSegments spiroConstrain elem

    ///Convert spiro curves to bezier SVG curves
    member this.spiroToSvg spiro = 
        match spiro with
        | SpiroOpenCurve(segs) ->
            let bc = PathBezierContext()
            Spiro.SpirosToBezier (Array.ofList segs) false bc |> ignore
            [bc.ToString]
        | SpiroClosedCurve(segs) ->
            let bc = PathBezierContext()
            Spiro.SpirosToBezier (Array.ofList segs) true bc |> ignore
            [bc.ToString]
        | SpiroDot(p) ->
            let x, y = getXY p
            svgCircle x y thickness
        | SpiroSpace -> []

    ///Convert element to bezier SVG curves
    member this.elementToSvg elem = 
        if axes.spline_not_spiro then
            elementToSplineSvg elem
        else
            elementToSpiroSegments elem |> List.collect this.spiroToSvg

    member this.elementToSvgPath element offsetX offsetY strokeWidth fillColour=
        let fillrule = "nonzero"
        let fillStyle = if this.axes.outline && this.axes.filled then fillColour else "none"
        let guid = Guid.NewGuid()
        [
            sprintf "<clipPath id='%A'>" guid
            let margin = thickness * 2
            sprintf "<rect x='%d' y='%d' width='%d' height='%d'/>" 
                -margin (_glyphDefs._D-margin) (this.width element + margin) (this.charHeight + margin)
            "</clipPath>"
            "<path "
            "d='"
        ] @
        this.elementToSvg element @
        [
            "'"
            sprintf "transform='translate(%d,%d) scale(1,-1)'" offsetX offsetY
            sprintf "style='fill:%s;fill-rule:%s;stroke:%s;stroke-width:%d'" fillStyle fillrule fillColour strokeWidth
            sprintf "clip-path='url(#%A)'" guid
            "/>"
        ]

    ///circles highlighting the knots (defined points on the spiro curves)
    member this.getSvgKnots offsetX offsetY outline elem =
        let x1,x2,y1,y2 = bounds elem
        let rec toSvgPoints elem =
            let svgKnot (p, t) = 
                let x,y = getXY p
                let r = if t=Handle then 1 elif outline then 10 else 20
                if x = x1 then
                    svgSemiCircle x y r 'r'
                elif x = x2 then
                    svgSemiCircle x y r 'l'
                elif y = y1 then
                    svgSemiCircle x y r 'u'
                elif y = y2 then
                    svgSemiCircle x y r 'd'
                else
                    svgCircle x y r
            match elem with
            | OpenCurve(pts) | ClosedCurve(pts) -> pts |> List.collect svgKnot
            | Dot(p) -> svgKnot (p, G2)
            | EList(elems) -> List.collect toSvgPoints elems
            | Space -> []
            | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem) 
        // small red circles
        [
            "<!-- knots -->"
            "<path d='"
        ] @
        toSvgPoints elem @
        [
            "'"
            sprintf "transform='translate(%d,%d) scale(1,-1)'" offsetX offsetY
            sprintf "style='fill:none;stroke:%s;stroke-width:10'/>" (if outline then "#aaaaff" else "#ffaaaa")
        ]

    member this.translateBy dx dy = 
        let shift p = let x,y = getXY p in YX(y + dy, x + dx)
        movePoints shift

    member this.translateByThickness = this.translateBy thickness thickness

    member this.charToElem ch =
        Glyph(ch) 
            |> this.reduce 
            |> applyIf axes.constraints this.constrainTangents
            |> this.monospace 
            |> this.translateByThickness

    member this.charToSvg ch offsetX offsetY =
        // printfn "%c" ch
        let spine = this.charToElem ch
        [sprintf "<!-- %c -->" ch]
        @ 
            try
                let outline = spine |> this.getOutline ch
                this.elementToSvgPath outline offsetX offsetY 5 black
                @ if this.axes.show_knots && this.axes.outline then
                    outline |> this.getSvgKnots offsetX offsetY true
                  else []
            with
            | _ ->
                try
                    this.elementToSvgPath spine offsetX offsetY 5 red
                with
                | _ -> this.elementToSvgPath (Dot(HC)) offsetX offsetY 5 red
        @ if this.axes.show_knots then
            (spine |> this.italicise |> this.getSvgKnots offsetX offsetY false)
          else
            []

    member this.width e =
        (e |> this.reduce |> this.monospace |> this.elemWidth) 
        + this.axes.tracking
        + int ((1.0 + this.axes.contrast) * float thickness * 2. + float this.axes.serif)

    member this.charWidth ch =
        this.width (Glyph(ch))

    member this.charWidths str = Seq.map this.charWidth str |> List.ofSeq

    member this.stringWidth str = List.sum (this.charWidths str)

    member this.stringToSvgLineInternal (lines : string list) offsetX offsetY =
        let svg, lineWidths = 
            List.unzip
                [for i in 0..lines.Length-1 do
                    let str = lines.[i]
                    let widths = this.charWidths str
                    let offsetXs = List.scan (+) offsetX widths
                    let lineOffset = offsetY + this.charHeight * (i+1) - this.yBaselineOffset + thickness
                    let svg = [for c in 0 .. str.Length - 1 do
                                yield! this.charToSvg str.[c] (offsetXs.[c]) lineOffset]
                    (svg, List.sum widths)
                ]
        (List.collect id svg, lineWidths)

    member this.stringToSvgLines (lines : string list) offsetX offsetY =
        fst (this.stringToSvgLineInternal lines offsetX offsetY)

    member this.stringToSvg (lines : string list) offsetX offsetY autoscale =
        let margin = 50
        let svg, lineWidths = this.stringToSvgLineInternal lines offsetX offsetY
        let w, h =
            if autoscale then 
                (List.max lineWidths + margin),
                (this.charHeight * lineWidths.Length + margin)
            else
                6000, 6000
        toSvgDocument 
            -margin
            -margin
            w
            h
            svg

    member this.ElementToSpiroSegments = elementToSpiroSegments
    member this.CharToOutline ch = this.charToElem ch |> this.getOutline ch
    member this.GlyphDefs = _glyphDefs
