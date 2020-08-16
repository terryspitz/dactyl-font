// Functional Fonts by terryspitz
// Mar 2020

//TODOs:
// move outline point inward only
// improve serifs
// join lines properly
// correct tight bend in 5
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
// Italic with spline (subdivide)
// Dactyl-smooth, no corners

//Features :
// Backscratch font (made of 4 parallel lines)
// Generated FontForge fonts
// Variable font explorer: https://terryspitz.github.io/dactyl-font/
// Mono (fixed-width) font
// Horiz/vertical endcaps using axis_align_caps
// Randomise in explorer
// Contrast (horiz vs vert stroke width ratio)
// Flared endcaps

module Generator


open System

open SpiroPointType
open SpiroSegment
open SpiroControlPoint
open PathBezierContext
open Axes
open Curves


type Point =
    // Raw coordinates
    | YX of y: int * x: int

    // Y coordinate: Top,X-height,Half-height,Bottom
    // X coordinate: Left,Centre,Right
    // o adds/subtracts an offset to the dimension it follows
    | TL | TLo | TC | TRo | TR        // Top points: Left, Left offset inward, Centre, Right
    | ToL | ToC | ToR           // Top offset down
    | XL | XLo | XC | XRo | XR  // x-height
    | XoL | XoC | XoR           // x-height offset down
    | ML | MC | MR              // Midway down from x-height
    | HL | HLo | HC | HRo | HR  // half glyph height
    | BoL | BoC | BoR           // Bottom offset up
    | BL | BLo | BC | BRo | BR  // Bottom
    | DoL                       // Descender offset up
    | DL | DC | DR              // Descender
    | BN | BoN | HN | XoN | XN | TN         // Narrow width points
    | Mid of p1 : Point * p2 : Point
    | Interp of p1 : Point * p2 : Point * frac : float
    
    member this.GetXY = match this with | YX(y,x) -> x,y | _ -> invalidArg "this" "Point (+) only works with reduced points"
    static member (+) (lhs : Point, rhs : Point) =
        let x1,y1 = lhs.GetXY
        let x2,y2 = rhs.GetXY
        YX(y1+y2, x1+x2)
    static member (-) (lhs : Point, rhs : Point) =
        let x1,y1 = lhs.GetXY
        let x2,y2 = rhs.GetXY
        YX(y1-y2, x1-x2)

type SCP = SpiroControlPoint

type Element = 
    | Glyph of c : char
    | Line of p1: Point * p2: Point
    | PolyLine of list<Point>
    | OpenCurve of list<Point * SpiroPointType>
    | ClosedCurve of list<Point * SpiroPointType>
    | Dot of Point
    | EList of list<Element>
    | Space

type SpiroElement =
    | SpiroOpenCurve of segments: list<SpiroSegment>
    | SpiroClosedCurve of segments: list<SpiroSegment>
    | SpiroDot of Point
    | SpiroSpace

// Bring into local namespace, and rename if nec.
let CurveToLine = SpiroPointType.Left
let LineToCurve = SpiroPointType.Right
let G2 = SpiroPointType.G2
let G4 = SpiroPointType.G4
let Start = SpiroPointType.OpenContour
let Corner = SpiroPointType.Corner
let End = SpiroPointType.EndOpenContour
let EndClosed = SpiroPointType.End
let Anchor = SpiroPointType.Anchor
let Handle = SpiroPointType.Handle


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
        "<svg xmlns='http://www.w3.org/2000/svg'";
        sprintf "viewBox='%d %d %d %d'>" left bottom width height;
        "<g id='layer1'>";
    ] @ svg @ [
        "</g>";
        "</svg>";
    ]

///normalise angle to between PI/2 and -PI/2
let norm th = if th>PI then th-PI*2.0 else if th<(-PI) then th+PI*2.0 else th


//class
type Font (axes: Axes) =

    // X axis guides, from left
    let L = 0               // Left
    let R = axes.width      // Right = standard glyph width
    let N = R * 4/5         // Narrow glyph width
    let C = R / 2           // Centre
    let monospaceWidth = N

    // Y axis guides, from bottom-up
    let B = 0               // Bottom
    let X = axes.x_height   // x-height
    let M = X/2             // Midway down from x-height
    let T = axes.height     // Top = standard glyph caps height
    let H = T/2             // Half total height
    let D = -axes.height/2  // descender height
    let offset = axes.roundedness // offset from corners
    let dotHeight = max ((X+T)/2) (X+axes.thickness*3)

    let minOffset = 50
    //let flooredOffset = if offset > minOffset then offset else minOffset
    // let flooredOffsetHalf = if offset/2 > minOffset then offset/2 else minOffset
    let flooredOffset = offset + minOffset
    let flooredOffsetHalf = offset/2 + minOffset
    let thickness = if axes.stroked || axes.scratches then max axes.thickness 30 else axes.thickness

    //basic manipulation using class variables

    let toPolar dy dx = SpiroImpl.hypot dx dy, atan2 dy dx

    let addPolarContrast X Y theta dist =
        YX(int(Y + dist * sin(theta)), int(X + dist * cos(theta) + axes.contrast * float thickness * cos(theta)))
    
    let segAddPolar (seg : SpiroSegment) theta dist = 
            addPolarContrast seg.X seg.Y theta dist

    let rec reducePoint p = 
        match p with
        | YX(y,x) -> (y,x)
        | TL -> (T,L) | TLo -> (T,L+offset) | TC -> (T,C) | TRo -> (T,R-offset) | TR -> (T,R)
        | ToL -> (T-offset,L) | ToC -> (T-offset,C) | ToR -> (T-offset,R)
        | XL -> (X,L) | XLo -> (X,L+offset) | XC -> (X,C) | XRo -> (X,R-offset) | XR -> (X,R)
        | XoL -> (X-offset,L) | XoC -> (X-offset,C) | XoR -> (X-offset,R)
        | ML -> (M,L) | MC -> (M,C) | MR -> (M,R)
        | HL -> (H,L) | HLo -> (H,L+offset) | HC -> (H,C) | HRo -> (H,R-offset) | HR -> (H,R)
        | BoL -> (B+offset,L) | BoC -> (B+offset,C) | BoR -> (B+offset,R)
        | BL -> (B,L) | BLo -> (B,L+offset) | BC -> (B,C) | BRo -> (B,R-offset) | BR -> (B,R)
        | DoL -> (D+offset,L)
        | DL -> (D,L) | DC -> (D,C) | DR -> (D,R)
        | BN -> (B,N) | BoN -> (B+offset,N) | HN -> (H,N) | XoN -> (X-offset,N) | XN -> (X,N) | TN -> (T,N)
        | Mid(p1, p2) -> reducePoint (Interp(p1, p2, 0.5))
        | Interp(p1, p2, f) -> let y1, x1 = reducePoint p1
                               let y2, x2 = reducePoint p2
                               (y1+int(float(y2-y1)*f), x1+int(float(x2-x1)*f))
    
    let getXY p =
        let y, x = reducePoint p
        (x, y)

    let rec elementToSpiros elem =
        let makeSCP (p, t) = let x,y = getXY p in { SCP.X=float(x); Y=float(y); Type=t}
        match elem with
        | OpenCurve(pts) ->
            match Spiro.SpiroCPsToSegments (List.map makeSCP pts |> Array.ofList) false with
            | Some segs -> [SpiroOpenCurve(Array.toList segs)]
            | None -> [SpiroSpace]
        | ClosedCurve(pts) ->
            match Spiro.SpiroCPsToSegments (List.map makeSCP pts |> Array.ofList) true with
            | Some segs -> [SpiroClosedCurve(Array.toList segs)]
            | None -> [SpiroSpace]
        | Dot(p) -> [SpiroDot(p)]
        | EList(elems) -> List.collect elementToSpiros elems
        | Space -> [SpiroSpace]
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem) 

    let toSplineControlPoint (p : Point, t : SpiroPointType) =
        let x,y = getXY p
        let ty = match t with 
                    | SpiroPointType.Corner 
                    | SpiroPointType.OpenContour | SpiroPointType.EndOpenContour | SpiroPointType.End
                    | SpiroPointType.Anchor | SpiroPointType.Handle
                        -> if axes.smooth then SplinePointType.Smooth else SplinePointType.Corner
                    | SpiroPointType.Left 
                        -> SplinePointType.CurveToLine
                    | SpiroPointType.Right 
                        -> SplinePointType.LineToCurve
                    | SpiroPointType.G2 | SpiroPointType.G4
                        -> SplinePointType.Smooth
                    | _ -> invalidArg "ty" (sprintf "Unexpected SpiroPointType %A" t) 

        // let pt = new ControlPoint(new Vec2(knot.x, knot.y), knot.ty, knot.lth, knot.rth);
        SplineControlPoint({x=float x;y=float y}, ty)
    
    let rec elementToSpline elem =
        let toSegs pts isClosed =
            let ctrlPts = List.map toSplineControlPoint pts |> Array.ofList
            let spline = Spline(ctrlPts, isClosed)
            spline.solve(axes.max_spline_iter)
            // printfn "spline"
            // for pt in spline.ctrlPts do
            //     printfn "%f %f %f %f" pt.pt.x pt.pt.y pt.lTh pt.rTh
            assert (spline.ctrlPts.Length = pts.Length)
            [for i in 0..pts.Length-1 do
                let ty = snd pts.[i]
                let pt = spline.ctrlPts.[i]
                let pt1 = if i<pts.Length-1 then spline.ctrlPts.[i+1] else spline.ctrlPts.[0]
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
        match elem with
        | OpenCurve(pts) ->
            [SpiroOpenCurve(toSegs pts false)]
        | ClosedCurve(pts) ->
            let segs = toSegs pts true
            [SpiroClosedCurve(segs @ [segs.[0]])]
        | Dot(p) -> [SpiroDot(p)]
        | EList(elems) -> List.collect elementToSpline elems
        | Space -> [SpiroSpace]
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem) 

    let elementToSegments elem =
        let e = 
            if axes.spline_not_spiro then
                elementToSpline elem
            else
                elementToSpiros elem
        let debug = false
        if debug then
            for seg in e do
                printfn "segments"
                match seg with
                | SpiroOpenCurve(segs) ->
                    for seg in segs do
                        printfn "%f,%f %f %f" seg.X seg.Y seg.tangent1 seg.tangent2
                | SpiroClosedCurve(segs) ->
                    for seg in segs do
                        printfn "%f,%f %f %f" seg.X seg.Y seg.tangent1 seg.tangent2
                | _ -> ()
        e

    let rec elementToSplineSvg elem =
        let toSvg pts isClosed =
            let ctrlPts = List.map toSplineControlPoint pts |> Array.ofList
            let spline = Spline(ctrlPts, isClosed)
            spline.solve(axes.max_spline_iter)
            [spline.render().renderSvg()]
        match elem with
        | OpenCurve(pts) -> toSvg pts false
        | ClosedCurve(pts) -> toSvg pts true
        | Dot(p) -> let x, y = getXY p in svgCircle x y thickness
        | EList(elems) -> List.collect elementToSplineSvg elems
        | Space -> []
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem) 

    //apply a transformation fn: Spiros -> Elements
    let applyToSegments fn elem = 
        let elems = elementToSegments elem |> List.collect fn
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

    member this.axes = {axes with thickness = thickness;}

    static member dotToClosedCurve x y r =
        ClosedCurve([(YX(y-r,x), G2); (YX(y,x+r), G2);
                     (YX(y+r,x), G2); (YX(y,x-r), G2)])

    //Straights: AEFHIKLMNTVWXYZklvwxyz147/=[]\`|*"'
    //Dots: ij:;!?
    //Curves: COScos36890()~
    //LeftUpright: BDPRb mnpr 
    //RightUpright: GJadgq
    //Other: QUefhtu25@#$€£_&-+{}%

    member this.getGlyph e =
        let adgqLoop = OpenCurve([(XoR, Corner); (XC, G2); (ML, G2); (BC, G2); (BoR, Corner)])

        match e with
        | Glyph('!') -> EList([Line(TL, ML); Dot(BL)])
        | Glyph('"') -> EList([Line(TL, YX(T-flooredOffset,L)); Line(TC, YX(T-flooredOffset,C))])
        | Glyph('#') -> let Y3 = T/3
                        EList([Line(YX(Y3*2,L), YX(Y3*2,R)); Line(YX(Y3,L), YX(Y3,R));
                               Line(YX(T,R-R/4), YX(B,R-R/4)); Line(YX(T,R/4), YX(B,R/4))])
        | Glyph('£') -> EList([OpenCurve([(ToR, Start); (YX(T,R-flooredOffset), G2); (YX(H,R/4), CurveToLine); (YX(B,R/4), End)]);
                               Line(BL,BR); Line(HL,HC)])
        | Glyph('$') -> EList([Glyph('S'); Line(YX(T+thickness,C),YX(B-thickness,C))])
        | Glyph('%') -> EList([Line(TR,BL);
                               ClosedCurve([(YX(T-T/5,L), G2); (YX(T,R/5), G2); (YX(T-T/5,R*2/5), G2)]);
                               ClosedCurve([(YX(T/5,R), G2); (YX(B,R-R/5), G2); (YX(T/5,R*3/5), G2)])])
        | Glyph('&') -> OpenCurve([(BR, Start); (YX(T/4,C), LineToCurve); (YX(T-flooredOffset,L), G2); (TC, G2);
                                   (YX(T-flooredOffset,R-offset), G2); (YX(H,(R-offset)/2), G2);
                                   (YX(B+flooredOffset,L), G2); (BLo, G2); (BoR, End)])
        | Glyph(''') -> Line(TL, YX(T-flooredOffset,L))
        | Glyph('(') -> OpenCurve([(YX(T+thickness, flooredOffset), Start); (HL, G2); (YX(B-thickness,flooredOffset), End)])
        | Glyph(')') -> this.reflect (Glyph('('))
        | Glyph('*') -> let sin30 = int (0.866 * float T / 4.0)
                        EList([Line(YX(T*2/3,L), YX(T*2/3,N)); 
                               Line(YX(T*2/3+sin30,R/5), YX(T*2/3-sin30,N*4/5)); Line(YX(T*2/3+sin30,N*4/5), YX(T*2/3-sin30,R/5))])
        | Glyph('+') -> EList([Line(HL, HR); Line(YX(T-T/4,C), YX(T/4,C))])
        | Glyph('-') -> Line(HL, HN)
        | Glyph('.') -> Dot(BC)
        | Glyph(',') -> Line(BC, YX(B-min thickness 50,C-min thickness 50))
        | Glyph('/') -> this.reflect (Glyph('\\'))
        | Glyph(':') -> EList([Dot(BC); Dot(YX(T/3,C))])
        | Glyph(';') -> EList([Glyph(','); Dot(YX(T/3,C))])
        | Glyph('<') -> PolyLine([XR; YX(X/2,L); BR])
        | Glyph('=') -> EList([Line(YX(X*2/3,L), YX(X*2/3,N)); Line(YX(X/3,L), YX(X/3,N))])
        | Glyph('>') -> this.reflect (Glyph('<'))
        | Glyph('?') -> EList([OpenCurve([(YX(T-flooredOffset,L), G2); (TC, G2); (Mid(TR,HR), G2);
                                         (HC, Corner); (YX(T/4,C), End);])
                               Dot(BC)])
        | Glyph('@') -> OpenCurve([(YX(T/3,R*3/4), Corner); (YX(T/4,C), G2); (YX(H,R/4), G2); (YX(T*2/3,C), G2); (YX(H,R*3/4), CurveToLine)
                                   (YX(T/3,R*3/4), LineToCurve); (YX(T/4,R), CurveToLine); (YX(T/2,R), LineToCurve)
                                   (TC,G2); (HL,G2); (BLo,G2); (BR,End)])
        | Glyph('[') -> PolyLine([YX(T+thickness,C); YX(T+thickness,L); YX(B-thickness,L);YX(B-thickness,C);])
        | Glyph('\\') -> Line(YX(T+thickness,L), YX(B-thickness,R))
        | Glyph(']') -> this.reflect (Glyph('['))
        | Glyph('^') -> PolyLine([YX(T*2/3,L); TC; YX(T*2/3,R)])
        | Glyph('_') -> Line(YX(B-thickness*2,L), YX(B-thickness*2,N))
        | Glyph('`') -> Line(TC, YX(T-min thickness 50,C+min thickness 50))
        | Glyph('{') -> EList([OpenCurve([(YX(T+thickness, N), Start); (YX(T+thickness, N-10), LineToCurve);
                                   (YX(H+flooredOffset, L+flooredOffset/2), G2); (HL, G2); ]);
                               OpenCurve([(YX(B-thickness, N), Start); (YX(B-thickness, N-10), LineToCurve);
                                   (YX(H-flooredOffset, L+flooredOffset/2), G2); (HL, G2); ])])
        | Glyph('|') -> Line(TC,BC)
        | Glyph('}') -> this.reflect (Glyph('{'))
        | Glyph('~') -> let h = flooredOffset/2
                        OpenCurve([(YX(T-h,L), Start); (YX(T,R/4), G2); (YX(T-h,R/2), G2); (YX(T-h*2,R*3/4), G2); (YX(T-h,R), G2); ])

        | Glyph('0') -> EList([ClosedCurve([(HL, G2); (BC, G2); (HR, G2); (TC, G2)]); Line(TR,BL)])
        | Glyph('1') -> let midX = max (thickness * 2) (int ((float monospaceWidth * this.axes.monospace) / 2.0))
                        EList([PolyLine([YX(T*4/5,L); YX(T,midX); YX(B, midX)])] @
                              if this.axes.monospace > 0.0 then [Line(BL, YX(B,midX*2))] else [])
        | Glyph('2') -> OpenCurve([(YX(T-offset,L), Start); (YX(T,L+flooredOffset), G2); (YX(T-flooredOffset,R), G2)
                                   (YX((T-offset)/3,C), CurveToLine); 
                                   (BL, Corner); (BR, End)])
        | Glyph('3') -> EList([OpenCurve([(YX(T-offset,L), Start); (YX(T,L+min flooredOffset R), G2);
                                          (Mid(TR, HR), G2); (YX(H,C+1), CurveToLine); (HC, End)]);
                               OpenCurve([(HC, Start); (YX(H,C+1), LineToCurve); (Mid(HR, BR), G2)
                                          (YX(B,L+flooredOffset), G2); (YX(B+min offset R,L), End)])])
        | Glyph('4') -> PolyLine([BN; TN; YX(T/4,L); YX(T/4,R)])
        // Wow i've been struggling with the tight bend where the vertical meets the curve. 
        // My outline logic seems to needs work, uniquely affecting this glyph. 
        // | Glyph('5') -> OpenCurve([(TR, Start); (TL, Corner); (YX(T*2/3-offset,L), Corner); (YX(T*2/3,C), G2); (YX(T/3,R), G2); (YX(B,L+flooredOffset), G2); (BoL, End)])
        | Glyph('5') -> EList([OpenCurve([(TR, Start); (TL, Corner); (YX(T*2/3-offset,L), Corner)])
                               OpenCurve([(YX(T*2/3-offset,L), Corner); (YX(T*2/3,C), G2); (YX(T/3,R), G2); (YX(B,L+flooredOffset), G2); (BoL, End)])])
        | Glyph('6') -> OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (YX(T/3,R), G2); (YX(T*2/3,C), G2); (HL, End)])
        | Glyph('7') -> PolyLine([TL; TR; BC])
        | Glyph('8') -> ClosedCurve([(TC, G2); (Mid(TL,HL), G2); 
                                   (HC, Anchor); (YX(T*4/10,C+flooredOffsetHalf), Handle); 
                                   (Mid(HR,BR), G2); (BC, G2); (Mid(HL,BL), G2);
                                   (HC, Anchor); (YX(T*6/10,C+flooredOffsetHalf), Handle); 
                                   (Mid(TR,HR), G2)])
        | Glyph('9') -> OpenCurve([(BC, Start); (HR, G2); (Mid(TR,HR), G2);(TC, G2);
                                   (Mid(TL,HL), G2); (HC, G2); (YX(H+min offset (H/2-thickness),R), End)])

        | Glyph('A') -> let f = float(H/2)/float(T)
                        let p1,p2 = Interp(BL,TC,f), Interp(BR,TC,f)
                        EList([PolyLine([BL; p1; TC; p2; BR]); Line(p1, p2)])
        | Glyph('a') -> EList([Line(XR, BR); adgqLoop])
        | Glyph('B') -> EList([Glyph('P'); OpenCurve([(HL, Corner); (HC, LineToCurve); (Mid(HR, BR), G2); (BC, CurveToLine); (BL, End)])])
        | Glyph('b') -> EList([Line(TL, BL); OpenCurve([(XoL, Start); (XC, G2); (MR, G2); (BC, G2); (BoL, End)])])
        | Glyph('C') -> OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)])
        | Glyph('c') -> OpenCurve([(YX(X - max 0 (offset-thickness),R), Start); (XC, G2); (ML, G2); (BC, G2)
                                   (YX(B + max 0 (offset-thickness),R), End)])
        | Glyph('D') -> let cornerOffset = min flooredOffset (R-minOffset)
                        ClosedCurve([(BL, Corner); (TL, Corner); (YX(T,R-cornerOffset), LineToCurve);
                                     (YX(T-cornerOffset,R), CurveToLine); (YX(B+cornerOffset,R), LineToCurve);
                                     (YX(B,R-cornerOffset), CurveToLine)])
        | Glyph('d') -> EList([Line(BR, TR); adgqLoop])
        | Glyph('E') -> EList([PolyLine([TR; TL; BL; BR]); Line(HL, HR)])
        | Glyph('e') -> OpenCurve([(YX(M,L+thickness), Start); (MR, Corner); (YX(M+flooredOffsetHalf,R), G2);
                                   (XC, G2); (ML, G2); (BC, G2); (YX(B + max 0 (offset-thickness),R), End)])
        | Glyph('F') -> EList([PolyLine([TR; TL; BL]); Line(HL, HRo)])
        | Glyph('f') -> EList([OpenCurve([(TC, Start); (XL, CurveToLine); (BL, End)]); Line(XL, XC)])
        | Glyph('G') -> OpenCurve([(ToR, G2); (TC, G2); (HL, G2); (BC, G2); (YX(H-flooredOffset,R), CurveToLine); (HR, Corner); (HC, End)])
        | Glyph('g') -> EList([OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (DoL, End)]);
                               adgqLoop;])
        | Glyph('H') -> EList([Line(BL, TL); Line(HL, HR); Line(BR, TR)])
        | Glyph('h') -> EList([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, CurveToLine); (BR, End)])])
        | Glyph('I') -> let midX = int (float this.axes.width * this.axes.monospace / 2.0)
                        let vertical = Line(YX(T,midX), YX(B,midX))
                        if this.axes.monospace > 0.0 then
                            EList([vertical; Line(BL, YX(B,midX*2)); Line(TL, YX(T,midX*2))])
                        else 
                            vertical
        | Glyph('i') -> let midX = int (float this.axes.width * this.axes.monospace / 2.0)
                        EList([Line(YX(X,midX), YX(B,midX))
                               Dot(YX(dotHeight,midX))] @
                               if this.axes.monospace > 0.0 then [Line(BL, YX(B,midX*2)); Line(XL, YX(X,midX))] else [])
        | Glyph('J') -> OpenCurve([(TL, Corner); (TR, Corner); (HR, LineToCurve); (BC, G2); (BoL, End)])
        | Glyph('j') -> EList([OpenCurve([(XN, Start); (BN, LineToCurve); (DC, G2); (DoL, End)])
                               Dot(YX(dotHeight,N))])
        | Glyph('K') -> EList([PolyLine([TR; HL; BL]); PolyLine([TL; HL; BR])])
        | Glyph('k') -> EList([Line(TL, BL); PolyLine([YX(X,N); ML; YX(B,N)])])
        | Glyph('L') -> PolyLine([TL; BL; BR])
        | Glyph('l') -> OpenCurve([(TL, Corner); (ML, LineToCurve); (BC, G2)])
        | Glyph('M') -> PolyLine([BL; TL; YX(B,R*3/4); YX(T,R*3/2); YX(B,R*3/2)])
        | Glyph('m') -> EList([Glyph('n');
                              OpenCurve([(BN, Start); (YX(X-flooredOffset,N), LineToCurve); (YX(X,N+C), G2)
                                         (YX(M,N+N), CurveToLine); (YX(B,N+N), End)])])
        | Glyph('N') -> PolyLine([BL; TL; BR; TR])
        | Glyph('n') -> EList([Line(XL,BL)
                               OpenCurve([(BL, Start); (XoL, Corner); (XC, G2); (YX(M,N), CurveToLine); (BN, End)])])
        | Glyph('O') -> ClosedCurve([(HL, G4); (BC, G2); (HR, G4); (TC, G2)])
        | Glyph('o') -> ClosedCurve([(XC, G2); (ML, G2); (BC, G2); (MR, G2)])
        | Glyph('P') -> OpenCurve([(BL, Corner); (TL, Corner); (TC, LineToCurve); (Mid(TR, HR), G2); (HC, CurveToLine); (HL, End)])
        | Glyph('p') -> EList([Line(XL, DL)
                               OpenCurve([(XoL, Start); (XC, G2); (MR, G2); (BC, G2); (BoL, End)])])
        | Glyph('Q') -> EList([Line(Mid(HC, BR), BR); Glyph('O'); ])
        | Glyph('q') -> EList([Line(XR, DR); adgqLoop])
        | Glyph('R') -> EList([Glyph('P'); PolyLine([HL; HC; BR])])
        | Glyph('r') -> EList([Line(BL,XL)
                               //OpenCurve([(BL, Start); (XoL, LineToCurve); (XC, G2); (XoN, End)])])
                               OpenCurve([(YX(X-minOffset,L), Corner); (XC, G2); (XoN, End)])])
        | Glyph('S') -> OpenCurve([(ToR, G2); (TC, G2); (Mid(TL,HL), G2); 
                                   (YX(H*11/10,C-offset), G2); (YX(H*9/10,C+offset), G2); 
                                   (Mid(HR,BR), G2); (BC, G2); (BoL, End)])
        | Glyph('s') -> let X14, X2, X34, cOffsetX, cOffsetY = X/4, X/2, X*3/4, 100, 25
                        OpenCurve([(YX(X-max 0 (offset-thickness),R), G2); (YX(X, C-offset/2), G2); (YX(X34,L), G2)
                                   (YX(X2,C), Anchor); (YX(X2-cOffsetY,C+cOffsetX), Handle)
                                   (YX(X14,R), G2); (YX(B,C+offset/2), G2); (YX(B+max 0 (offset-thickness),L), End)
                                ])
        | Glyph('T') -> EList([Line(TL, TR); Line(TC, BC)])
        | Glyph('t') -> EList([Glyph('l'); Line(XL,XC)])
        | Glyph('U') -> OpenCurve([(TL, Corner); (HL, LineToCurve); (BC, G2); (HR, CurveToLine); (TR, End)])
        | Glyph('u') -> EList([Line(BN,XN)
                               OpenCurve([(BoN, Start); (BC, G2); (ML, CurveToLine); (XL, End)])])
        | Glyph('V') -> PolyLine([TL; BC; TR])
        | Glyph('v') -> PolyLine([XL; BC; XR])
        | Glyph('W') -> PolyLine([TL; BC; TR; YX(B,R+R/2); YX(T,R+R)])
        | Glyph('w') -> PolyLine([XL; YX(B,N/2); XN; YX(B,N+N/2); YX(X,N+N)])
        | Glyph('X') -> EList([Line(TL,BR); Line(TR,BL)])
        | Glyph('x') -> EList([Line(XL,BR); Line(XR,BL)])
        | Glyph('Y') -> EList([PolyLine([TL; HC; TR]); Line(HC,BC)])
        | Glyph('y') -> EList([OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (DoL, End)])
                               OpenCurve([(XL, Corner); (ML, LineToCurve); (BC, G2); (MR, CurveToLine); (XR, End)])])
        | Glyph('Z') -> PolyLine([TL; TR; BL; BR])
        | Glyph('z') -> PolyLine([XL; XR; BL; BR])

        | Glyph(' ') -> Space

        //default
        | Glyph(c) -> printfn "Glyph %c not defined" c
                      Dot(XC)
        | any -> any

    member this.reduce e =
        match e with
        | Line(p1, p2) -> OpenCurve([(p1, Start); (p2, End)]) |> this.reduce
        | PolyLine(points) -> let a = Array.ofList points
                              OpenCurve([for i in 0..a.Length-1 do yield (a.[i], if i=(a.Length-1) then End else Corner)])
                              |> this.reduce
        | OpenCurve(pts) -> OpenCurve([for p, t in pts do YX(reducePoint p), t])
        | ClosedCurve(pts) -> ClosedCurve([for p, t in pts do YX(reducePoint p), t])
        | Dot(p) -> Dot(YX(reducePoint(p)))
        | EList(elems) -> EList(List.map this.reduce elems)
        | Space -> Space
        | e -> this.getGlyph(e) |> this.reduce

    member this.elemWidth e =
        let maxX pts = List.fold max 0 (List.map (fst >> getXY >> fst) pts)
        match e with
        | OpenCurve(pts) -> maxX pts
        | ClosedCurve(pts) -> maxX pts
        | Dot(p) -> fst (getXY p)
        | EList(elems) -> List.fold max 0 (List.map this.elemWidth elems)
        | Space -> 
            let space = this.axes.height / 4  //according to https://en.wikipedia.org/wiki/Whitespace_character#Variable-width_general-purpose_space
            int ((1.0-this.axes.monospace) * float space + this.axes.monospace * float monospaceWidth)
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" e)

    member this.charHeight = this.axes.height - D + thickness * 2 + this.axes.leading

    ///distance from bottom of descenders to baseline ()
    member this.yBaselineOffset = - D + thickness

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
        let cap X Y theta isJoint =
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
                 (addPolarContrast X Y (thetaAligned - PI * 0.75) (fthickness * sqrt 2.0), Corner)]
            //make flared endcap
            elif this.axes.flare <> 0.0 && not isJoint then
                let preflareDist, preflareAng = toPolar fthickness -(fthickness*0.80)
                let flareDist, flareAng = toPolar fthickness ((this.axes.flare + 1.0) * fthickness)
                [(addPolarContrast X Y (theta + PI * 0.75) (fthickness * sqrt 2.0), Corner);
                 (addPolarContrast X Y (theta + preflareAng) preflareDist, LineToCurve);
                 (addPolarContrast X Y (thetaAligned + PI * 0.5 - flareAng) flareDist, Corner);
                 (addPolarContrast X Y (thetaAligned - PI * 0.5 + flareAng) flareDist, Corner);
                 (addPolarContrast X Y (theta - preflareAng) preflareDist, CurveToLine);
                 (addPolarContrast X Y (theta - PI * 0.75) (fthickness * sqrt 2.0), Corner)]
            elif this.axes.end_bulb <> 0.0 || isJoint then
                [(offsetPointCap X Y (thetaAligned + PI * 0.25), Corner);
                 (addPolarContrast X Y thetaAligned (fthickness * (1.0+this.axes.end_bulb)), G2);
                 (offsetPointCap X Y (thetaAligned - PI * 0.25), Corner)]
            else
                [(offsetPointCap X Y (thetaAligned + PI * 0.25), Corner);
                 (offsetPointCap X Y (thetaAligned - PI * 0.25), Corner)]
        let startCap (seg : SpiroSegment) =
            cap seg.X seg.Y (seg.tangent1 + PI) (this.isJoint ch seg)
        let endCap (seg : SpiroSegment) (lastSeg : SpiroSegment) = 
            cap seg.X seg.Y (lastSeg.tangent2) (this.isJoint ch seg)
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
            Font({this.axes with stroked = false; scratches = false; thickness = 2}).getSansOutlines dummyChar

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

    member this.reflect e =
        let el = this.reduce e
        let w = this.width el
        let reflectP p = 
            let x,y = getXY p
            YX(y, w-x)
        this.movePoints reflectP el

    member this.movePoints fn e = 
        match e with
        | OpenCurve(pts) ->
            OpenCurve([for p, t in pts do (fn p, t)])
        | ClosedCurve(pts) ->
            ClosedCurve([for p, t in pts do (fn p, t)])
        | Dot(p) -> Dot(fn p)
        | EList(elems) -> EList(List.map (this.movePoints fn) elems)
        | Space -> Space
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" e) 

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
        EList(elementToSegments e |> List.map splitSegment)

    member this.italicise e = 
        if this.axes.italic>0.0 && not this.axes.spline_not_spiro then
            e |> this.subdivide |> this.movePoints this.italiciseP
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
    member this.constrainTangents e = 
        let x1,x2,y1,y2 = bounds e
        let constrain (point : Point) type_ angle =
            let x, y = getXY point
            let angle = norm angle
            let offset = 20
            if x = x1 && angle > PI/2. then
                //constain to vertical up
                [(point, Anchor); (point + YX(offset,0), Handle)]
            elif x = x1 && angle < -PI/2. then
                //constain to vertical down
                [(point, Anchor); (point - YX(offset,0), Handle)]
            elif x = x2 && 0. < angle && angle < PI/2. then
                //constain to vertical up
                [(point, Anchor); (point + YX(offset,0), Handle)]
            elif x = x2 && -PI/2. < angle && angle < 0. then
                //constain to vertical down
                [(point, Anchor); (point - YX(offset,0), Handle)]
            // if segment.Y = y1 then
            //     svgSemiCircle x y r 'u'
            // if segment.Y = y2 then
            //     svgSemiCircle x y r 'd'
            else [(point, type_)]
        let constrainStart (segment : SpiroSegment) =
            constrain segment.Point segment.Type segment.tangent1
        let spiroConstrain spiro =
            match spiro with
            | SpiroOpenCurve(segments) ->
                [OpenCurve(List.collect constrainStart (segments.[0..segments.Length-2]) 
                            @ ( let angle = PI - segments.[segments.Length-2].tangent2
                                let lastSeg = segments.[segments.Length-1]
                                constrain lastSeg.Point lastSeg.Type angle))]
            | SpiroClosedCurve(segments) ->
                [ClosedCurve(List.collect constrainStart segments)]
            | SpiroDot(p) -> [Dot(p)]
            | SpiroSpace -> [Space]
        applyToSegments spiroConstrain e

    ///Scale width of glyph to monospace width based on axis monospace fraction
    member this.monospace e =
        if this.axes.monospace > 0.0 then
            let nonMono = Font({this.axes with monospace=0.0})
            let mono p = let x,y = getXY p
                         let full_scale = float monospaceWidth / float (this.elemWidth e)
                         let x_scale = (1.0 - this.axes.monospace) + this.axes.monospace * full_scale
                         YX(y, int (float x * x_scale))
            this.movePoints mono e
        else
            e        

    ///Convert spiro curves to bezier SVG curves
    member this.spiroToSvg spiro = 
        match spiro with
        | SpiroOpenCurve(segs) ->
            let bc = PathBezierContext()
            Spiro.SpirosToBezier (Some (Array.ofList segs)) false bc |> ignore
            [bc.ToString]
        | SpiroClosedCurve(segs) ->
            let bc = PathBezierContext()
            Spiro.SpirosToBezier (Some (Array.ofList segs)) true bc |> ignore
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
            elementToSegments elem |> List.collect this.spiroToSvg

    member this.getSvgPath element offsetX offsetY strokeWidth =
        let fillrule = "nonzero"
        let fillStyle = if this.axes.outline && this.axes.filled then "#000000" else "none"
        [
            "<path "
            "d='"
        ] @
        this.elementToSvg element @
        [
            "'"
            sprintf "transform='translate(%d,%d) scale(1,-1)'" offsetX offsetY
            sprintf "style='fill:%s;fill-rule:%s;stroke:#000000;stroke-width:%d'/>" fillStyle fillrule strokeWidth
        ]

    ///circles highlighting the knots (defined points on the spiro curves)
    member this.getSvgKnots offsetX offsetY outline elem =
        let x1,x2,y1,y2 = bounds elem
        let rec toSvgPoints elem =
            let circle (p, t) = 
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
            | OpenCurve(pts) | ClosedCurve(pts) -> pts |> List.collect circle
            | Dot(p) -> circle (p, G2)
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
        this.movePoints shift

    member this.translateByThickness = this.translateBy thickness thickness

    member this.charToElem ch =
        Glyph(ch) 
            |> this.reduce 
            |> if axes.constraints then this.constrainTangents else id
            |> this.monospace 
            |> this.translateByThickness

    member this.charToOutline ch =
        this.charToElem ch
            |> this.getOutline ch

    member this.charToSvg ch offsetX offsetY =
        // printfn "%c" ch
        let spine = this.charToElem ch
        let outline = spine |> this.getOutline ch
        [sprintf "<!-- %c -->" ch]
        @ this.getSvgPath outline offsetX offsetY 5
        @ if this.axes.show_knots then
            (spine |> this.italicise |> this.getSvgKnots offsetX offsetY false)
            @ if this.axes.outline then
                outline |> this.getSvgKnots offsetX offsetY true
              else []
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

    member this.stringToSvg (lines : string list) offsetX offsetY =
        let margin = 50
        let svg, lineWidths = this.stringToSvgLineInternal lines offsetX offsetY
        toSvgDocument 
            -margin
            -margin
            (List.max lineWidths + margin)
            (this.charHeight * lineWidths.Length + margin)
            svg

    member this.ElementToSegments = elementToSegments
    member this.GetXY = getXY
