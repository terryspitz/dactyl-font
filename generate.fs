// Functional Font by terryspitz
// Mar 2020

//TODOs:
//- move outline point inward only
//- fixed-width option
//- serifs
//- make variable font
//- join lines properly
//- try merging with https://magenta.tensorflow.org/svg-vae
//- add punctuation chars

open System
open System.IO
open System.Text.RegularExpressions

//https://github.com/wieslawsoltes/SpiroNet
open SpiroNet

module glyphs =

    // Variable values for the font
    type Axes = {
        width : int
        height : int
        x_height : int
        offset : int
        thickness : int
    }

    type Point =
        // Raw coordinates
        | YX of y: int * x: int

        // Y coordinate: Top,X-height,Half-height,Bottom
        // X coordinate: Left,Centre,Right
        // o adds/subtracts an offset to the dimension it follows
        | TL | TLo | TC | TR        // Top points: Left, Left offset inward, Centre, Right
        | ToL | ToC | ToR           // Top offset down
        | XL | XLo | XC | XRo | XR  // x-height
        | XoL | XoC | XoR           // x-height offset down
        | ML | MC | MR              // Midway down from x-height
        | HL | HLo | HC | HR        // half glyph height
        | HoR                       // half offset down
        | BoL | BoC | BoR           // Bottom offset up
        | BL | BLo | BC | BRo | BR  // Bottom
        | DoL                       // Descender offset up
        | DL | DC | DR              // Descender
        | BN | BoN | HN | XoN | XN | TN         // Narrow width points
        | Mid of p1 : Point * p2 : Point
        | Interp of p1 : Point * p2 : Point * frac : float

    type SCP = SpiroNet.SpiroControlPoint

    type Element = 
        | Glyph of c : char
        | Part of name : string
        | Line of p1: Point * p2: Point
        | PolyLine of list<Point>
        | OpenCurve of list<Point * SpiroNet.SpiroPointType>
        | ClosedCurve of list<Point * SpiroNet.SpiroPointType>
        | Dot of Point
        | Scale of Element * float
        | List of list<Element>

    type SpiroElement =
        | SpiroOpenCurve of list<SCP>
        | SpiroClosedCurve of list<SCP>
        | SpiroDot of Point

    let CurveToLine = SpiroNet.SpiroPointType.Left
    let LineToCurve = SpiroNet.SpiroPointType.Right
    let G2 = SpiroNet.SpiroPointType.G2
    let G4 = SpiroNet.SpiroPointType.G4
    let Start = SpiroNet.SpiroPointType.OpenContour
    let Corner = SpiroNet.SpiroPointType.Corner
    let End = SpiroNet.SpiroPointType.EndOpenContour
    let EndClosed = SpiroNet.SpiroPointType.End

    //class
    type Font (axes: Axes) =

        // X axis guides, from left
        let L = 0               // Left
        let R = axes.width      // Right = standard glyph width
        let N = R * 4/5         // Narrow glyph width
        let C = R / 2           // Centre

        // Y axis guides, from bottom-up
        let B = 0               // Bottom
        let X = axes.x_height   // x-height
        let M = X/2             // Midway down from x-height
        let T = axes.height     // Top = standard glyph caps height
        let H = T/2             // Half total height
        let D = -axes.height/2  // descender height
        let offset = axes.offset // offset from corners
        let dotHeight = max ((X+T)/2) (X+axes.thickness*3)

        let concatLines = String.concat "\n"
        let svgCircle x y r =
            sprintf "M %d,%d\n" (x-r) y +
            sprintf "C %d,%d %d,%d %d,%d\n" (x-r) (y+r/2) (x-r/2) (y+r) x (y+r) +
            sprintf "C %d,%d %d,%d %d,%d\n" (x+r/2) (y+r) (x+r) (y+r/2) (x+r) y +
            sprintf "C %d,%d %d,%d %d,%d\n" (x+r) (y-r/2) (x+r/2) (y-r) x (y-r) +
            sprintf "C %d,%d %d,%d %d,%d\n" (x-r/2) (y-r) (x-r) (y-r/2) (x-r) y +
            "Z" 

        member this.axes = axes
        
        member this.rewritePoint p = 
            match p with
            | YX(y,x) -> YX(y,x)
            | TL -> YX(T,L) | TLo -> YX(T,L+offset) | TC -> YX(T,C) | TR -> YX(T,R)
            | ToL -> YX(T-offset,L) | ToC -> YX(T-offset,C) | ToR -> YX(T-offset,R)
            | XL -> YX(X,L) | XLo -> YX(X,L+offset) | XC -> YX(X,C) | XRo -> YX(X,R-offset) | XR -> YX(X,R)
            | XoL -> YX(X-offset,L) | XoC -> YX(X-offset,C) | XoR -> YX(X-offset,R)
            | ML -> YX(M,L) | MC -> YX(M,C) | MR -> YX(M,R)
            | HL -> YX(H,L) | HLo -> YX(H,L+offset) | HC -> YX(H,C) | HR -> YX(H,R)
            | HoR -> YX(H-offset,R)
            | BoL -> YX(B+offset,L) | BoC -> YX(B+offset,C) | BoR -> YX(B+offset,R)
            | BL -> YX(B,L) | BLo -> YX(B,L+offset) | BC -> YX(B,C) | BRo -> YX(B,R-offset) | BR -> YX(B,R)
            | DoL -> YX(D+offset,L)
            | DL -> YX(D,L) | DC -> YX(D,C) | DR -> YX(D,R)
            | BN -> YX(B,N) | BoN -> YX(B+offset,N) | HN -> YX(H,N) | XoN -> YX(X-offset,N) | XN -> YX(X,N) | TN -> YX(T,N)
            | Mid(p1, p2) -> this.rewritePoint (Interp(p1, p2, 0.5))
            | Interp(p1, p2, f) -> let x1, y1 = (this.getXY p1) in let x2, y2 = (this.getXY p2)
                                   YX(y1+int(float(y2-y1)*f), x1+int(float(x2-x1)*f))
        
        member this.getXY p =
            match (this.rewritePoint p) with 
                YX(y,x) -> (x, y)

        //Straights: AEFHIKLMNTVWXYZklvwxyz147/=[]\`|*"'
        //Dots: ij:;!?
        //Curves: COScos36890()~
        //LeftUpright: BDPRb mnpr 
        //RightUpright: GJadgq
        //Other: QUefhtu25@#$€£_&-+{}%

        member this.getGlyph e =

            match e with

            // TODO: 
            // !"#£$%&'()*+,-./
            // 0123456789
            // :;<=>?@
            // [\]^_` {|}~
            | Part("adgqLoop") -> ClosedCurve([(XoR, Corner); (XC, G2); (ML, G2); (BC, G2); (BoR, Corner)])
            | Glyph('A') -> let f = float(H/2)/float(T)
                            List([PolyLine([BL; TC; BR]); PolyLine([BL; Interp(BL,TC,f); Interp(BR,TC,f); BR])])
            | Glyph('a') -> List([Line(XR, BR); Part("adgqLoop")])
            | Glyph('B') -> List([Glyph('P'); OpenCurve([(HL, Corner); (HC, LineToCurve); (Mid(HR, BR), G2); (BC, CurveToLine); (BL, End)])])
            | Glyph('b') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, G2); (BC, G2); (BoL, End)])])
            | Glyph('C') -> OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)])
            | Glyph('c') -> OpenCurve([(XoR, Start); (XC, G2); (ML, G2); (BC, G2); (BoR, End)])
            | Glyph('D') -> ClosedCurve([(BL, Corner); (TL, Corner); (TLo, LineToCurve);
                            (YX(H+offset,R), CurveToLine); (YX(H-offset,R), LineToCurve); (BLo, CurveToLine)])
            | Glyph('d') -> List([Line(BR, TR); Part("adgqLoop")])
            | Glyph('E') -> List([PolyLine([TR; TL; BL; BR]); Line(HL, HR)])
            | Glyph('e') -> OpenCurve([(ML, Start); (MR, Corner); (YX(M+offset,R), G2); (XC, G2); (ML, G2);
                            //(YX(B,C+offset), G2); (BoR, End)])
                            (BC, G2); (BoR, End)])
            | Glyph('F') -> List([PolyLine([TR; TL; BL]); Line(HL, HR)])
            | Glyph('f') -> List([OpenCurve([(TC, Start); (XL, CurveToLine); (BL, End)]); Line(XL, XC)])
            | Glyph('G') -> OpenCurve([(ToR, G2); (TC, G2); (HL, G2); (BC, G2); (HoR, CurveToLine); (HR, Corner); (HC, End)])
            | Glyph('g') -> List([Part("adgqLoop");
                                  OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (DoL, End)])])
            | Glyph('H') -> List([Line(BL, TL); Line(HL, HR); Line(BR, TR)])
            | Glyph('h') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, CurveToLine); (BR, End)])])
            | Glyph('I') -> Line(BL, TL)
            | Glyph('i') -> List([Line(XL, BL)
                                  Dot(YX(dotHeight,L))])
            | Glyph('J') -> OpenCurve([(TL, Corner); (TR, Corner); (HR, LineToCurve); (BC, G2); (BoL, End)])
            | Glyph('j') -> List([OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (DoL, End)])
                                  Dot(YX(dotHeight,R))])
            | Glyph('K') -> List([Line(TL, BL); PolyLine([TR; HL; BR])])
            | Glyph('k') -> List([Line(TL, BL); PolyLine([YX(X,N); ML; YX(B,N)])])
            | Glyph('L') -> PolyLine([TL; BL; BR])
            | Glyph('l') -> OpenCurve([(TL, Corner); (ML, LineToCurve); (BC, G2)])
            | Glyph('M') -> PolyLine([BL; TL; YX(B,R*3/4); YX(T,R*3/2); YX(B,R*3/2)])
            | Glyph('m') -> List([Glyph('n');
                                  OpenCurve([(YX(X-offset,N), Start); (YX(X,N+C), G2); (YX(M,N+N), CurveToLine); (YX(B,N+N), End)])])
            | Glyph('N') -> PolyLine([BL; TL; BR; TR])
            | Glyph('n') -> List([Line(XL,BL)
                                  OpenCurve([(XoL, Start); (XC, G2); (YX(M,N), CurveToLine); (BN, End)])])
            | Glyph('O') -> ClosedCurve([(HL, G2); (BC, G2); (HR, G2); (TC, G2)])
            | Glyph('o') -> ClosedCurve([(XC, G2); (ML, G2); (BC, G2); (MR, G2)])
            | Glyph('P') -> OpenCurve([(BL, Corner); (TL, Corner); (TC, LineToCurve); (Mid(TR, HR), G2); (HC, CurveToLine); (HL, End)])
            | Glyph('p') -> List([Line(XL, DL)
                                  OpenCurve([(XoL, Start); (XC, G2); (MR, G2); (BC, G2); (BoL, End)])])
            | Glyph('Q') -> List([Glyph('O'); Line(Mid(HC, BR), BR)])
            | Glyph('q') -> List([Line(XR, DR); Part("adgqLoop")])
            | Glyph('R') -> List([Glyph('P'); PolyLine([HL; HC; BR])])
            | Glyph('r') -> List([Line(BL,XL)
                                  OpenCurve([(XoL, Start); (XC, G2); (XoN, End)])])
            | Glyph('S') -> OpenCurve([(ToR, G2); (TC, G2); (Mid(TL,HL), G2); 
                                       (YX(H*11/10,C-offset), G2); (YX(H*9/10,C+offset), G2); 
                                       (Mid(HR,BR), G2); (BC, G2); (BoL, End)])
            | Glyph('s') -> let X14, X2, X34, cOffset = X/4, X/2, X*3/4, C/8
                            OpenCurve([(YX(X-offset,R), G2); (YX(X, C-offset/2), G2); (YX(X34,L), G2);
                                       (YX(X2,C-cOffset), CurveToLine); (YX(X2,C+cOffset), LineToCurve); 
                                       (YX(X14,R), G2); (YX(B,C+offset/2), G2); (YX(B+offset,L), End)])
            | Glyph('T') -> List([Line(TL, TR); Line(TC, BC)])
            | Glyph('t') -> List([Glyph('l'); Line(XL,XC)])
            | Glyph('U') -> OpenCurve([(TL, Corner); (HL, LineToCurve); (BC, G2); (HR, CurveToLine); (TR, End)])
            | Glyph('u') -> List([Line(BN,XN)
                                  OpenCurve([(BoN, Start); (BC, G2); (ML, CurveToLine); (XL, End)])])
            | Glyph('V') -> PolyLine([TL; BC; TR])
            | Glyph('v') -> PolyLine([XL; BC; XR])
            | Glyph('W') -> PolyLine([TL; BC; TR; YX(B,R+R/2); YX(T,R+R)])
            | Glyph('w') -> PolyLine([XL; YX(B,N/2); XN; YX(B,N+N/2); YX(X,N+N)])
            | Glyph('X') -> List([Line(TL,BR); Line(TR,BL)])
            | Glyph('x') -> List([Line(XL,BR); Line(XR,BL)])
            | Glyph('Y') -> List([PolyLine([TL; HC; TR]); Line(HC,BC)])
            | Glyph('y') -> List([OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (DoL, End)])
                                  OpenCurve([(XL, Corner); (ML, LineToCurve); (BC, G2); (BoR, End)])])
            | Glyph('Z') -> PolyLine([TL; TR; BL; BR])
            | Glyph('z') -> PolyLine([XL; XR; BL; BR])

            | Glyph(' ') -> PolyLine([])

            //default
            | Glyph(c) -> printfn "Glyph %c not defined" c
                          Dot(XC)
            | any -> any

        member this.reduce  e =
            match e with
            | Line(p1, p2) -> OpenCurve([(this.rewritePoint p1, Start); (this.rewritePoint p2, End)])
            | PolyLine(points) -> let a = Array.ofList points
                                  OpenCurve([for i in 0 .. a.Length-1 do
                                             let p = this.rewritePoint a.[i]
                                             yield (p, if i=(a.Length-1) then End else Corner)])
            | OpenCurve(curvePoints) -> OpenCurve([for p, t in curvePoints do this.rewritePoint p, t])
            | ClosedCurve(curvePoints) -> ClosedCurve([for p, t in curvePoints do this.rewritePoint p, t])
            | Dot(p) -> Dot(this.rewritePoint(p))
            | List(el) -> List(List.map (this.getGlyph >> this.reduce)  el)
            | e -> this.reduce(this.getGlyph(e))

        member this.width el =
            let thickness = this.axes.thickness
            match this.reduce(el) with
            | OpenCurve(curvePoints) -> thickness*2 + List.fold max 0 (List.map (fst >> this.getXY >> fst) curvePoints)
            | ClosedCurve(curvePoints) -> thickness*2 + List.fold max 0 (List.map (fst >> this.getXY >> fst) curvePoints)
            | Dot(p) -> thickness*2 + fst(this.getXY(p))
            | List(el) -> List.fold max 0 (List.map this.width el)

        member this.elementToSpiros e =
            match this.reduce(e) with
            | OpenCurve(curvePoints) -> [SpiroOpenCurve([
                                            for p, t in curvePoints do
                                                yield SCP(X=float(fst(this.getXY(p))), Y=float(snd(this.getXY p)),Type=t)])]
            | ClosedCurve(curvePoints) -> [SpiroClosedCurve([
                                            for p, t in curvePoints do
                                                yield SCP(X=float(fst(this.getXY(p))), Y=float(snd(this.getXY(p))),Type=t)])]
            | Dot(p) -> [SpiroDot(p)]
            | List(el) -> List.collect this.elementToSpiros (List.map this.getGlyph el)  //TODO

        member this.getOutlines e = 
            let thickness = this.axes.thickness
            let fThickness = float(thickness)
            let spiros = this.elementToSpiros(e)
            let offsetPoint(X, Y, theta, thickness) =
                let offsetX, offsetY = thickness * sin(theta), thickness * cos(theta)
                YX(int(Y+offsetX), int(X+offsetY))
            let offsetPointCap(X, Y, theta) = offsetPoint(X, Y, theta, fThickness * sqrt 2.0)
            let tangents (seg : SpiroSegment) = 
                // from https://levien.com/phd/thesis.pdf Equations 8.22 and 8.23
                let psi = Math.Atan(seg.ks.[1]/24.0)
                let th0 = -seg.ks.[0]/2.0 + seg.ks.[1]/8.0 - seg.ks.[2]/(8.0*6.0) + seg.ks.[3]/(16.0*24.0) + psi
                let th1 = seg.ks.[0]/2.0 + seg.ks.[1]/8.0 + seg.ks.[2]/(8.0*6.0) + seg.ks.[3]/(16.0*24.0) - psi
                (seg.seg_th + th0, seg.seg_th + th1)
            let offsetSegment (seg : SpiroSegment) (lastSeg : SpiroSegment) reverse =
                let norm(x) = if x>Math.PI then x-Math.PI*2.0 else if x<(-Math.PI) then x+Math.PI*2.0 else x
                let newType = if reverse then 
                                    match seg.Type with
                                    | SpiroPointType.Left -> SpiroPointType.Right
                                    | SpiroPointType.Right -> SpiroPointType.Left
                                    | x -> x
                               else seg.Type
                let angle = if reverse then -Math.PI/2.0 else Math.PI/2.0
                match seg.Type with
                | SpiroPointType.Corner ->
                    let th1, th2 = norm(lastSeg.seg_th + angle), norm(seg.seg_th + angle)
                    let bend = norm(th2 - th1)
                    let tightBend = if reverse then bend > Math.PI/2.0
                                               else bend < -Math.PI/2.0
                    //if tightBend then
                        //outer bend
                    [(offsetPoint(seg.X, seg.Y, th1, fThickness), newType);
                     (offsetPoint(seg.X, seg.Y, th2, fThickness), newType)]
                    //else //inner
                    //    [(offsetPoint(seg.X, seg.Y, th1 + bend/2.0, abs thickness/sin(bend/2.0)), segType)]
                | SpiroPointType.Right ->
                    [(offsetPoint(seg.X, seg.Y, norm(lastSeg.seg_th + angle), fThickness), newType)]
                | SpiroPointType.Left ->
                    [(offsetPoint(seg.X, seg.Y, norm(seg.seg_th + angle), fThickness), newType)]
                | _ ->
                    [(offsetPoint(seg.X, seg.Y, fst (tangents seg) + angle, fThickness), newType)]

            let offsetSegments (segments : list<SpiroSegment>) start endP reverse =
                let newPoints = 
                    [for i in start .. endP do
                        let seg = if i=segments.Length-1 then segments.[0] else segments.[i]
                        let lastSeg = segments.[i-1]
                        offsetSegment seg lastSeg reverse
                    ]
                List.collect id newPoints
            let offsetMidSegments(segments, reverse) =
                offsetSegments segments 1 (segments.Length-2) reverse
            let startCap (seg : SpiroSegment) =
                [(offsetPointCap(seg.X, seg.Y, fst (tangents seg) - Math.PI * 0.75), Corner);
                 (offsetPointCap(seg.X, seg.Y, fst (tangents seg) + Math.PI * 0.75), Corner)]
            let endCap (seg : SpiroSegment) (lastSeg : SpiroSegment) = 
                [(offsetPointCap(seg.X, seg.Y, snd (tangents lastSeg) + Math.PI/4.0), Corner);
                 (offsetPointCap(seg.X, seg.Y, snd (tangents lastSeg) - Math.PI/4.0), Corner)]
            let spiroToOffsetElement spiro =
                let reverseList list = List.fold (fun acc elem -> elem::acc) [] list
                match spiro with
                | SpiroOpenCurve(scps) ->
                    let segments = SpiroNet.Spiro.SpiroCPsToSegments(Array.ofList scps, scps.Length, false)
                    if not (isNull segments) then 
                        let points = startCap segments.[0]
                                     @ offsetMidSegments(List.ofArray segments, false)
                                     @ endCap segments.[segments.Length-1] segments.[segments.Length-2]
                                     @ reverseList(offsetMidSegments(List.ofArray segments, true))
                        [ClosedCurve(points)]
                    else [ClosedCurve([])]
                | SpiroClosedCurve(scps) ->
                    let segments = List.ofArray(Spiro.SpiroCPsToSegments(Array.ofList scps, scps.Length, true))
                    [ClosedCurve(offsetSegments segments 1 (segments.Length-1) false);
                     ClosedCurve(reverseList(offsetSegments segments 1 (segments.Length-1) true))]
                | SpiroDot(p) -> [Dot(p)]
            List(List.collect spiroToOffsetElement spiros)

        member this.toSvgBezierCurve spiro = 
            match spiro with
            | SpiroOpenCurve(scps) ->
                let bc = SpiroNet.Editor.PathBezierContext()
                let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList scps, scps.Length, false, bc)
                bc.ToString()
            | SpiroClosedCurve(scps) ->
                let bc = SpiroNet.Editor.PathBezierContext()
                let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList scps, scps.Length, true, bc)
                bc.ToString()
            | SpiroDot(p) -> let x, y = this.getXY(p)
                             svgCircle x y this.axes.thickness

        member this.getSvgCurves element offsetX offsetY strokeWidth filled outlines =
            let thickness = this.axes.thickness
            let spirosPath = this.elementToSpiros element
            let spiros = 
                if outlines then
                    let outlineElement = this.getOutlines element
                    let debug = false
                    if debug then
                        printfn "%s" (spirosPath.ToString())
                        printfn "%s" (outlineElement.ToString())
                    this.elementToSpiros(outlineElement)
                else spirosPath

            let fillrule = match spirosPath.[0] with
                            | SpiroClosedCurve(_) -> "evenodd"
                            | _ -> "nonzero"
            let svg = spiros |> List.map this.toSvgBezierCurve |> concatLines
            let fillStyle = if filled then "#000000" else "none"
            sprintf "<path d='%s' transform='scale(1,-1) translate(%d,%d)' " svg (offsetX+thickness) (offsetY+thickness) +
                sprintf "style='fill:%s;fill-rule:%s;stroke:#000000;stroke-width:%d'/>\n" fillStyle fillrule strokeWidth

        member this.getSvgPoints element offsetX offsetY =
            let thickness = this.axes.thickness
            let toSvgPoints (spiro : SpiroElement) : string = 
                let point(x,y) = svgCircle (int x) (int y) 50
                match spiro with
                | SpiroOpenCurve(scps) -> scps |> List.map (fun scp -> point(scp.X, scp.Y)) |> concatLines
                | SpiroClosedCurve(scps) -> scps |> List.map (fun scp -> point(scp.X, scp.Y)) |> concatLines
                | SpiroDot(p) -> let x,y = this.getXY(p) in point(float(x), float(y))
            let svg = element |> this.elementToSpiros |> List.map toSvgPoints |> concatLines
            // small red circles
            sprintf "<!-- points --><path d='%s' transform='scale(1,-1) translate(%d,%d)' " svg (offsetX+thickness) (offsetY+thickness) + 
                "style='fill:none;stroke:#ffaaaa;stroke-width:10'/>\n"

        member this.charToSvg ch offsetX offsetY outlines filled points =
            let element = Glyph(ch)
            let path = this.getSvgCurves element offsetX offsetY 20 false false
            sprintf "<!-- %c -->\n\n" ch +
            if outlines then
                this.getSvgCurves element offsetX offsetY 5 filled outlines +
                    (if filled then "" else path) +
                    (if points then this.getSvgPoints element offsetX offsetY else "")
            else path + 
                 (if points then this.getSvgPoints element offsetX offsetY else "")

        member this.stringToSvg (str : string) offsetX offsetY outlines filled points =
            let widths = [for ch in str do this.width (Glyph(ch))]
            let offsetXs = List.scan (fun a e -> a+e+50) offsetX widths
            String.concat "\n"
                [for c in 0 .. str.Length - 1 do
                    printfn "%c" str.[c]
                    yield this.charToSvg str.[c] (offsetXs.[c]) offsetY outlines filled points]

        member this.toFontForgeGlyph (ch : char) =
            // reverse engineered from saved font  
            // TODO: coords shifted by (thickness, thickness) (hard for beziers)
            let thickness = this.axes.thickness
            let scpToString (scp : SCP) = sprintf "%f %f %c" scp.X scp.Y (char scp.Type)
            let toFFSpiro spiro =
                //rearrange SVG bezier curve format to fontforge format
                let matchEval (amatch : Match) = amatch.Groups.[2].Value.Replace(","," ") + " "
                                                 + amatch.Groups.[1].Value.ToLower() + " 0"
                let reorder s = Regex.Replace(s, "(.) (.*)", matchEval)
                let bezierString = this.toSvgBezierCurve spiro |> fun s-> s.Split("\r\n") 
                                   |> Array.map reorder |> String.concat "\n"
                let spiroString =
                    match spiro with
                    | SpiroOpenCurve(scps) -> scps |> List.map scpToString |> concatLines
                    | SpiroClosedCurve(scps) -> scps |> List.map scpToString |> concatLines
                    | SpiroDot(p) -> let x,y = this.getXY(p) 
                                     sprintf "%d %d o " x (y+thickness) +
                                     sprintf "%d %d o " (x+thickness) y +
                                     sprintf "%d %d o " (x+thickness*2) (y+thickness)
                sprintf """
                        %s
                        Spiro
                        %s
                        0 0 z
                        EndSpiro
                        """ bezierString spiroString
            let frameSpiros = (Glyph(ch)) |> this.elementToSpiros |> 
                              List.map toFFSpiro |> concatLines
            // let frameSpiros = Font({this.axes with thickness = 2}).getOutlines (Glyph(ch)) 
            //                   |> this.elementToSpiros |> List.map toString |> concatLines
            let outlineSpiros = this.getOutlines (Glyph(ch)) |> this.elementToSpiros |> 
                                List.map toFFSpiro |> concatLines
            sprintf "StartChar: %c\n" ch +
            sprintf "Encoding: %d %d 0\n" (int ch) (int ch) +
            sprintf "Width: %d\n" (this.width (Glyph(ch)) + thickness) +
            sprintf """
                    InSpiro: 1
                    Flags: H
                    LayerCount: 2
                    Back
                    SplineSet
                        %s
                    EndSplineSet
                    Fore
                    SplineSet
                        %s
                    EndSplineSet
                    EndChar
                    """ frameSpiros outlineSpiros

    let toSvgDocument rows cols path =
        sprintf """<svg xmlns='http://www.w3.org/2000/svg'
                viewBox='0 -%d %d %d'>
                <g id='layer1'>
                %s
                </g>
                </svg>""" (rows*1300) (cols*700) (rows*1300) path

//end module


let writeFile filename (text : string) = 
    let trim (x : string) = x.Trim()
    let trimmedText = text.Split("\n") |> Array.map trim |> String.concat "\n"
    printfn "Writing %s" filename
    File.WriteAllText(filename, trimmedText) |> ignore

[<EntryPoint>]
let main argv =

    // let scps = [|glyphs.SCP(X=1.0,Y=0.0,Type=glyphs.G2);
    //              glyphs.SCP(X=1.0,Y=1.0,Type=glyphs.G2);
    //              glyphs.SCP(X=0.5,Y=2.0,Type=glyphs.G4);
    //              glyphs.SCP(X=(0.0),Y=2.0,Type=glyphs.G2) |]
    // let segments = SpiroNet.Spiro.SpiroCPsToSegments(scps, scps.Length, false)
    // for seg in segments do
    //     printfn "x=%f y=%f segth=%f bendth=%f ks=%A" seg.X seg.Y seg.seg_th seg.bend_th seg.ks
    //     let psi = Math.Atan(seg.ks.[1]/24.0)
    //     let th0 = seg.ks.[0]/2.0 - seg.ks.[1]/8.0 + psi 
    //     let th1 = seg.ks.[0]/2.0 + seg.ks.[1]/8.0 - psi
    //     printfn "psi=%f th0,1=(%f, %f) seg=%f, (%f, %f)" psi th0 th1 seg.seg_th (seg.seg_th-th0) (seg.seg_th+th1)

    //let chars = [ for c in 'A'..'Z' -> c ] @ [ for c in  'a'..'z' -> c ]
    //let chars = "The truth is in there,  don't let it out"
    let chars = "THE QUICK BROWN FOX JUMPS OVER  THE LAZY DOG the quick brown fox jumps over the lazy dog"
    //let chars = "j"
    let cols = 16
    let rows = (chars.Length - 1)/cols + 1
    let outlines = true
    let filled = true
    let points = true
    let font1 = glyphs.Font({ glyphs.width = 300; height = 600; x_height = 400; offset = 50; thickness = 30;})
    let font2 = glyphs.Font({ glyphs.width = 300; height = 600; x_height = 400; offset = 100; thickness = 30;})

    // SVG output, side by side
    let rowHeight = 1024
    printfn "charsPerRow: %d, rows: %d" cols rows |> ignore
    let svgRows (font : glyphs.Font) xOffset =
        [for r in 0 .. rows do
            let rowChars = chars.[r*cols .. min ((r+1)*cols-1) (chars.Length-1)]
            font.stringToSvg rowChars xOffset ((rows-r)*rowHeight) outlines filled points
        ] |> String.concat("\n")
    let svg = svgRows font1 0 + svgRows font2 6000

    writeFile @".\allGlyphs.svg" (glyphs.toSvgDocument rows cols svg)

    // FontForge output
    printfn(@"Writing font to dactyl\")
    let dir = @".\dactyl.sfdir"
    if not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> printfn "%A"
        File.Copy(@".\font.props", dir)
    for ch in chars do 
        let prefix = if ch>='A' && ch<='Z' then "_" else ""
        font2.toFontForgeGlyph ch |> writeFile (sprintf @"%s\%s%c.glyph" dir prefix ch)

    let outputInterpolatedStr = false
    // Interpolate as in https://levien.com/spiro/s_interp2.png
    if outputInterpolatedStr then
        let str = "dog"
        [
            for r in 1..10 do
            for c in 1..10 do
                let font = glyphs.Font({glyphs.width = 300; height = 600;
                                        x_height = (11-r)*60; offset = c*30; thickness = r*6;})
                font.stringToSvg str (c*600*str.Length) ((11-r)*1000) true true false
        ] |> String.concat "\n" |> glyphs.toSvgDocument 10 30 |> writeFile @".\interp.svg"

    0 // return code
