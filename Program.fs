// Functional Font by terryspitz
// Mar 2020

open System
open SpiroNet
open System.IO

module glyphs =
    // Variables
    let L = 0       // Left
    let R = 600     // Right = standard glyph width
    let N = R*4/5   // Narrow glyph width
    let C = R / 2   // Centre
    let B = 0       // Bottom
    let X = 600     // x-height
    let M = X/2     // Midway down from x-height
    let T = 1000    // Top = standard glyph caps height
    let H = T/2     // Half total height
    let D = -500    // descender height
    let offset = 150  // offset from corners
    let thickness = 100.0

    type Point =
        // Raw coordinates
        | YX of y: int * x: int

        // Y coordinate: Top,X-height,Half-height,Bottom
        // X coordinate: Left,Centre,Right
        // o adds/subtracts an offset to the dimension it follows
        | TL | TLo | TC | TR        // Top points: Left, Left offset inward, Centre, Right
        | ToL | ToC | ToR           // Top offset down
        | XL | XLo | XC | XRo | XR        // x-height
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

    
    let rec rewritePoint(p : Point) : Point = 
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
        | Mid(p1, p2) -> rewritePoint(Interp(p1, p2, 0.5))
        | Interp(p1, p2, f) -> let x1, y1 = getXY(p1) in let x2, y2 = getXY(p2)
                               YX(y1+int(float(y2-y1)*f), x1+int(float(x2-x1)*f))
    and getXY(p : Point) : int * int =
        match rewritePoint(p) with 
            YX(y,x) -> (x, y)

    type SCP = SpiroNet.SpiroControlPoint
    let CurveToLine = SpiroNet.SpiroPointType.Left
    let LineToCurve = SpiroNet.SpiroPointType.Right
    let G2 = SpiroNet.SpiroPointType.G2
    let G4 = SpiroNet.SpiroPointType.G4
    let Start = SpiroNet.SpiroPointType.OpenContour
    let Corner = SpiroNet.SpiroPointType.Corner
    let End = SpiroNet.SpiroPointType.EndOpenContour
    let EndClosed = SpiroNet.SpiroPointType.End

    type Element = 
        | Glyph of c : char
        | Line of p1: Point * p2: Point
        | PolyLine of list<Point>
        | OpenCurve of list<Point * SpiroNet.SpiroPointType>
        | ClosedCurve of list<Point * SpiroNet.SpiroPointType>
        | Dot of Point
        | Scale of Element * float
        | List of list<Element>

    //Straights: AEFHIKLMNTVWXYZklvwxyz147/=[]\`|*"'
    //Dots: ij:;!?
    //Curves: COScos36890()~
    //LeftUpright: BDPRb mnpr 
    //RightUpright: GJadgq
    //Other: QUefhtu25@#$€£_&-+{}%

    let rec getGlyph (e: Element) : Element =
        match e with
        | Glyph('A') -> let Y, f = T-X, float(T-X)/float(T)
                        List([PolyLine([BL; TC; BR]); Line(Interp(BL,TC,f), Interp(BR,TC,f))])
        | Glyph('a') -> List([Line(XR, BR); Glyph('c')])
        | Glyph('B') -> List([Glyph('P'); OpenCurve([(HL, Corner); (HC, LineToCurve); (Mid(HR, BR), G2); (BC, CurveToLine); (BL, End)])])
        | Glyph('b') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, G2); (BC, G2); (BoL, End)])])
        | Glyph('C') -> OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)])
        | Glyph('c') -> OpenCurve([(XoR, Start); (XC, G2); (ML, G2); (BC, G2); (BoR, End)])
        | Glyph('D') -> List([Line(BL, TL); OpenCurve([(TL, Corner); (TLo, LineToCurve); (HR, G2); (BLo, CurveToLine); (BL, End)])])
        | Glyph('d') -> List([Line(BR, TR); Glyph('c')]) // or flip b
        | Glyph('E') -> List([PolyLine([TR; TL; BL; BR]); Line(HL, HR)])
        | Glyph('e') -> List([OpenCurve([(ML, Start); (MR, Corner); (XC, G2); (ML, G2); (BC, G2); (BoR, End)])])
        | Glyph('F') -> List([PolyLine([TR; TL; BL]); Line(HL, HR)])
        | Glyph('f') -> List([OpenCurve([(TC, Start); (XL, CurveToLine); (BL, End)]); Line(XL, XC)])
        | Glyph('G') -> OpenCurve([(ToR, G2); (TC, G2); (HL, G2); (BC, G2); (Mid(BR,HR), CurveToLine); (HR, Corner); (HC, End)])
        | Glyph('g') -> List([Glyph('c');
                              OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (Mid(BL, DL), End)])])
        | Glyph('H') -> List([Line(BL, TL); Line(HL, HR); Line(BR, TR)])
        | Glyph('h') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, CurveToLine); (BR, End)])])
        | Glyph('I') -> Line(BC, TC)
        | Glyph('i') -> List([Line(XC, BC)
                              Dot(Mid(XC, TC))])
        | Glyph('J') -> List([Line(TL, YX(T,R+offset))
                              OpenCurve([(TR, Corner); (HR, LineToCurve); (BC, G2); (BoL, End)])])
        | Glyph('j') -> List([OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (DoL, End)])
                              Dot(Mid(XR, TR))])
        | Glyph('K') -> List([Line(TL, BL); PolyLine([TR; HL; BR])])
        | Glyph('k') -> List([Line(TL, BL); PolyLine([YX(X,N); ML; YX(B,N)])])
        | Glyph('L') -> PolyLine([TL; BL; BR])
        | Glyph('l') -> OpenCurve([(TL, Corner); (ML, LineToCurve); (BC, G2)])
        | Glyph('M') -> PolyLine([BL; TL; BR; YX(T,R+R); YX(B,R+R)])
        | Glyph('m') -> List([Glyph('n');
                              OpenCurve([(YX(M,N), Start); (YX(X,N+N/2), G2); (YX(M,N+N), CurveToLine); (YX(B,N+N), End)])])
        | Glyph('N') -> PolyLine([BL; TL; BR; TR])
        | Glyph('n') -> List([Line(XL,BL)
                              OpenCurve([(XoL, Start); (XC, G2); (YX(M,N), CurveToLine); (BN, End)])])
        | Glyph('O') -> ClosedCurve([(HL, G2); (BC, G2); (HR, G2); (TC, G2)])
        | Glyph('o') -> ClosedCurve([(XC, G2); (ML, G2); (BC, G2); (Mid(HR,BR), G2)])
        | Glyph('P') -> List([Line(TL, BL)
                              OpenCurve([(TL, Corner); (TC, LineToCurve); (Mid(TR, HR), G2); (HC, CurveToLine); (HL, End)])])
        | Glyph('p') -> List([Line(XL, DL)
                              OpenCurve([(XoL, Start); (XC, G2); (MR, G2); (BC, G2); (BoL, End)])])
        | Glyph('Q') -> List([Glyph('O'); Line(Mid(HC, BR), BR)])
        | Glyph('q') -> List([Line(XR, DR); Glyph('c')])
        | Glyph('R') -> List([Line(BL, TL)
                              OpenCurve([(TL, Corner); (TC, LineToCurve); (Mid(TR, HR), G2); (HC, CurveToLine); (HL, End)])
                              Line(HC, BR)])
        | Glyph('r') -> List([Line(BL,XL)
                              OpenCurve([(XoL, Start); (XC, G2); (XoN, End)])])
        | Glyph('S') -> OpenCurve([(ToR, G2); (TC, G2); (Mid(TL,HL), G2); (HC, G2); (Mid(HR,BR), G2); (BC, G2); (BoL, End)])
        | Glyph('s') -> let X14, X2, X34, offsetX, offsetY = X/4, X/2, X*3/4, N/16, X/6
                        OpenCurve([(YX(X34,N), Start); (YX(X34, L+offsetX), G2); (YX(X2+offsetY,L+offsetX), G2);
                                   (YX(X2-offsetY,N-offsetX), G2); (YX(X14,N-offsetX), G2); (YX(X14,L), End)])
        // | Glyph('s') -> let X14, X2, X34 = X/4, X/2, X*3/4
        //                 OpenCurve([(YX(X34,R), G2); (XRo, G2); (XLo, G2); (YX(X34,L), G2); //(YX(X2,C),G2);
        //                            (YX(X14,R), G2); (BLo, G2); (YX(X14,L), End)])
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
        //default
        | Glyph(c) -> printfn "Glyph %c not defined" c
                      Dot(XC)
        | any -> any

    and reduce(e: Element) : Element =
        match e with
        | Line(p1, p2) -> OpenCurve([(rewritePoint(p1), Start); (rewritePoint(p2), End)])
        | PolyLine(points) -> let a = Array.ofList points
                              OpenCurve([for i in 0 .. a.Length-1 do
                                         let p = rewritePoint(a.[i])
                                         yield (p, if i=(a.Length-1) then End else Corner)])
        | OpenCurve(curvePoints) -> OpenCurve([for p, t in curvePoints do rewritePoint(p), t])
        | ClosedCurve(curvePoints) -> ClosedCurve([for p, t in curvePoints do rewritePoint(p), t])
        | Dot(p) -> Dot(rewritePoint(p))
        | List(el) -> List(List.map (getGlyph >> reduce)  el)
        | e -> reduce(getGlyph(e))

    type SpiroElement =
    | SpiroOpenCurve of list<SCP>
    | SpiroClosedCurve of list<SCP>
    | SpiroDot of Point

    let rec width(e: Element) : int =
        match reduce(e) with
        | OpenCurve(curvePoints) -> List.fold max 0 (List.map (fst >> getXY >> fst) curvePoints)
        | ClosedCurve(curvePoints) -> List.fold max 0 (List.map (fst >> getXY >> fst) curvePoints)
        | Dot(p) -> fst(getXY(p))
        | List(el) -> List.fold max 0 (List.map width el)

    let rec elementToSpiros(e: Element) : list<SpiroElement> =
        match reduce(e) with
        | OpenCurve(curvePoints) -> [SpiroOpenCurve([
                                        for p, t in curvePoints do
                                            yield SCP(X=float(fst(getXY(p))), Y=float(snd(getXY p)),Type=t)])]
        | ClosedCurve(curvePoints) -> [SpiroClosedCurve([
                                        for p, t in curvePoints do
                                            yield SCP(X=float(fst(getXY(p))), Y=float(snd(getXY(p))),Type=t)])]
        | Dot(p) -> [SpiroDot(p)]
        | List(el) -> List.collect elementToSpiros (List.map getGlyph el)  //TODO

    let getOutlines(e : Element) : Element = 
        let spiros = elementToSpiros(e)
        let offsetPoint(X: float, Y: float, theta: float) =
            let offsetX, offsetY = float(thickness)*sin(theta), float(thickness)*cos(theta)
            YX(int(Y+offsetX), int(X+offsetY))
        let offsetMidSegments(segments : list<SpiroSegment>, reverse : bool) : list<Point * SpiroPointType> =
            [for i in 1 .. segments.Length-2 do
             let seg, angle = segments.[i], if reverse then -Math.PI/2.0 else Math.PI/2.0
             let lastSeg = segments.[i-1]
             let norm(x:float) = if x>Math.PI then x-Math.PI*2.0 else if x<(-Math.PI) then x+Math.PI*2.0 else x
             let th1, th2 = norm(lastSeg.seg_th + angle), norm(seg.seg_th + angle)
             (offsetPoint(seg.X, seg.Y, th1 + norm(th2 - th1)/2.0), seg.Type)]
        let spiroToOffsetElement(spiro : SpiroElement) : list<Element> =
            match spiro with
            | SpiroOpenCurve(scps) ->
                let reverseList list = List.fold (fun acc elem -> elem::acc) [] list
                let startCap(seg: SpiroSegment)=
                    [(offsetPoint(seg.X, seg.Y, seg.seg_th-Math.PI/2.0), Corner);
                     (offsetPoint(seg.X, seg.Y, seg.seg_th+Math.PI/2.0), Corner)]
                let endCap(seg: SpiroSegment, lastSeg: SpiroSegment) = 
                    [(offsetPoint(seg.X, seg.Y, lastSeg.seg_th+Math.PI/2.0), Corner);
                     (offsetPoint(seg.X, seg.Y, lastSeg.seg_th-Math.PI/2.0), Corner)]
                let segments = SpiroNet.Spiro.SpiroCPsToSegments(Array.ofList scps, scps.Length, false)
                let points = offsetMidSegments(List.ofArray segments, false)
                             @ endCap(segments.[segments.Length-1], segments.[segments.Length-2])
                             @ reverseList(offsetMidSegments(List.ofArray segments, true))
                             @ startCap(segments.[0])
                [ClosedCurve(points)]
            | SpiroClosedCurve(scps) ->
                let segments = List.ofArray(Spiro.SpiroCPsToSegments(Array.ofList scps, scps.Length, true))
                [ClosedCurve(offsetMidSegments(segments, false));
                 ClosedCurve(offsetMidSegments(segments, true))]
            | SpiroDot(p) -> [Dot(p)]
        List(List.collect spiroToOffsetElement spiros)

    let getSvg(spiros : list<SpiroElement>, offsetX : float, offsetY : float) : string =
        let toSvg(spiro : SpiroElement) : string = 
            match spiro with
            | SpiroOpenCurve(scps) ->
                let bc = SpiroNet.Editor.PathBezierContext()
                let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList scps, scps.Length, false, bc, offsetX, offsetY)
                bc.ToString()
            | SpiroClosedCurve(scps) ->
                let bc = SpiroNet.Editor.PathBezierContext()
                let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList scps, scps.Length, true, bc, offsetX, offsetY)
                bc.ToString()
            | SpiroDot(p) -> let x, y = getXY(p)
                             let xx, yy = x+int(offsetX), y+int(offsetY)
                             let r = int(thickness/2.0)
                             sprintf "M %d,%d" (xx-r) yy +
                             sprintf "A %d,%d 0 1,0 %d,%d" r r (xx+r) yy +
                             sprintf "A %d,%d 0 1,0 %d,%d" r r (xx-r) yy
        let svg = List.map toSvg spiros
        String.Join("\n", svg)

    //end module

let toSvgPath(strokeWidth: int, svg: string) =
    sprintf "<path d='%s' style='fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:%d' \
                transform='scale(1,-1) translate(0,-6900)' />" svg strokeWidth
let toSvgXml(path: string) =
    sprintf "<svg xmlns='http://www.w3.org/2000/svg' \
        viewBox='0 0 8000 7000' > \
      <g id='layer1'> %s </g> \
    </svg>" path

[<EntryPoint>]
let main argv =
    //let chars = [ for c in 'A'..'Z' -> c ] @ [ for c in  'a'..'z' -> c ]
    let chars = "THEQUICKBROWNFOXJUMPSOVERTHELAZYDOGthequickbrownfoxjumpsoverthelazydog"
    //let chars = "The truth is in there,  don't let it out"
    //let chars = "E"
    let charToSvg(c : char, i : int) : string =
        let offsetX, offsetY = float((i%12)*800), float(5500-(i/12)*1100)
        let spiros = glyphs.elementToSpiros(glyphs.Glyph(c))
        let path = toSvgPath(20, glyphs.getSvg(spiros, offsetX, offsetY))
        let calcOutlines = true
        if calcOutlines then
            let segments = glyphs.getOutlines(glyphs.Glyph(c))
            let spiros = glyphs.elementToSpiros(segments)
            toSvgPath(5, glyphs.getSvg(spiros, offsetX, offsetY)) + path
        else path
    let svg = [
        for i in 0 .. (chars.Length - 1) do
            let c = chars.[i]
            printfn "%c" c
            yield charToSvg(c, i)
    ]
    printfn("Writing test.svg")
    File.WriteAllText(@".\test.svg", String.Join("\n\n", svg) |> toSvgXml) |> ignore
    0 // return an integer exit code
