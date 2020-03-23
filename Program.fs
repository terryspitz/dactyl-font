// Functional Font by terryspitz
// Mar 2020

//TODOs:
//- move outline point inward only
//- fix angles of first/last outline pairs
//- fixed-width option
//- serifs
//- make font
//- make variable font 
//- try merging with https://magenta.tensorflow.org/svg-vae
//- add punctuation chars

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

    
    let rec rewritePoint(p : Point) = 
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
    and getXY(p : Point) =
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

    let rec getGlyph (e: Element) =
        match e with
        | Glyph('A') -> let Y, f = T-X, float(T-X)/float(T)
                        List([PolyLine([BL; TC; BR]); Line(Interp(BL,TC,f), Interp(BR,TC,f))])
        | Glyph('a') -> List([Line(XR, BR); Glyph('c')])
        | Glyph('B') -> List([Glyph('P'); OpenCurve([(HL, Corner); (HC, LineToCurve); (Mid(HR, BR), G2); (BC, CurveToLine); (BL, End)])])
        | Glyph('b') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, G2); (BC, G2); (BoL, End)])])
        | Glyph('C') -> OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)])
        | Glyph('c') -> OpenCurve([(XoR, Start); (XC, G2); (ML, G2); (BC, G2); (BoR, End)])
        | Glyph('D') -> List([Line(BL, TL); OpenCurve([(TL, Corner); (TLo, LineToCurve); (HR, G2); (BLo, CurveToLine); (BL, End)])])
        //| Glyph('D') -> ClosedCurve([(BL, Corner); (TL, Corner); (TLo, LineToCurve); (HR, G2); (BLo, CurveToLine)])
        | Glyph('d') -> List([Line(BR, TR); Glyph('c')]) // or flip b
        | Glyph('E') -> List([PolyLine([TR; TL; BL; BR]); Line(HL, HR)])
        | Glyph('e') -> List([OpenCurve([(ML, Start); (MR, Corner); (XC, G2); (ML, G2); (BC, G2); (BoR, End)])])
        | Glyph('F') -> List([PolyLine([TR; TL; BL]); Line(HL, HR)])
        | Glyph('f') -> List([OpenCurve([(TC, Start); (XL, CurveToLine); (BL, End)]); Line(XL, XC)])
        | Glyph('G') -> OpenCurve([(ToR, G2); (TC, G2); (HL, G2); (BC, G2); (HoR, CurveToLine); (HR, Corner); (HC, End)])
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
        | Glyph('M') -> PolyLine([BL; TL; YX(B,R*3/4); YX(T,R*3/2); YX(B,R*3/2)])
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
        | Glyph('s') -> let X14, X2, X34 = X/4, X/2, X*3/4
                        OpenCurve([(YX(X34,R), G2); (XRo, G2); (XLo, G2); (YX(X34,L), G2); (YX(X2,C),G2);
                                   (YX(X14,R), G2); (BRo, G2); (BLo, G2); (YX(X14,L), End)])
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

    and reduce(e: Element) =
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

    let rec width(el) =
        match reduce(el) with
        | OpenCurve(curvePoints) -> List.fold max 0 (List.map (fst >> getXY >> fst) curvePoints)
        | ClosedCurve(curvePoints) -> List.fold max 0 (List.map (fst >> getXY >> fst) curvePoints)
        | Dot(p) -> fst(getXY(p))
        | List(el) -> List.fold max 0 (List.map width el)

    let rec elementToSpiros(e: Element) =
        match reduce(e) with
        | OpenCurve(curvePoints) -> [SpiroOpenCurve([
                                        for p, t in curvePoints do
                                            yield SCP(X=float(fst(getXY(p))), Y=float(snd(getXY p)),Type=t)])]
        | ClosedCurve(curvePoints) -> [SpiroClosedCurve([
                                        for p, t in curvePoints do
                                            yield SCP(X=float(fst(getXY(p))), Y=float(snd(getXY(p))),Type=t)])]
        | Dot(p) -> [SpiroDot(p)]
        | List(el) -> List.collect elementToSpiros (List.map getGlyph el)  //TODO

    let getOutlines(e : Element, thickness) = 
        let spiros = elementToSpiros(e)
        let offsetPointByThickness(X, Y, theta, thickness) =
            let offsetX, offsetY = thickness*sin(theta), thickness*cos(theta)
            YX(int(Y+offsetX), int(X+offsetY))
        let offsetPoint(X, Y, theta) = offsetPointByThickness(X, Y, theta, thickness)
        let offsetSegments(segments : list<SpiroSegment>, start, endP, reverse) =
            let newPoints = 
                [for i in start .. endP do
                    let seg = if i=segments.Length-1 then segments.[0] else segments.[i]
                    let angle = if reverse then -Math.PI/2.0 else Math.PI/2.0
                    let lastSeg = segments.[i-1]
                    let norm(x) = if x>Math.PI then x-Math.PI*2.0 else if x<(-Math.PI) then x+Math.PI*2.0 else x
                    let th1, th2 = norm(lastSeg.seg_th + angle), norm(seg.seg_th + angle)
                    let segType = if reverse then 
                                        match seg.Type with
                                        | SpiroPointType.Left -> SpiroPointType.Right
                                        | SpiroPointType.Right -> SpiroPointType.Left
                                        | x -> x
                                   else seg.Type
                    let bend = norm(th2 - th1)
                    if segType = Corner then
                        let tightBend = if reverse then bend > Math.PI/2.0
                                                   else bend < -Math.PI/2.0
                        //if tightBend then
                            //outer bend
                        [(offsetPoint(seg.X, seg.Y, th1), segType);
                         (offsetPoint(seg.X, seg.Y, th2), segType)]
                        //else //inner
                        //    [(offsetPointByThickness(seg.X, seg.Y, th1 + bend/2.0, abs thickness/sin(bend/2.0)), segType)]
                    else                        
                        [(offsetPoint(seg.X, seg.Y, th1 + bend/2.0), segType)]
                ]
            List.collect id newPoints
        let spiroToOffsetElement(spiro : SpiroElement) =
            match spiro with
            | SpiroOpenCurve(scps) ->
                let offsetMidSegments(segments, reverse) =
                    offsetSegments(segments, 1, segments.Length-2, reverse)
                let reverseList list = List.fold (fun acc elem -> elem::acc) [] list
                let startCap(seg : SpiroSegment)=
                    [(offsetPoint(seg.X, seg.Y, seg.seg_th-Math.PI/2.0), Corner);
                     (offsetPoint(seg.X, seg.Y, seg.seg_th+Math.PI/2.0), Corner)]
                let endCap(seg : SpiroSegment, lastSeg : SpiroSegment) = 
                    [(offsetPoint(seg.X, seg.Y, lastSeg.seg_th+Math.PI/2.0), Corner);
                     (offsetPoint(seg.X, seg.Y, lastSeg.seg_th-Math.PI/2.0), Corner)]
                let segments = SpiroNet.Spiro.SpiroCPsToSegments(Array.ofList scps, scps.Length, false)
                if not (isNull segments) then 
                    let points = startCap(segments.[0])
                                 @ offsetMidSegments(List.ofArray segments, false)
                                 @ endCap(segments.[segments.Length-1], segments.[segments.Length-2])
                                 @ reverseList(offsetMidSegments(List.ofArray segments, true))
                    [ClosedCurve(points)]
                else [ClosedCurve([])]
            | SpiroClosedCurve(scps) ->
                let segments = List.ofArray(Spiro.SpiroCPsToSegments(Array.ofList scps, scps.Length, true))
                [ClosedCurve(offsetSegments(segments, 1, segments.Length-1, false));
                 ClosedCurve(offsetSegments(segments, 1, segments.Length-1, true))]
            | SpiroDot(p) -> [Dot(p)]
        List(List.collect spiroToOffsetElement spiros)

    let svgCircle(x, y, r) =
        sprintf "M %d,%d  " (x-r) y +
        sprintf "A %d,%d 0 1,0 %d,%d  " r r (x+r) y +
        sprintf "A %d,%d 0 1,0 %d,%d  " r r (x-r) y

    let getSvgCurves(spiros : list<SpiroElement>, offsetX, offsetY,
                              strokeWidth, filled, fillrule, thickness) =
        let toSvgBezierCurve(spiro : SpiroElement) : string = 
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
                             svgCircle(x+int(offsetX), y+int(offsetY), thickness)
        let svg = spiros |> List.map toSvgBezierCurve |> String.concat "\n"
        let fillStyle = if filled then "#000000" else "none"
        sprintf "\n<path d='%s' \nstyle='fill:%s;fill-rule:%s;stroke:#000000;stroke-width:%d' />" svg fillStyle fillrule strokeWidth

    let getSvgPoints(spiros : list<SpiroElement>, offsetX : float, offsetY : float) : string =
        let toSvgPoints(spiro : SpiroElement) : string = 
            let point(x,y) = svgCircle(int(x+offsetX), int(y+offsetY), 50)
            match spiro with
            | SpiroOpenCurve(scps) -> scps |> List.map (fun scp -> point(scp.X, scp.Y)) |> String.concat "\n"
            | SpiroClosedCurve(scps) -> scps |> List.map (fun scp -> point(scp.X, scp.Y)) |> String.concat "\n"
            | SpiroDot(p) -> let x,y = getXY(p) in point(float(x), float(y))
        let svg = spiros |> List.map toSvgPoints |> String.concat "\n"
        sprintf "\n<path d='%s' \nstyle='fill:none;stroke:#ff0000;stroke-width:10' />" svg  // small red circles

    //end module


let toSvgDocument rows cols path =
    sprintf "<svg xmlns='http://www.w3.org/2000/svg' \
        viewBox='0 0 %d %d'> \
      <g id='layer1' transform='scale(1,-1) translate(0,%d)'> \n %s \n</g> \
    </svg>" (cols*700) (rows*1300) (-rows*1300-500) path

[<EntryPoint>]
let main argv =
    //let chars = [ for c in 'A'..'Z' -> c ] @ [ for c in  'a'..'z' -> c ]
    let chars = "THE QUICK BROWN FOX JUMPS OVER  THE LAZY DOG the quick brown fox jumps over the lazy dog"
    //let chars = "The truth is in there,  don't let it out"
    //let chars = "Q"
    let cols = 16
    let rows = (chars.Length - 1)/cols + 1
    let outlines = true
    let filled = true
    let debug = false
    let points = true
    let rowHeight = 1200
    let thickness = 70
    printfn "charsPerRow: %d, rows: %d" cols rows |> ignore
    let charToSvg(ch, offsetX, offsetY) =
        let spiros = glyphs.elementToSpiros(glyphs.Glyph(ch))
        let path = glyphs.getSvgCurves(spiros, offsetX, offsetY, 20, false, "", thickness)
        if outlines then
            let fillrule = match spiros.[0] with
                            | glyphs.SpiroClosedCurve(_) -> "evenodd"
                            | _ -> "nonzero"
            let segments = glyphs.getOutlines(glyphs.Glyph(ch), float(thickness))
            if debug then
                printfn "%s" (spiros.ToString())
                printfn "%s" (segments.ToString())
            let spiros = glyphs.elementToSpiros(segments)
            glyphs.getSvgCurves(spiros, offsetX, offsetY, 5, filled, fillrule, thickness) +
                (if filled then "" else path) +
                (if points then glyphs.getSvgPoints(spiros, offsetX, offsetY) else "")
        else path + 
             (if points then glyphs.getSvgPoints(spiros, offsetX, offsetY) else "")
    let svg = [
        for r in 0 .. rows-1 do
            let rowChars = [for i in r*cols .. min ((r+1)*cols-1) (chars.Length-1) do chars.[i]]
            let widths = [for ch in rowChars do glyphs.width(glyphs.Glyph(ch))]
            let offsetXs = List.scan (fun a e -> a+e+200) 0 widths
            for c in 0 .. cols-1 do
                let i = r*cols + c
                if i <= chars.Length - 1 then
                    printfn "%c" rowChars.[c]
                    yield charToSvg(rowChars.[c], float(offsetXs.[c]), float((rows-r)*rowHeight))
    ]
    printfn("Writing test.svg")
    let svg = String.Join("\n\n", svg) |> (toSvgDocument rows cols)
    File.WriteAllText(@".\test.svg", svg) |> ignore
    0 // return an integer exit code
