// Functional Font by terryspitz
// Mar 2020

open System
open SpiroNet
open System.IO

module glyphs =
    // Variables
    let L = 0       // Left
    let R = 600     // Right = standard glyph width
    let N = R*3/4   // Narrow glyph width
    let C = R / 2   // Centre
    let B = 0       // Bottom
    let X = 600     // x-height
    let M = X/2     // Midway down from x-height
    let T = 1000    // Top = standard glyph caps height
    let H = T/2     // Half total height
    let D = -500    // descender height
    let offset = 150  // offset from corners
    let thickness = 150.0

    type Point =
        // Raw coordinates
        | XY of x: int * y: int

        // Y coordinate: Top,X-height,Half-height,Bottom
        // X coordinate: Left,Centre,Right
        // o adds/subtracts an offset to the dimension it follows
        | TL | TLo | TC | TR        // Top points: Left, Left offset inward, Centre, Right
        | ToL | ToC | ToR           // Top offset down
        | XL | XC | XR              // x-height
        | XoL | XoC | XoR           // x-height offset down
        | ML | MC | MR              // Midway down from x-height
        | HL | HLo | HC | HR        // half glyph height
        | HoR                       // half offset down
        | BoL | BoC | BoR           // Bottom offset up
        | BL | BLo | BC | BRo | BR  // Bottom
        | DoL                       // Descender offset up
        | DL | DC | DR              // Descender
        | Mid of p1 : Point * p2 : Point
    
    let rec rewritePoint(p : Point) : Point = 
        match p with
        | XY(x,y) -> XY(x, y)
        | TL -> XY(L, T) | TLo -> XY(L+offset, T) | TC -> XY(C, T) | TR -> XY(R, T)
        | ToL -> XY(L, T-offset) | ToC -> XY(C, T-offset) | ToR -> XY(R, T-offset)
        | XL -> XY(L, X) | XC -> XY(C, X) | XR -> XY(R, X)
        | XoL -> XY(L, X-offset) | XoC -> XY(C, X-offset) | XoR -> XY(R, X-offset)
        | ML -> XY(L, M) | MC -> XY(C, M) | MR -> XY(R, M)
        | HL -> XY(L, H) | HLo -> XY(L+offset, H) | HC -> XY(C, H) | HR -> XY(R, H)
        | HoR -> XY(R, H-offset)
        | BoL -> XY(L, B+offset) | BoC -> XY(C, B+offset) | BoR -> XY(R, B+offset)
        | BL -> XY(L, B) | BLo -> XY(L+offset, B) | BC -> XY(C, B) | BRo -> XY(R-offset, B) | BR -> XY(R, B)
        | DoL -> XY(L, D+offset)
        | DL -> XY(L, D) | DC -> XY(C, D) | DR -> XY(R, D)
        | Mid(p1, p2) -> let x1, y1 = getXY(p1) in let x2, y2 = getXY(p2)
                         XY((x1+x2)/2, (y1+y2)/2)
    and getXY(p : Point) : int * int =
        match rewritePoint(p) with 
            XY(x, y) -> (x, y)

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
        | Glyph('A') -> List([PolyLine([BL; TC; BR]); Line(XY(L,T-X), XY(R,T-X))])
        | Glyph('a') -> List([Line(XR, BR); OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End)])])
        | Glyph('B') -> List([Glyph('P'); OpenCurve([(HL, Corner); (HC, LineToCurve); (Mid(HR, BR), G2); (BC, CurveToLine); (BL, End)])])
        | Glyph('b') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, G2); (BC, G2); (BoL, End)])])
        | Glyph('C') -> OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)])
        //| Glyph('c') -> Scale(OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)]), float(X)/float(T)));
        | Glyph('c') -> OpenCurve([(XoR, Start); (XC, G2); (ML, G2); (BC, G2); (BoR, End)])
        | Glyph('D') -> List([Line(BL, TL); OpenCurve([(TL, Corner); (TLo, LineToCurve); (HR, G2); (BLo, CurveToLine); (BL, End)])])
        | Glyph('d') -> List([Line(BR, TR); OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End)])]) // or flip b
        | Glyph('E') -> List([PolyLine([TR; TL; BL; BR]); Line(HL, HR)])
        | Glyph('e') -> List([OpenCurve([(ML, Start); (MR, Corner); (XC, G2); (ML, G2); (BC, G2); (BoR, End)])])
        | Glyph('F') -> List([PolyLine([TR; TL; BL]); Line(HL, HR)])
        | Glyph('f') -> List([OpenCurve([(TC, Start); (XL, CurveToLine); (BL, End)]); Line(HL, HC)])
        | Glyph('G') -> OpenCurve([(ToR, G2); (TC, G2); (HL, G2); (BC, G2); (Mid(BR,HR), CurveToLine); (HR, Corner); (HC, End)])
        | Glyph('g') -> List([OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End)]);
                              OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (Mid(BL, DL), End)])])
        | Glyph('H') -> List([Line(BL, TL); Line(HL, HR); Line(BR, TR)])
        | Glyph('h') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, CurveToLine); (BR, End)])])
        | Glyph('I') -> Line(BC, TC)
        | Glyph('i') -> List([Line(XC, BC)
                              Dot(Mid(XC, TC))])
        | Glyph('J') -> List([Line(TL, XY(R+offset,T))
                              OpenCurve([(TR, Corner); (HR, LineToCurve); (BC, G2); (BoL, End)])])
        | Glyph('j') -> List([OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (DoL, End)])
                              Dot(Mid(XR, TR))])
        | Glyph('K') -> List([Line(TL, BL); PolyLine([TR; HL; BR])])
        | Glyph('k') -> List([Line(TL, BL); PolyLine([XY(N,X); ML; XY(N,B)])])
        | Glyph('L') -> PolyLine([TL; BL; BR])
        | Glyph('l') -> OpenCurve([(TL, Corner); (ML, LineToCurve); (BC, G2)])
        | Glyph('M') -> PolyLine([BL; TL; BR; XY(R+R,T); XY(R+R,B)])
        | Glyph('m') -> List([Line(BL,XL); OpenCurve([(XoL, Start); (Mid(XL,XC), G2); (XoC, CurveToLine); (BC, End)]);
                              OpenCurve([(XoC, Start); (Mid(XC,XR), G2); (XoR, CurveToLine); (BR, End)])])
        | Glyph('N') -> PolyLine([BL; TL; BR; TR])
        | Glyph('n') -> List([Line(XL,BL)
                              OpenCurve([(XoL, Start); (XC, G2); (MR, CurveToLine); (BR, End)])])
        | Glyph('O') -> ClosedCurve([(TC, G2); (HL, G2); (BC, G2); (HR, G2)])
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
                              OpenCurve([(XoL, Start); (XC, G2); (XoR, End)])])
        | Glyph('S') -> OpenCurve([(ToR, G2); (TC, G2); (Mid(TL,HL), G2); (HC, G2); (Mid(HR,BR), G2); (BC, G2); (BoL, End)])
        | Glyph('s') -> let X2=X/2
                        OpenCurve([(XoR, G2); (XC, G2); (XoL, G2); (XY(C,X2), G2); (BoR, G2); (BC, G2); (BoL, End)])
        | Glyph('T') -> List([Line(TL, TR); Line(TC, BC)])
        | Glyph('t') -> List([Glyph('l'); Line(XL,XC)])
        | Glyph('U') -> OpenCurve([(TL, Corner); (HL, LineToCurve); (BC, G2); (HR, CurveToLine); (TR, End)])
        | Glyph('u') -> List([Line(BR,XR)
                              OpenCurve([(BoR, Start); (BC, G2); (ML, CurveToLine); (XL, End)])])
        | Glyph('V') -> PolyLine([TL; BC; TR])
        | Glyph('v') -> PolyLine([XL; BC; XR])
        | Glyph('W') -> PolyLine([TL; BC; TR; XY(R+R/2,B); XY(R+R,T)])
        | Glyph('w') -> PolyLine([XL; BC; XR; XY(R+R/2,B); XY(R+R,X)])
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

    let rec elementToSpiros(e: Element) : list<SpiroElement> =
        match reduce(e) with
        | OpenCurve(curvePoints) -> [SpiroOpenCurve([
                                        for p, t in curvePoints do
                                            yield SCP(X=float(fst(getXY(p))), Y=float(snd(getXY p)),Type=t)])]
        | ClosedCurve(curvePoints) -> [SpiroClosedCurve([
                                        for p, t in curvePoints do
                                            yield SCP(X=float(fst(getXY(p))), Y=float(snd(getXY(p))),Type=t)]
                                        @ [SCP(X=0.0, Y=0.0, Type=EndClosed)])] // append as per http://libspiro.sourceforge.net/
        | Dot(p) -> [SpiroDot(p)]
        | List(el) -> List.collect elementToSpiros (List.map getGlyph el)

    let getOutlines(e : Element) : Element = 
        //let offsetPoint(x : float, y : float) : Point =
        //    let X = if x < float(L)/2.0 then x/( + thickness
        //            else x
        //    let Y = y
        //    XY(int(X),int(Y))

        let spiros = elementToSpiros(e)
        let offsetPoints(segments : list<SpiroSegment>) : list<Point * Point> =
            [for seg in segments do
                (XY(int(seg.X), int(seg.Y)),
                 XY(int(seg.X), int(seg.Y)))
                 //XY(int(seg.X-float(thickness)*cos(seg.seg_th)), int(seg.Y-float(thickness)*sin(seg.seg_th))))
            ]
        let spiroToElements(spiro : SpiroElement) : list<Element> =
            match spiro with
            | SpiroOpenCurve(scps) ->
                let segments = SpiroNet.Spiro.SpiroCPsToSegments(Array.ofList scps, scps.Length, false)
                let points = offsetPoints(List.ofArray segments)
                let outer, inner = List.map fst points, List.map snd points
                let types = Array.map (fun (s : SpiroSegment) -> s.Type) segments
                [OpenCurve(List.ofSeq(Seq.zip (Seq.ofList inner) types));
                        OpenCurve(List.ofSeq(Seq.zip (Seq.ofList outer) types))]
            | SpiroClosedCurve(scps) ->
                let segments = SpiroNet.Spiro.SpiroCPsToSegments(Array.ofList scps, scps.Length-1, true)
                let points = offsetPoints(List.ofArray segments)
                let outer, inner = List.map fst points, List.map snd points
                let types = Array.map (fun (s : SpiroSegment) -> s.Type) segments
                [OpenCurve(List.ofSeq(Seq.zip (Seq.ofList inner) types));
                        OpenCurve(List.ofSeq(Seq.zip (Seq.ofList outer) types))]
            | SpiroDot(p) -> [Dot(p)]
        List(List.collect spiroToElements spiros)

    let getSvg(spiros : list<SpiroElement>, offsetX : float, offsetY : float) : string =
        let toSvg(spiro : SpiroElement) : string = 
            match spiro with
            | SpiroOpenCurve(scps) ->
                let bc = SpiroNet.Editor.PathBezierContext()
                let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList scps, scps.Length, false, bc, offsetX, offsetY)
                bc.ToString()
            | SpiroClosedCurve(scps) ->
                let bc = SpiroNet.Editor.PathBezierContext()
                let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList scps, scps.Length-1, true, bc, offsetX, offsetY)
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

let toSvgXml(svg: string) =
    "<svg xmlns='http://www.w3.org/2000/svg' \
        viewBox='0 0 8000 7000' > \
      <g id='layer1'>" +
        "<path d='" + svg + "' " +
            "style='fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:50' \
             transform='scale(1,-1) translate(0,-6900)' \
         />
      </g> \
    </svg>" 

[<EntryPoint>]
let main argv =
    let chars = [ for c in 'A'..'Z' -> c ] @ [ for c in  'a'..'z' -> c ]
    let charToSvg(c : char, i : int) : string =
        let offsetX, offsetY = float((i%12)*800), float(5500-(i/12)*1100)
        let segments = glyphs.getOutlines(glyphs.Glyph(c))
        let spiros = glyphs.elementToSpiros(segments)
        glyphs.getSvg(spiros, offsetX, offsetY)
    let svg = [
        for i in 0 .. (chars.Length - 1) do
            let c = chars.[i]
            printfn "%c" c
            yield charToSvg(c, i)
    ]
    printfn("Writing test.svg")
    File.WriteAllText(@".\test.svg", toSvgXml(String.Join("\n\n", svg))) |> ignore
    0 // return an integer exit code
