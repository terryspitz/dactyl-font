// Functional Font by terryspitz
// Mar 2020

open System
open SpiroNet
open System.IO

module glyphs =
    let L = 0
    let R = 600 // standard glyph width
    let C = R / 2
    let B = 0
    let X = 600 // x-height
    let T = 1000 // standard glyph caps height
    let H = T/2 // half
    let D = -500 // descender height
    let offset = 100

    type Point =
        // Raw coordinates
        | XY of x: int * y: int

        // Y coordinate: Top,X-height,Half-height,Bottom
        // X coordinate: Left,Centre,Right
        // o adds/subtracts an offset to the dimension it follows
        | TL | TLo | TC | TR  // Top points: Left, Left offset inward, Centre, Right
        | ToL | ToC | ToR     // Top offset down
        | XL | XC | XR        // x-height
        | XoL | XoR           // x-height offset down
        | HL | HLo | HC | HR  // half glyph height
        | HoR                 // helf offset down
        | BoL | BoR           // Bottom offset up
        | BL | BLo | BC | BR  // Bottom
        | DoL                 // Descender offset up
        | DL | DC | DR        // Descender
        | Mid of p1 : Point * p2 : Point
    
    let rec rewritePoint(p : Point) : Point = 
        match p with
        | XY(x,y) -> XY(x, y)
        | TL -> XY(L, T) | TLo -> XY(L+offset, T) | TC -> XY(C, T) | TR -> XY(R, T)
        | ToL -> XY(L, T-offset) | ToC -> XY(C, T-offset) | ToR -> XY(R, T-offset)
        | XL -> XY(L, X) | XC -> XY(C, X) | XR -> XY(R, X)
        | XoL -> XY(L, X-offset) | XoR -> XY(R, X-offset)
        | HL -> XY(L, H) | HLo -> XY(L+offset, H) | HC -> XY(C, H) | HR -> XY(R, H)
        | HoR -> XY(R, H-offset)
        | BoL -> XY(L, B+offset) | BoR -> XY(R, B+offset)
        | BL -> XY(L, B) | BLo -> XY(L+offset, B) | BC -> XY(C, B) | BR -> XY(R, B)
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
        | Scale of Element * float
        | List of list<Element>

    //Straights: AEFHIKLMNTVWXYZklvwxyz147/=[]\`|*"'
    //Dots: ij:;!?
    //Curves: COScos36890()~
    //LeftUpright: BDPRb mnpr 
    //RightUpright: GJadgq
    //Other: QUefhtu25@#$€£_&-+{}%

    let rec rewriteElement (e: Element) : Element =
        match e with
        | Line(p1, p2) -> OpenCurve([(p1, Start); (p2, End)])
        | PolyLine(points) -> let a = Array.ofList points
                              OpenCurve([for i in 0 .. a.Length-1 do
                                         let p = a.[i]
                                         yield (p, if i=(a.Length-1) then End else Corner)])

        | Glyph('A') -> List([PolyLine([BL; TC; BR]); Line(XY(L,T-X), XY(R,T-X))])
        | Glyph('a') -> List([Line(XR, BR); OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End)])])
        | Glyph('B') -> List([Glyph('P'); OpenCurve([(HL, Corner); (HC, LineToCurve); (Mid(HR, BR), G2); (BC, CurveToLine); (BL, End)])])
        | Glyph('b') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (Mid(XR, BR), G2); (BC, G2); (BoL, End)])])
        | Glyph('C') -> OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)])
        //| Glyph('c') -> Scale(OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)]), float(X)/float(T)));
        | Glyph('c') -> OpenCurve([(XoR, Start); (XC, G2); (Mid(XL,BL), G2); (BC, G2); (BoR, End)])
        | Glyph('D') -> List([Line(BL, TL); OpenCurve([(TL, Corner); (TLo, LineToCurve); (HR, G2); (BLo, CurveToLine); (BL, End)])])
        | Glyph('d') -> List([Line(BR, TR); OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End)])]) // or flip b
        | Glyph('E') -> List([PolyLine([TR; TL; BL; BR]); Line(HL, HR)])
        | Glyph('e') -> List([OpenCurve([(Mid(XL,BL), Start); (Mid(XR,BR), Corner); (XC, G2); (Mid(XL,BL), G2); (BC, G2); (BoR, End)])])
        | Glyph('F') -> List([PolyLine([TR; TL; BL]); Line(HL, HR)])
        | Glyph('f') -> List([OpenCurve([(TC, Start); (XL, CurveToLine); (BL, End)]); Line(HL, HC)])
        | Glyph('G') -> OpenCurve([(ToR, G2); (TC, G2); (HL, G2); (BC, G2); (Mid(BR,HR), CurveToLine); (HR, Corner); (HC, End)])
        | Glyph('g') -> List([OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End)]);
                              OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (Mid(BL, DL), End)])])
        | Glyph('H') -> List([Line(BL, TL); Line(HL, HR); Line(BR, TR)])
        | Glyph('h') -> List([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (Mid(XR,BR), CurveToLine); (BR, End)])])
        | Glyph('I') -> Line(BC, TC)
        | Glyph('i') -> List([Line(XC, BC); Line(Mid(XC, TC), Mid(XY(C,X+50), TC))])


        | Glyph('P') -> List([Line(BL, TL); OpenCurve([(TL, Corner); (TC, LineToCurve); (Mid(TR, HR), G2); (HC, CurveToLine); (HL, End)])])
        | Glyph('p') -> List([Line(DL, XL); OpenCurve([(XL, Corner); (XC, LineToCurve); (Mid(XR, BR), G2); (BC, CurveToLine); (BL, End)])])
        | Glyph('O') -> ClosedCurve([(TC, G2); (HL, G2); (BC, G2); (HR, G2)])
        | Glyph('o') -> ClosedCurve([(XC, G2); (Mid(XL,BL), G2); (BC, G2); (Mid(HR,BR), G2)])
        | Glyph('Z') -> PolyLine([TL; TR; BL; BR])
        | Glyph('z') -> PolyLine([XL; XR; BL; BR])
        //default
        | Glyph(c) -> printfn "Glyph %c not defined" c
                      OpenCurve([])
        | any -> any

    and elementToSpiros(e: Element) : list<bool * list<SCP>> =
        match e with
        | OpenCurve(curvePoints) -> [(true, [ for p, t in curvePoints do
                                              yield SCP(X=float(fst(getXY(p))), Y=float(snd(getXY(p))),Type=t)])]
        | ClosedCurve(curvePoints) -> [(false, [ for p, t in curvePoints do
                                                 yield SCP(X=float(fst(getXY(p))), Y=float(snd(getXY(p))),Type=t)]
                                               @ [SCP(X=0.0, Y=0.0, Type=EndClosed)])] // append as per http://libspiro.sourceforge.net/
        | List(el) -> List.collect elementToSpiros (List.map rewriteElement el)
        //| Scale(e, s) -> [ for el in e do yield [ for scp in el do yield SCP(X=scp.X*s, Y=scp.Y*s, Type=scp.Type)]]
        | e -> elementToSpiros(rewriteElement(e))

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
    let svg = [
        let chars = [ for c in 'A'..'Z' -> c ] @ [ for c in  'a'..'z' -> c ]
        for i in 0 .. (chars.Length - 1) do
            let c = chars.[i]
            printfn ""
            printfn "%c" c
            let ell = glyphs.elementToSpiros(glyphs.Glyph(c))
            let svg : list<string> = [
                for isOpen, el in ell do
                    let n = if isOpen then el.Length else el.Length-1
                    let segments = SpiroNet.Spiro.SpiroCPsToSegments(Array.ofList el, n, not isOpen)
                    printfn "%A" (el, segments)
                    let bc = SpiroNet.Editor.PathBezierContext()
                    let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList el, n, not isOpen, bc, float((i%12)*700), float(5500-(i/12)*1100))
                    yield bc.ToString()]
            yield String.Join("\n", svg)
    ]
    printfn("Writing test.svg")
    File.WriteAllText(@".\test.svg", toSvgXml(String.Join("\n\n", svg))) |> ignore
    0 // return an integer exit code
