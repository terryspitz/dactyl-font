// Functional Font by terryspitz
// Mar 2020

open System
open SpiroNet
open System.IO

module glyphs =
    let w = 500
    let h = 1000
    let xh = 600
        
    type SCP = SpiroNet.SpiroControlPoint
    let LeftCurve = SpiroNet.SpiroPointType.Left
    let RightCurve = SpiroNet.SpiroPointType.Right
    let G2 = SpiroNet.SpiroPointType.G2
    let G4 = SpiroNet.SpiroPointType.G4
    let Start = SpiroNet.SpiroPointType.OpenContour
    let Corner = SpiroNet.SpiroPointType.Corner
    let End = SpiroNet.SpiroPointType.EndOpenContour

    let L = 0
    let R = 600
    let C = R / 2
    let B = 0
    let X = 600 // x-height
    let T = 1000
    let H = T/2 // half
    let offset = 100

    type Point =
        // Y coordinate: Top,X-height,Half-height,Bottom
        // X coordinate: Left,Centre,Right
        // o adds/subtracts an offset to the dimension it follows
        | TL | TLo | TC | TR
        | ToL | ToC | ToR
        | XL | XC | XR
        | XoL | XoR
        | HL | HLo | HC | HR
        | HoR
        | BoL | BoR
        | BL | BLo | BC | BR
        | XY of int * int
        | Mid of p1 : Point * p2 : Point
    
    let rec rewritePoint(p : Point) : int * int = 

        match p with
        | TL -> (L, T) | TLo -> (L+offset, T) | TC -> (C, T) | TR -> (R, T)
        | ToL -> (L, T-offset) | ToC -> (C, T-offset) | ToR -> (R, T-offset)
        | XL -> (L, X) | XC -> (C, X) | XR -> (R, X)
        | XoL -> (L, X-offset) | XoR -> (R, X-offset)
        | HL -> (L, H) | HLo -> (L+offset, H) | HC -> (C, H) | HR -> (R, H)
        | HoR -> (R, H-offset)
        | BoL -> (L, B+offset) | BoR -> (R, B+offset)
        | BL -> (L, B) | BLo -> (L+offset, B) | BC -> (C, B) | BR -> (R, B)
        | XY(x,y) -> (x, y)
        | Mid(p1, p2) -> ((fst(rewritePoint(p1))+fst(rewritePoint(p2)))/2,
                          (snd(rewritePoint(p1))+snd(rewritePoint(p2)))/2)

    type Element = 
        | Glyph of c : char
        | Line of p1: Point * p2: Point
        | PolyLine of list<Point>
        | OpenCurve of list<Point * SpiroNet.SpiroPointType>
        | ClosedCurve of list<SCP>
        | Scale of Element * float

    //Straights: AEFHIKLMNTVWXYZklvwxyz147/=[]\`|*"'
    //Dots: ij:;!?
    //Curves: COScos36890()~
    //LeftUpright: BDPRb mnpr 
    //RightUpright: GJadgq
    //Other: QUefhtu25@#$€£_&-+{}%

    let rec rewriteElement (e: Element) : list<list<SCP>> =
        let re = rewriteElement
        match e with
        | Line(p1, p2) -> [[SCP(X=float(fst(rewritePoint(p1))), Y=float(snd(rewritePoint(p1))),Type=Start);
                            SCP(X=float(fst(rewritePoint(p2))), Y=float(snd(rewritePoint(p2))),Type=End);]]
        | PolyLine(points) -> let a = Array.ofList points
                              [[for i in 0 .. a.Length-1 do
                                let p = a.[i]
                                yield SCP(X=float(fst(rewritePoint(p))), Y=float(snd(rewritePoint(p))),
                                          Type=if i=(a.Length-1) then End else Corner)]]
        | OpenCurve(curvePoints) -> [List.map (fun ((p: Point), (t: SpiroPointType))
                                                    -> SCP(X=float(fst(rewritePoint(p))), Y=float(snd(rewritePoint(p))),Type=t))
                                              curvePoints]
        //| ClosedCurve(points) -> [[]]
        | Scale(e, s) -> [ for el in re(e) do yield [ for scp in el do yield SCP(X=scp.X*s, Y=scp.Y*s, Type=scp.Type)]]

        | Glyph('A') -> re(PolyLine([BL; TC; BR])) @ re(Line(XY(L,T-X), XY(R,T-X)))
        | Glyph('a') -> re(Line(XR, BR)) @
                        re(OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End);] ))
        | Glyph('B') -> re(Glyph('P')) @ re(OpenCurve([(HL, Corner); (HC, RightCurve); (Mid(HR, BR), G2); (BC, LeftCurve); (BL, End)]))
        | Glyph('b') -> re(Line(BL, TL)) @ re(OpenCurve([(XoL, Start); (XC, G2); (Mid(XR, BR), G2); (BC, G2); (BoL, End)]))
        | Glyph('C') -> re(OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)]));
        //| Glyph('c') -> re(Scale(OpenCurve([(ToR, Start); (TC, G2); (HL, G2); (BC, G2); (BoR, End)]), float(X)/float(T)));
        | Glyph('c') -> re(OpenCurve([(XoR, Start); (XC, G2); (Mid(XL,BL), G2); (BC, G2); (BoR, End)]));
        | Glyph('D') -> re(Line(BL, TL)) @ 
                        re(OpenCurve([(TL, Corner); (TLo, RightCurve); (HR, G2); (BLo, LeftCurve); (BL, End)]))
        | Glyph('d') -> re(Line(BR, TR)) @ re(OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End)])) // or flip b
        | Glyph('E') -> re(PolyLine([TR; TL; BL; BR])) @ re(Line(HL, HR))
        | Glyph('e') -> re(OpenCurve([(Mid(XL,BL), Start); (Mid(XR,BR), Corner); (XC, G2); (Mid(XL,BL), G2); (BC, G2); (BoR, End)]));


        | Glyph('P') -> re(Line(BL, TL)) @ 
                        re(OpenCurve([(TL, Corner); (TC, RightCurve); (Mid(TR, HR), G2); (HC, LeftCurve); (HL, End)]))

let toSvgXml(svg: string) =
    "<svg xmlns='http://www.w3.org/2000/svg' \
        viewBox='0 0 8000 1500' \
    > \
      <g id='layer1'>" +
        "<path d='" + svg + "' " +
            "style='fill:none;fill-rule:evenodd;stroke:#000000;stroke-width:5' \
             transform='scale(1,-1) translate(0,-2900)' \
         />
      </g> \
    </svg>" 

[<EntryPoint>]
let main argv =
    let svg = [
        let chars = "AaBbCcDdEe"
        for i in 0 .. (chars.Length - 1) do
            let c = chars.[i]
            printfn ""
            printfn "%c" c
            let ell = glyphs.rewriteElement(glyphs.Glyph(c))
            let svg : list<string> = [
                for el in ell do
                    let segments = SpiroNet.Spiro.SpiroCPsToSegments(Array.ofList el, el.Length, isClosed=false)
                    printfn "%A" (el, segments)
                    let bc = SpiroNet.Editor.PathBezierContext()
                    let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList el, el.Length, false, bc, float(i*700), 0.0)
                    yield bc.ToString()]
            yield String.Join("\n", svg)
    ]
    printfn("Writing test.svg")
    File.WriteAllText(@".\test.svg", toSvgXml(String.Join("\n\n", svg))) |> ignore
    0 // return an integer exit code
