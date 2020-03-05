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

    type Point =
        | TL | TC | TR
        | ToL | ToC | ToR
        | XL | XC | XR
        | HL | HC | HR
        | XoR
        | BoR
        | BL | BC | BR
        | Mid of p1 : Point * p2 : Point
    
    let rec rewritePoint(p : Point) : int * int = 
        let L = 0
        let R = 600
        let C = R / 2
        let B = 0
        let X = 600 // x-height
        let T = 1000
        let H = T/2 // half
        let offset = 100

        match p with
        | TL -> (L, T) | TC -> (C, T) | TR -> (R, T)
        | ToL -> (L, T-offset) | ToC -> (C, T-offset) | ToR -> (R, T-offset)
        | XL -> (L, X) | XC -> (C, X) | XR -> (R, X)
        | XoR -> (R, X-offset)
        | HL -> (L, H) | HC -> (C, H) | HR -> (R, H)
        | BoR -> (R, B+offset)
        | BL -> (L, B) | BC -> (C, B) | BR -> (R, B)
        | Mid(p1, p2) -> ((fst(rewritePoint(p1))+fst(rewritePoint(p2)))/2,
                          (snd(rewritePoint(p1))+snd(rewritePoint(p2)))/2)

    type Element = 
        | Glyph of c : char
        | Line of p1: Point * p2: Point
        | PolyLine of list<Point>
        | OpenCurve of list<Point * SpiroNet.SpiroPointType>
        | ClosedCurve of list<SCP>
        | LeftUprightFull
        | RightUpright
        | TopCircle
        | BottomCircle
        | TopDot
        | BottomDot
        | Multi of e1 : Element * e2 : Element

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
        | LeftUprightFull -> rewriteElement(Line(BL, TL))
        //| RightUpright
        //| TopCircle
        //| BottomCircle
        //| TopDot
        //| BottomDot
        | Multi(e1, e2) -> rewriteElement(e1) @ rewriteElement(e2)

        | Glyph('A') -> re(PolyLine([BL; TC; BR])) @ re(Line(HL, HR))
        | Glyph('a') -> re(Line(XR, BR)) @
                        re(OpenCurve([(XoR, Start); (XC, G2); (Mid(XL, BL), G2); (BC, G2); (BoR, End);] ))
        | Glyph('P') -> re(LeftUprightFull) @ 
                        re(OpenCurve([(TL, Corner); (TC, G2); (Mid(TR, HR), G2); (HC, LeftCurve); (HL, End)]))
        | Glyph('B') -> re(Glyph('P')) @ re(OpenCurve([(HL, Corner); (HC, G2); (Mid(HR, BR), G2); (BC, LeftCurve); (BL, End)]))
        | LeftUprightFull -> re(Line(BL, XL))

        //| 'a' -> [
        //    Line("backbone", [(w,0.0); (w,xh)])
        //    Spiro("curve", [
        //        SCP(X=w, Y=xh*0.8, Type=G2)
        //        SCP(X=w*0.7, Y=xh, Type=G2)
        //        SCP(X=0.0, Y=xh*0.5, Type=G2)
        //        SCP(X=w*0.6, Y=0.0, Type=G2)
        //        SCP(X=w, Y=xh*0.2, Type=End)
        //    ])
        //]

let toSvgXml(svg: string) =
    "<svg xmlns='http://www.w3.org/2000/svg' \
        viewBox='0 0 2000 3000' \
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
        let chars = "AaPB"
        for i in 0 .. (chars.Length - 1) do
            let c = chars.[i]
            printfn
            printfn "%c" c
            let ell = glyphs.rewriteElement(glyphs.Glyph(c))
            let svg : list<string> = [
                for el in ell do
                    let segments = SpiroNet.Spiro.SpiroCPsToSegments(Array.ofList el, el.Length, isClosed=false)
                    printfn "%A" (el, segments)
                    let bc = SpiroNet.Editor.PathBezierContext()
                    let success = SpiroNet.Spiro.SpiroCPsToBezier(Array.ofList el, el.Length, false, bc, float(i*650), 0.0)
                    yield bc.ToString()]
            yield String.Join("\n", svg)
    ]
    printfn("Writing test.svg")
    File.WriteAllText(@".\test.svg", toSvgXml(String.Join("\n\n", svg))) |> ignore
    0 // return an integer exit code
