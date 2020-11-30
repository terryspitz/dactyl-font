module GlyphDefs

open GeneratorTypes
open Axes


let rec movePoints fn e = 
    match e with
    | OpenCurve(pts) -> OpenCurve([for p, t in pts do (fn p, t)])
    | ClosedCurve(pts) -> ClosedCurve([for p, t in pts do (fn p, t)])
    | TangentCurve(pts, isClosed) -> TangentCurve([for p, ty, tang in pts do (fn p, ty, tang)], isClosed)
    | Dot(p) -> Dot(fn p)
    | EList(elems) -> EList(List.map (movePoints fn) elems)
    | Space -> Space
    | _ -> invalidArg "e" (sprintf "Unreduced element %A" e) 

let applyIf b f = if b then f else id


///Class defining important Font x, y points, and a set of Glyph definitions in code
type GlyphDefs (axes: Axes) =

    // X axis guides, from left
    let L = 0               // Left
    let R = axes.width      // Right = standard glyph width
    let N = R * 4/5         // Narrow glyph width
    let C = R / 2           // Centre
    let monospaceWidth = N

    // Y axis guides, from bottom-up
    let B = 0               // Bottom
    let X = int (axes.x_height * float axes.height) // x-height
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
    
    // members ------------------------------------------------------------------------------------

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
        | Glyph(')') -> this.reflect flooredOffset (Glyph('('))
        | Glyph('*') -> let sin30 = int (0.866 * float T / 4.0)
                        EList([Line(YX(T*2/3,L), YX(T*2/3,N)); 
                               Line(YX(T*2/3+sin30,R/5), YX(T*2/3-sin30,N*4/5)); Line(YX(T*2/3+sin30,N*4/5), YX(T*2/3-sin30,R/5))])
        | Glyph('+') -> EList([Line(HL, HR); Line(YX(T-T/4,C), YX(T/4,C))])
        | Glyph('-') -> Line(HL, HN)
        | Glyph('.') -> Dot(BC)
        | Glyph(',') -> Line(BC, YX(B-min thickness 50,C-min thickness 50))
        | Glyph('/') -> this.reflect R (Glyph('\\'))
        | Glyph(':') -> EList([Dot(BC); Dot(YX(T/3,C))])
        | Glyph(';') -> EList([Glyph(','); Dot(YX(T/3,C))])
        | Glyph('<') -> PolyLine([XR; YX(X/2,L); BR])
        | Glyph('=') -> EList([Line(YX(X*2/3,L), YX(X*2/3,N)); Line(YX(X/3,L), YX(X/3,N))])
        | Glyph('>') -> this.reflect R (Glyph('<'))
        | Glyph('?') -> EList([OpenCurve([(YX(T-flooredOffset,L), G2); (TC, G2); (Mid(TR,HR), G2);
                                         (HC, Corner); (YX(T/4,C), End);])
                               Dot(BC)])
        | Glyph('@') -> OpenCurve([(YX(T/3,R*3/4), Corner); (YX(T/4,C), G2); (YX(H,R/4), G2); (YX(T*2/3,C), G2); (YX(H,R*3/4), CurveToLine)
                                   (YX(T/3,R*3/4), LineToCurve); (YX(T/4,R), CurveToLine); (YX(T/2,R), LineToCurve)
                                   (TC,G2); (HL,G2); (BLo,G2); (BR,End)])
        | Glyph('[') -> PolyLine([YX(T+thickness,C); YX(T+thickness,L); YX(B-thickness,L);YX(B-thickness,C);])
        | Glyph('\\') -> Line(YX(T+thickness,L), YX(B-thickness,R))
        | Glyph(']') -> this.reflect C (Glyph('['))
        | Glyph('^') -> PolyLine([YX(T*2/3,L); TC; YX(T*2/3,R)])
        | Glyph('_') -> Line(YX(B-thickness*2,L), YX(B-thickness*2,N))
        | Glyph('`') -> Line(TC, YX(T-min thickness 50,C+min thickness 50))
        | Glyph('{') -> EList([OpenCurve([(YX(T+thickness, N), Start); (YX(T+thickness, N-10), LineToCurve);
                                   (YX(H+flooredOffset, L+flooredOffset/2), G2); (HL, G2); ]);
                               OpenCurve([(YX(B-thickness, N), Start); (YX(B-thickness, N-10), LineToCurve);
                                   (YX(H-flooredOffset, L+flooredOffset/2), G2); (HL, G2); ])])
        | Glyph('|') -> Line(TC,BC)
        | Glyph('}') -> this.reflect N (Glyph('{'))
        | Glyph('~') -> let h = flooredOffset/2
                        OpenCurve([(YX(T-h,L), Start); (YX(T,R/4), G2); (YX(T-h,R/2), G2); (YX(T-h*2,R*3/4), G2); (YX(T-h,R), G2); ])

        | Glyph('0') -> EList([ClosedCurve([(HL, G2); (BC, G2); (HR, G2); (TC, G2)]); Line(TR,BL)])
        | Glyph('1') -> let midX = max (thickness * 2) (int ((float monospaceWidth * axes.monospace) / 2.0))
                        EList([PolyLine([YX(T*4/5,L); YX(T,midX); YX(B, midX)])] @
                              if axes.monospace > 0.0 then [Line(BL, YX(B,midX*2))] else [])
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
        // | Glyph('f') -> EList([OpenCurve([(TC, Anchor); (TL, Handle); (XL, CurveToLine); (BL, End)]); Line(XL, XC)])
        | Glyph('G') -> OpenCurve([(ToR, G2); (TC, G2); (HL, G2); (BC, G2); (YX(H-flooredOffset,R), CurveToLine); (HR, Corner); (HC, End)])
        | Glyph('g') -> EList([OpenCurve([(XR, Corner); (BR, LineToCurve); (DC, G2); (DoL, End)]);
                               adgqLoop;])
        | Glyph('H') -> EList([Line(BL, TL); Line(HL, HR); Line(BR, TR)])
        | Glyph('h') -> EList([Line(BL, TL); OpenCurve([(XoL, Start); (XC, G2); (MR, CurveToLine); (BR, End)])])
        | Glyph('I') -> let midX = int (float axes.width * axes.monospace / 2.0)
                        let vertical = Line(YX(T,midX), YX(B,midX))
                        if axes.monospace > 0.0 then
                            EList([vertical; Line(BL, YX(B,midX*2)); Line(TL, YX(T,midX*2))])
                        else 
                            vertical
        | Glyph('i') -> let midX = int (float axes.width * axes.monospace / 2.0)
                        EList([Line(YX(X,midX), YX(B,midX))
                               Dot(YX(dotHeight,midX))] @
                               if axes.monospace > 0.0 then [Line(BL, YX(B,midX*2)); Line(XL, YX(X,midX))] else [])
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
        | TangentCurve(pts, isClosed) ->
            TangentCurve([for p, t, tang in pts do YX(reducePoint p), t, tang], isClosed)
        | Dot(p) -> Dot(YX(reducePoint(p)))
        | EList(elems) -> EList(List.map this.reduce elems)
        | Space -> Space
        | Glyph(ch) -> this.getGlyph e |> this.reduce
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" e)

    member this.reflect w e =
        let el = this.reduce e
        let reflectP p = 
            let x,y = getXY p
            YX(y, w-x)
        movePoints reflectP el

    member this._axes = {axes with thickness = thickness;}
    member this._T = T
    member this._X = X
    member this._H = H
    member this._B = B
    member this._D = D
    member this._offset = offset
    member this._L = L
    member this._C = C
    member this._R = R
    member this._thickness = thickness
    member this._getXY = getXY
    member this._monospaceWidth = monospaceWidth