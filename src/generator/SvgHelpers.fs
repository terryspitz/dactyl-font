module SvgHelpers

open System
open SpiroPointType
open GeneratorTypes

let black = "#000000"
let red = "#e00000"
let green = "#00e000d0"
let lightGreen = "#aaffaa"
let blue = "#0000e0d0"
let lightBlue = "#aaaaff"
let pink = "#ffaaaa"
let orange = "#ffaa00d0"


let svgCircle (x: float) (y: float) (r: float) =
    [ sprintf "M %f,%f" (x - r) y
      sprintf "C %f,%f %f,%f %f,%f" (x - r) (y + r / 2.0) (x - r / 2.0) (y + r) x (y + r)
      sprintf "C %f,%f %f,%f %f,%f" (x + r / 2.0) (y + r) (x + r) (y + r / 2.0) (x + r) y
      sprintf "C %f,%f %f,%f %f,%f" (x + r) (y - r / 2.0) (x + r / 2.0) (y - r) x (y - r)
      sprintf "C %f,%f %f,%f %f,%f" (x - r / 2.0) (y - r) (x - r) (y - r / 2.0) (x - r) y
      "Z" ]

let svgDiamond (x: float) (y: float) (r: float) =
    [ sprintf "M %f,%f" (x - r) y
      sprintf "L %f,%f" x (y + r)
      sprintf "L %f,%f" (x + r) y
      sprintf "L %f,%f" x (y - r)
      sprintf "L %f,%f" (x - r) y
      "Z" ]

let svgSemiCircle (x: float) (y: float) (r: float) (ch: char) =
    assert "udlr".Contains(ch)

    [ if ch = 'u' || ch = 'd' then
          sprintf "M %f,%f" (x - r) y
      else
          sprintf "M %f,%f" x (y - r)
      if ch = 'u' then
          sprintf "C %f,%f %f,%f %f,%f" (x - r) (y + r) (x + r) (y + r) (x + r) y
      elif ch = 'd' then
          sprintf "C %f,%f %f,%f %f,%f" (x - r) (y - r) (x + r) (y - r) (x + r) y
      elif ch = 'l' then
          sprintf "C %f,%f %f,%f %f,%f" (x - r) (y - r) (x - r) (y + r) x (y + r)
      elif ch = 'r' then
          sprintf "C %f,%f %f,%f %f,%f" (x + r) (y - r) (x + r) (y + r) x (y + r)
      "Z" ]

let svgText (x: float) (y: float) text =
    sprintf "<text x='%f' y='%f' font-size='200'>%s</text>" x y text

let toSvgDocument (left: float) (bottom: float) (width: float) (height: float) svg =
    [ "<svg xmlns='http://www.w3.org/2000/svg'"
      sprintf "viewBox='%f %f %f %f'>" left bottom width height
      "<g id='1'>" ]
    @ svg
    @ [ "</g>"; "</svg>" ]

let toHtmlDocument (left: float) (bottom: float) (width: float) (height: float) svg =
    [ "<body>"
      //from https://www.cssscript.com/svg-pan-zoom-container/
      "<script src='https://cdn.jsdelivr.net/npm/svg-pan-zoom-container@0.1.2'></script>"
      "<div data-zoom-on-wheel='' data-pan-on-drag=''>" ]
    @ toSvgDocument left bottom width height svg
    @ [ "</div>"; "</body>" ]

///circles highlighting the knots (defined points on the spiro curves)
let getSvgKnots (offsetX: float) (offsetY: float) (size: float) (colour: string) (isJoint: Element -> float -> float -> bool) (elem: Element) =
    let l, r, b, t = bounds elem

    let rec toSvgPoints (elem2: Element) =
        let svgKnot (p: Point, ty: SpiroPointType) =
            let x, y = p.x, p.y
            let radius = if ty = Handle then 1.0 else size

            if isJoint elem x y then svgDiamond x y radius
            elif x = l then svgSemiCircle x y radius 'r'
            elif x = r then svgSemiCircle x y radius 'l'
            elif y = b then svgSemiCircle x y radius 'u'
            elif y = t then svgSemiCircle x y radius 'd'
            else svgCircle x y radius

        match elem2 with
        | Curve(pts, _) ->
            let svgKnotTangent (k: Knot) = svgKnot (k.pt, k.ty)
            List.collect svgKnotTangent pts
        | Dot(p) -> svgKnot (p, G2)
        | EList(elems) -> List.collect toSvgPoints elems
        | Space -> []
        | _ -> invalidArg "e" (sprintf "Unreduced element %A" elem)
    [ "<!-- knots -->"; "<path d='" ]
    @ toSvgPoints elem
    @ [ "'"
        sprintf "transform='translate(%f,%f) scale(1,-1)'" offsetX offsetY
        sprintf "style='fill:none;stroke:%s;stroke-width:%f'/>" colour 10.0 ]

let getSvgLabels (offsetX: float) (offsetY: float) (elem: Element) =
    let rec collectLabels (e: Element) =
        match e with
        | Curve(knots, _) ->
            knots
            |> List.choose (fun k ->
                k.label
                |> Option.map (fun lbl ->
                    let x = k.pt.x + offsetX
                    let y = offsetY - k.pt.y
                    sprintf "<text x='%f' y='%f' font-family='monospace' font-size='40' fill='red' font-weight='bold'>%s</text>" (x + 15.0) (y - 15.0) lbl))
        | Dot(p) -> [] // Dots usually don't have labels in this context
        | EList(elems) -> List.collect collectLabels elems
        | Space -> []
        | _ -> []

    [ "<!-- labels -->" ] @ collectLabels elem
