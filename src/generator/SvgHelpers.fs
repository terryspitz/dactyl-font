module SvgHelpers

open System

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
