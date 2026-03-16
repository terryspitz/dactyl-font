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


let svgCircle x y r =
    [ sprintf "M %d,%d" (x - r) y
      sprintf "C %d,%d %d,%d %d,%d" (x - r) (y + r / 2) (x - r / 2) (y + r) x (y + r)
      sprintf "C %d,%d %d,%d %d,%d" (x + r / 2) (y + r) (x + r) (y + r / 2) (x + r) y
      sprintf "C %d,%d %d,%d %d,%d" (x + r) (y - r / 2) (x + r / 2) (y - r) x (y - r)
      sprintf "C %d,%d %d,%d %d,%d" (x - r / 2) (y - r) (x - r) (y - r / 2) (x - r) y
      "Z" ]

let svgDiamond x y r =
    [ sprintf "M %d,%d" (x - r) y
      sprintf "L %d,%d" x (y + r)
      sprintf "L %d,%d" (x + r) y
      sprintf "L %d,%d" x (y - r)
      sprintf "L %d,%d" (x - r) y
      "Z" ]

let svgSemiCircle x y r (ch: char) =
    assert "udlr".Contains(ch)

    [ if ch = 'u' || ch = 'd' then
          sprintf "M %d,%d" (x - r) y
      else
          sprintf "M %d,%d" x (y - r)
      if ch = 'u' then
          sprintf "C %d,%d %d,%d %d,%d" (x - r) (y + r) (x + r) (y + r) (x + r) y
      elif ch = 'd' then
          sprintf "C %d,%d %d,%d %d,%d" (x - r) (y - r) (x + r) (y - r) (x + r) y
      elif ch = 'l' then
          sprintf "C %d,%d %d,%d %d,%d" (x - r) (y - r) (x - r) (y + r) x (y + r)
      elif ch = 'r' then
          sprintf "C %d,%d %d,%d %d,%d" (x + r) (y - r) (x + r) (y + r) x (y + r)
      "Z" ]

let svgText x y text =
    sprintf "<text x='%d' y='%d' font-size='200'>%s</text>" x y text

let toSvgDocument left bottom width height svg =
    [ "<svg xmlns='http://www.w3.org/2000/svg'"
      sprintf "viewBox='%d %d %d %d'>" left bottom width height
      "<g id='1'>" ]
    @ svg
    @ [ "</g>"; "</svg>" ]

let toHtmlDocument left bottom width height svg =
    [ "<body>"
      //from https://www.cssscript.com/svg-pan-zoom-container/
      "<script src='https://cdn.jsdelivr.net/npm/svg-pan-zoom-container@0.1.2'></script>"
      "<div data-zoom-on-wheel='' data-pan-on-drag=''>" ]
    @ toSvgDocument left bottom width height svg
    @ [ "</div>"; "</body>" ]
