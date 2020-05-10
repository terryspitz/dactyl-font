module Explorer

open Browser.Dom
open Generator
open Microsoft.FSharp.Reflection
open Browser.Types

let textbox = document.getElementById("text") :?> HTMLInputElement
let inputs = document.getElementById("inputs")
let output = document.getElementById("output")

let axisFields = FSharpType.GetRecordFields (typeof<Generator.Axes>)
let fieldDefaults = [
    for propinfo in axisFields do
        propinfo.Name, Reflection.FSharpValue.GetRecordField(Axes.DefaultAxes, propinfo)
    ]
let fieldDefaultsMap = Map.ofList fieldDefaults 

//Not supported in Fable :()
//let axesConstructor = Reflection.FSharpValue.PreComputeRecordConstructor typeof<Axes> 

type Controls = 
    | Range of from : int * upto : int
    | FracRange of from : float * upto : float
    | Checkbox

let controls = Map.ofList [
    "width", Range(100, 1000);
    "height", Range(100, 1000);
    "x_height", Range(0, 1000);
    "offset", Range(0, 500);
    "thickness", Range(0, 200);
    "leading", Range(0, 200);
    "italic_fraction", FracRange(0.0, 1.0);
    "outline", Checkbox;
    "stroked", Checkbox;
    "scratches", Checkbox;
    "filled", Checkbox;
    "show_knots", Checkbox
]

let generate _ = 
    let text = textbox.value
    let axes = Reflection.FSharpValue.MakeRecord(typeof<Axes>,
                [|for k,_ in fieldDefaults do
                     let input = document.getElementById k  :?> HTMLInputElement
                     let c = controls.[k]
                     match c with
                     | Range(_,_) -> System.Int32.Parse input.value :> obj
                     | FracRange(_,_) -> System.Single.Parse input.value :> obj
                     | Checkbox -> input.``checked`` :> obj
                 |])
    let font = Font(axes :?> Axes)
    printfn "%A" font.Axes     
    let yOffset = font.charHeight - font.yBaselineOffset + font.Axes.thickness + 10
    let svg = font.stringToSvg text 0 yOffset |> toSvgDocument (int (float font.charHeight*1.25)) (font.stringWidth text)
    output.innerHTML <- svg

let font = Font(Axes.DefaultAxes)
let yOffset = font.charHeight - font.yBaselineOffset + font.Axes.thickness + 10
let title = document.getElementById "title"
title.innerHTML <- font.stringToSvg "Dactyl  Live" 0 yOffset |> toSvgDocument (font.charHeight+10) 2000

for k,_ in fieldDefaults do
    let label = document.createElement "label" :?> HTMLLabelElement
    label.htmlFor <- k
    label.innerText <- (k + "  ")
    inputs.appendChild label |> ignore
    let input = document.createElement "input" :?> HTMLInputElement
    input.id <- k
    input.oninput <- generate
    let c = controls.[k]
    match c with
    | Range(x, y) -> 
        input.``type`` <- "range"
        input.min <- string x
        input.max <- string y 
        input.step <- string ((y-x)/20)
        input.value <- string fieldDefaultsMap.[k]
    | FracRange(x, y) ->
        input.``type`` <- "range"
        input.min <- string x
        input.max <- string y
        input.step <- "0.05" 
        input.value <- string fieldDefaultsMap.[k]
    | Checkbox ->
        input.``type`` <- "checkbox"
        input.``checked`` <- fieldDefaultsMap.[k] :?> bool
    inputs.appendChild input |> ignore

textbox.oninput <- generate
generate()
