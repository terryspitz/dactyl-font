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

let font = Font({Axes.DefaultAxes with thickness=3})
let titleElem = document.getElementById "title"
titleElem.innerHTML <-  font.stringToSvg "Dactyl Live" 0 0 |> String.concat "\n"

let generate _ = 
    let text = textbox.value
    let axes = Reflection.FSharpValue.MakeRecord(typeof<Axes>,
                [|for k,_ in fieldDefaults do
                     let input = document.getElementById k  :?> HTMLInputElement
                     let c = Axes.controls.[k]
                     match c with
                     | Range(_) -> System.Int32.Parse input.value :> obj
                     | FracRange(_) -> System.Single.Parse input.value :> obj
                     | Checkbox -> input.``checked`` :> obj
                 |])
    let font = Font(axes :?> Axes)
    printfn "%A" font.axes     
    let svg = font.stringToSvg text 0 0
    output.innerHTML <- String.concat "\n" svg

let init = 
    for k,_ in fieldDefaults do
        let label = document.createElement "label" :?> HTMLLabelElement
        label.htmlFor <- k
        label.innerText <- (k + "  ")
        inputs.appendChild label |> ignore
        let input = document.createElement "input" :?> HTMLInputElement
        input.id <- k
        input.oninput <- generate
        let c = Axes.controls.[k]
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

let randomise reset _ = 
    let rnd = System.Random()
    let fracAsDefault = if reset then 1.0 else 0.4
    let checkboxFracAsDefault = if reset then 1.0 else 0.7
    for k,_ in fieldDefaults do
        let input = document.getElementById k :?> HTMLInputElement
        let c = Axes.controls.[k]
        match c with
        | Range(x, y) -> 
            input.value <- if rnd.NextDouble() < fracAsDefault then
                                string fieldDefaultsMap.[k]
                           else
                                string (rnd.Next(x, y))
        | FracRange(x, y) ->
            input.value <- if rnd.NextDouble() < fracAsDefault then
                                string fieldDefaultsMap.[k]
                           else
                                string (rnd.NextDouble() * (y-x) + x)
        | Checkbox ->
            input.``checked`` <- if rnd.NextDouble() < checkboxFracAsDefault then
                                    fieldDefaultsMap.[k] :?> bool
                                 else
                                    not (fieldDefaultsMap.[k] :?> bool)
    generate ()

textbox.innerHTML <- "!\"#£$%&'()*+,-./
:;<=>?@
[\\]^_` {|}~
abcdefghijklm
nopqrstuvwxyz
0123456789
ABCDEFGHIJKLM
NOPQRSTUVWXYZ"

//textbox.innerHTML <- "c    "
//((document.getElementById "filled") :?> HTMLInputElement).``checked`` <- false
// textbox.innerHTML <- The Unbearable
// Lightness
// of Being"
textbox.oninput <- generate
(document.getElementById "reset").onclick <- randomise true
(document.getElementById "randomise").onclick <- randomise false
init
generate ()