module Explorer

open System
open Microsoft.FSharp.Reflection
open Browser.Dom
open Browser.Types
open Axes
open Generator

let textbox = document.getElementById("text") :?> HTMLInputElement
let inputs = document.getElementById("inputs")
let output = document.getElementById("output")

let axisFields = FSharpType.GetRecordFields (typeof<Axes>)
let fieldDefaults = [
    for propinfo in axisFields do
        propinfo.Name, Reflection.FSharpValue.GetRecordField(Axes.DefaultAxes, propinfo)
    ]
let fieldDefaultsMap = Map.ofList fieldDefaults 
let fieldNames = fst (List.unzip fieldDefaults)

//Not supported in Fable :()
//let axesConstructor = Reflection.FSharpValue.PreComputeRecordConstructor typeof<Axes> 

let titleFont = Font({Axes.DefaultAxes with thickness=3})
let titleElem = document.getElementById "title"
titleElem.innerHTML <-  titleFont.stringToSvg ["Dactyl Live"] 0 0 |> String.concat "\n"

///Read UI into array of current values
let currentFieldValues () = 
    [|for f in fieldNames do
        let input = document.getElementById f  :?> HTMLInputElement
        let c = Axes.controls.[f]
        match c with
        | Range(_) -> System.Int32.Parse input.value :> obj
        | FracRange(_) -> System.Single.Parse input.value :> obj
        | Checkbox -> input.``checked`` :> obj
    |]

///Create svg showing a number of values for each font axis
let tweensSvg (text : string) =
    let ch = text.[0]
    let steps = 9
    let currentValues = List.zip fieldNames (List.ofArray (currentFieldValues ()))
    let mutable yOffset = 0
    let svg, lineWidths, lineHeights =
        List.unzip3
            // [for f in 0..fieldDefaults.Length-1 do
            [for f in 0..fieldDefaults.Length-1 do
                let field = fieldNames.[f]
                let c = Axes.controls.[field]
                let fonts = 
                    match c with
                    | Range(from,upto) ->
                        [for i in from..(upto-from)/steps..upto do
                            let fields = [|for k,v in currentValues do if k<>field then v else i|]
                            Reflection.FSharpValue.MakeRecord(typeof<Axes>, fields) :?> Axes |> Font
                        ]
                    | FracRange(from,upto) ->
                        [for i in from..((upto-from)/float steps)..upto do
                            let fields = [|for k,v in currentValues do if k<>field then v else i|]
                            Reflection.FSharpValue.MakeRecord(typeof<Axes>, fields) :?> Axes |> Font
                        ]
                    | Checkbox ->
                        [for i in 0..1 do
                            let fields = [|for k,v in currentValues do if k<>field then v else i > 0|]
                            Reflection.FSharpValue.MakeRecord(typeof<Axes>, fields) :?> Axes |> Font
                        ]
                let widths = [for font in fonts do font.charWidth ch]
                let offsetXs = List.scan (+) 0 widths
                let height = List.max [for font in fonts do font.charHeight]
                let title = svgText 0 (yOffset+100) field
                yOffset <- yOffset + height + 100
                let lineOffset = yOffset - fonts.[0].yBaselineOffset + 100
                let svg = title
                          :: [for i in 0..fonts.Length-1 do yield! fonts.[i].charToSvg ch (offsetXs.[i]) lineOffset]
                (svg, List.sum widths, fonts.[0].charHeight + 100)
            ]
    let margin = 50
    toSvgDocument 
        -margin
        -margin
        (List.max lineWidths * 2 + margin)
        (yOffset + margin)
        (List.collect id svg)

///Update UI with SVG using current inputs
let generate _ = 
    let tweens = (document.getElementById "tweens" :?> HTMLInputElement).``checked``
    let text = textbox.value
    if tweens then
        output.innerHTML <- String.concat "\n" (tweensSvg text)
    else
        let axes = Reflection.FSharpValue.MakeRecord(typeof<Axes>, currentFieldValues ()) :?> Axes
        let font = Font axes
        printfn "%A" font.axes
        let lines = text.Split('\r','\n') |> List.ofArray
        let start = DateTime.UtcNow.Ticks
        let svg = font.stringToSvg lines 0 0
        printfn "%d ms" ((DateTime.UtcNow.Ticks-start)/10000L)
        output.innerHTML <- String.concat "\n" svg

///Initialise controls
let init = 
    let label = document.createElement "label" :?> HTMLLabelElement
    label.htmlFor <- "tweens"
    label.innerText <- "show tweens  "
    inputs.appendChild label |> ignore
    let tweens = document.createElement "input" :?> HTMLInputElement
    tweens.id <- "tweens"
    tweens.oninput <- generate
    tweens.``type`` <- "checkbox"
    inputs.appendChild tweens |> ignore

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

///Pick random inputs
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

textbox.innerHTML <- "abcdefghijklm
nopqrstuvwxyz
0123456789
ABCDEFGHIJKLM
NOPQRSTUVWXYZ
!\"#£$%&'()*+,-./:;
<=>?@[\\]^_`{|}~"

//textbox.innerHTML <- "56zvwx  "
// ((document.getElementById "show_knots") :?> HTMLInputElement).``checked`` <- true
// ((document.getElementById "serif") :?> HTMLInputElement).value <- "20"
// textbox.innerHTML <- "The Unbearable
// Lightness
// of Being"
textbox.oninput <- generate
(document.getElementById "reset").onclick <- randomise true
(document.getElementById "randomise").onclick <- randomise false
init
generate ()