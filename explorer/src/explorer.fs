module Explorer

open System
open Microsoft.FSharp.Reflection
open Browser.Dom
open Browser.Types
open Axes
open Generator
open GeneratorTypes
open GlyphStringDefs
open GlyphFsDefs


let textbox = document.getElementById("text") :?> HTMLInputElement
let inputs = document.getElementById("inputs")
let output = document.getElementById("output")

let axisFields = FSharpType.GetRecordFields (typeof<Axes>)
let fieldDefaults = [
    for propinfo in axisFields do
        propinfo.Name, Reflection.FSharpValue.GetRecordField(Axes.DefaultAxes, propinfo)
    ]
let fieldDefaultsMap = Map.ofList fieldDefaults 

//Not supported in Fable :/
//let axesConstructor = Reflection.FSharpValue.PreComputeRecordConstructor typeof<Axes> 

///Read UI into array of current values
let currentFieldValues () = 
    let controlsMap = Axes.controls |> Map.ofList
    [for f, def in fieldDefaults do
        let input = document.getElementById f :?> HTMLInputElement
        let found, c = controlsMap.TryGetValue(f)
        match input with
        | null -> f, def
        | inputElement ->
            if found then
                match c with
                | Range(_) -> f, System.Int32.Parse input.value :> obj
                | FracRange(_) -> f, System.Single.Parse input.value :> obj
                | Checkbox -> f, input.``checked`` :> obj
            else
                f, def
    ]

///Create svg showing a number of values for each font axis
let tweensSvg (text : string) =
    let ch = text.[0]
    let steps = 9
    let currentValues = currentFieldValues ()
    let mutable yOffset = 0
    let svg, lineWidths, lineHeights =
        List.unzip3
            [for field, c in Axes.controls do
                let fonts = 
                    match c with
                    | Range(from, upto) ->
                        [for i in from..(upto-from)/steps..upto do
                            let fields = [|for k,v in currentValues do if k<>field then v else i|]
                            Reflection.FSharpValue.MakeRecord(typeof<Axes>, fields) :?> Axes |> Font
                        ]
                    | FracRange(from, upto) ->
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
        // scales width to fit
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
        let values = currentFieldValues () |> List.map snd |> Array.ofList
        let axes = Reflection.FSharpValue.MakeRecord(typeof<Axes>, values) :?> Axes
        let font = Font axes
        printfn "%A" font.axes
        let lines = text.Split('\r','\n') |> List.ofArray
        let start = DateTime.UtcNow.Ticks
        let svg = font.stringToSvg lines 0 0 false
        printfn "%d ms" ((DateTime.UtcNow.Ticks-start)/10000L)
        output.innerHTML <- String.concat "\n" svg

///Initialise controls
let init generate controls = 
    let label = document.createElement "label" :?> HTMLLabelElement
    label.htmlFor <- "tweens"
    label.innerText <- "show tweens  "
    inputs.appendChild label |> ignore
    let tweens = document.createElement "input" :?> HTMLInputElement
    tweens.id <- "tweens"
    tweens.oninput <- generate
    tweens.``type`` <- "checkbox"
    inputs.appendChild tweens |> ignore

    for f, c in controls do
        let label = document.createElement "label" :?> HTMLLabelElement
        label.htmlFor <- f
        label.innerText <- (f + "  ")
        inputs.appendChild label |> ignore
        let input = document.createElement "input" :?> HTMLInputElement
        input.id <- f
        input.oninput <- generate
        match c with
        | Range(x, y) -> 
            input.``type`` <- "range"
            input.min <- string x
            input.max <- string y 
            input.step <- string ((y-x)/20)
            input.value <- string fieldDefaultsMap.[f]
        | FracRange(x, y) ->
            input.``type`` <- "range"
            input.min <- string x
            input.max <- string y
            input.step <- "0.05" 
            input.value <- string fieldDefaultsMap.[f]
        | Checkbox ->
            input.``type`` <- "checkbox"
            input.``checked`` <- fieldDefaultsMap.[f] :?> bool
        inputs.appendChild input |> ignore

///Pick random inputs
let randomise reset generate _ = 
    let rnd = System.Random()
    let fracAsDefault = if reset then 1.0 else 0.4
    let checkboxFracAsDefault = if reset then 1.0 else 0.7
    for f, c in Axes.controls do
        let input = document.getElementById f :?> HTMLInputElement
        match input with
        | null -> ()
        | inputElement ->
            match c with
            | Range(x, y) -> 
                input.value <- if rnd.NextDouble() < fracAsDefault then
                                    string fieldDefaultsMap.[f]
                               else
                                    string (rnd.Next(x, y))
            | FracRange(x, y) ->
                input.value <- if rnd.NextDouble() < fracAsDefault then
                                    string fieldDefaultsMap.[f]
                               else
                                    string (rnd.NextDouble() * (y-x) + x)
            | Checkbox ->
                input.``checked`` <- if rnd.NextDouble() < checkboxFracAsDefault then
                                        fieldDefaultsMap.[f] :?> bool
                                     else
                                        not (fieldDefaultsMap.[f] :?> bool)
    generate ()

let run_explorer () = 
    let titleFont = Font({Axes.DefaultAxes with thickness=3})
    let titleElem = document.getElementById "title"
    titleElem.innerHTML <-  titleFont.stringToSvg ["Dactyl Live"] 0 0 true |> String.concat "\n"
    textbox.innerHTML <- allChars
    textbox.oninput <- generate
    (document.getElementById "reset").onclick <- randomise true generate
    (document.getElementById "randomise").onclick <- randomise false generate
    init generate Axes.controls
    generate ()


let generate_splines _ =
    let text = textbox.value
    let values = currentFieldValues () |> List.map snd |> Array.ofList
    let axes = Reflection.FSharpValue.MakeRecord(typeof<Axes>, values) :?> Axes
    let new_axes = {axes with clip_rect=false; filled=false; show_knots=true}
    let font_spline = Font {new_axes with spline_not_spiro=true}
    let font_spiro = Font {new_axes with spline_not_spiro=false}
    let spline = 
        try
            EList([for c in text.Split(separator_re) do parse_curve (GlyphFsDefs(axes)) c]) |> font_spline.translateByThickness
        with | _ -> Dot (YX(axes.thickness, axes.thickness))
    let spiro = 
        try
            EList([for c in text.Split(separator_re) do parse_curve (GlyphFsDefs(axes)) c]) |> font_spline.translateByThickness
        with | _ -> Dot (YX(axes.thickness, axes.thickness))
    let offsetX, offsetY = 0, 1000
    let svg = font_spline.elementToSvgPath spline offsetX offsetY 30 green
                @ if axes.show_knots then (spline |> font_spline.getSvgKnots offsetX offsetY false) else []
                @ font_spiro.elementToSvgPath spiro offsetX offsetY 10 blue
                @ if axes.show_knots then (spiro |> font_spiro.getSvgKnots offsetX offsetY false) else []
    // let outline = this.getOutline spine
    let svg = 
        toSvgDocument -50 -50 1000 1000 svg
    output.innerHTML <- String.concat "\n" svg


let run_splines () = 
    let titleFont = Font({Axes.DefaultAxes with thickness=3; spline_not_spiro=true})
    let titleFont2 = Font({Axes.DefaultAxes with thickness=3; spline_not_spiro=false})
    let svg = titleFont.stringToSvgLines ["Splines"] 0 0 
                @ titleFont2.stringToSvgLines ["Splines"] 0 0
    let svg = 
        toSvgDocument -50 -50 2000 1000 svg
    (document.getElementById "title").innerHTML <- String.concat "\n" svg
    let select = document.getElementById "char" :?> HTMLSelectElement
    for c in allChars.Replace("\r\n","") do
        let option = document.createElement "option" :?> HTMLOptionElement
        option.value <- string c
        option.innerText <- string c
        select.add option
    let getStringDef _ =
        textbox.innerHTML <- glyphMap.[select.value.[0]]
        generate_splines ()
    select.oninput <- getStringDef
    textbox.oninput <- generate_splines
    (document.getElementById "reset").onclick <- randomise true generate_splines
    // (document.getElementById "randomise").onclick <- randomise false
    let spline_controls = [
        // "new_definitions", Checkbox
        // "spline_not_spiro", Checkbox
        "show_knots", Checkbox
        "width", Range(100, 1000)
        "height", Range(100, 1000)
        "x_height", FracRange(0., 2.)
        "thickness", Range(1, 200)
        "contrast", FracRange(-0.5, 0.5)
        "roundedness", Range(0, 300)
        "tracking", Range(0, 200)
        "leading", Range(-100, 200)
        "monospace", FracRange(0.0, 1.0)
        // "italic", FracRange(0.0, 1.0)
        "serif", Range(0, 70)
        "end_bulb", FracRange(-1.0, 3.0)
        "flare", FracRange(-1.0, 1.0)
        "axis_align_caps", Checkbox
        "constraints", Checkbox
        // "filled", Checkbox
        "outline", Checkbox
        // "stroked", Checkbox
        // "scratches", Checkbox
        // "max_spline_iter", Range(0, 15)
        "show_tangents", Checkbox
        // "joints", Checkbox
        // "smooth", Checkbox
    ]

    init generate_splines spline_controls
    select.focus ()
    select.selectedIndex <- 4  //'e'
    getStringDef ()
    // generate_splines ()


if window.location.href.Contains("splines.html") then
    run_splines ()
else
    run_explorer ()
