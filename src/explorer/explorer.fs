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

let grey = "#e0e0e0"

let textbox = document.getElementById ("text") :?> HTMLInputElement
let inputs = document.getElementById ("inputs")
let output = document.getElementById ("output")

let axisFields = FSharpType.GetRecordFields(typeof<Axes>)

let fieldDefaults =
    [ for propinfo in axisFields do
          propinfo.Name, Reflection.FSharpValue.GetRecordField(Axes.DefaultAxes, propinfo) ]

let fieldDefaultsMap = Map.ofList fieldDefaults

//Not supported in Fable :/
//let axesConstructor = Reflection.FSharpValue.PreComputeRecordConstructor typeof<Axes>

///Read UI into array of current values
let currentFieldValues () =
    let controlsMap = Axes.controls |> List.map (fun (n, c, _) -> n, c) |> Map.ofList

    [ for f, def in fieldDefaults do
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
                  f, def ]

///Create svg showing a number of values for each font axis
let tweensSvg (text: string) =
    let ch = text.[0]
    let steps = 9
    let currentValues = currentFieldValues ()
    let mutable yOffset = 0

    let svg, lineWidths, lineHeights =
        List.unzip3
            [ for field, c, _ in Axes.controls do
                  let fonts =
                      match c with
                      | Range(from, upto) ->
                          [ for i in from .. (upto - from) / steps .. upto do
                                let fields =
                                    [| for k, v in currentValues do
                                           if k <> field then v else i |]

                                Reflection.FSharpValue.MakeRecord(typeof<Axes>, fields) :?> Axes |> Font ]
                      | FracRange(from, upto) ->
                          [ for i in from .. ((upto - from) / float steps) .. upto do
                                let fields =
                                    [| for k, v in currentValues do
                                           if k <> field then v else i |]

                                Reflection.FSharpValue.MakeRecord(typeof<Axes>, fields) :?> Axes |> Font ]
                      | Checkbox ->
                          [ for i in 0..1 do
                                let fields =
                                    [| for k, v in currentValues do
                                           if k <> field then v else i > 0 |]

                                Reflection.FSharpValue.MakeRecord(typeof<Axes>, fields) :?> Axes |> Font ]

                  let widths =
                      [ for font in fonts do
                            font.charWidth ch ]

                  let offsetXs = List.scan (+) 0 widths

                  let height =
                      List.max
                          [ for font in fonts do
                                font.charHeight ]

                  let title = svgText 0 (yOffset + 100) field
                  yOffset <- yOffset + height + 100
                  let lineOffset = yOffset - fonts.[0].yBaselineOffset + 100

                  let svg =
                      title
                      :: [ for i in 0 .. fonts.Length - 1 do
                               yield! fonts.[i].charToSvg ch (offsetXs.[i]) lineOffset black ]

                  (svg, List.sum widths, fonts.[0].charHeight + 100) ]

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
        let lines = text.Split('\r', '\n') |> List.ofArray
        let start = DateTime.UtcNow.Ticks
        let svg = font.stringToSvg lines 0 0 false black
        printfn "%d ms" ((DateTime.UtcNow.Ticks - start) / 10000L)
        output.innerHTML <- String.concat "\n" svg

///Initialise controls
let init generate controls add_tweens =
    if add_tweens then
        let label = document.createElement "label" :?> HTMLLabelElement
        label.htmlFor <- "tweens"
        label.innerText <- "show tweens  "
        inputs.appendChild label |> ignore
        let tweens = document.createElement "input" :?> HTMLInputElement
        tweens.id <- "tweens"
        tweens.oninput <- generate
        tweens.``type`` <- "checkbox"
        inputs.appendChild tweens |> ignore

    // 'controls' passed to init is generic, but here we expect it to match what we iterate over.
    // However, init is called with Axes.controls in run_explorer, which is 3-tuple.
    // Also called with spline_controls in run_compare_splines, which IS 2-tuple (defined locally).
    // This is tricky.
    // F# should infer 'controls' as seq<'a * 'b> or something?
    // Using simple pattern matching in the loop might fail if they are different types.
    // I should probably make Axes.controls match the shape expected or handle it.
    // But since I can't change 'controls' type in init easily without breaking one of them.
    // I'll inline the fix in run_explorer to project Axes.controls to 2-tuples before calling init.

    // Actually, I'll just change init to destructure carefully? No, tuples have different sizes.
    // Best is to project Axes.controls in run_explorer.

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
            input.step <- string ((y - x) / 20)
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

    for f, c, _ in Axes.controls do
        let input = document.getElementById f :?> HTMLInputElement

        match input with
        | null -> ()
        | inputElement ->
            match c with
            | Range(x, y) ->
                input.value <-
                    if rnd.NextDouble() < fracAsDefault then
                        string fieldDefaultsMap.[f]
                    else
                        string (rnd.Next(x, y))
            | FracRange(x, y) ->
                input.value <-
                    if rnd.NextDouble() < fracAsDefault then
                        string fieldDefaultsMap.[f]
                    else
                        string (rnd.NextDouble() * (y - x) + x)
            | Checkbox ->
                input.``checked`` <-
                    if rnd.NextDouble() < checkboxFracAsDefault then
                        fieldDefaultsMap.[f] :?> bool
                    else
                        not (fieldDefaultsMap.[f] :?> bool)

    generate ()

let run_explorer () =
    let titleFont = Font({ Axes.DefaultAxes with thickness = 3 })
    let titleElem = document.getElementById "title"
    titleElem.innerHTML <- titleFont.stringToSvg [ "Dactyl Live" ] 0 0 true black |> String.concat "\n"
    textbox.innerHTML <- allChars
    textbox.oninput <- generate
    (document.getElementById "reset").onclick <- randomise true generate
    (document.getElementById "randomise").onclick <- randomise false generate
    let explorerControls = Axes.controls |> List.map (fun (n, c, _) -> n, c)
    init generate explorerControls true
    generate ()


let generate_splines _ =
    let text = textbox.value
    let values = currentFieldValues () |> List.map snd |> Array.ofList
    let axes = Reflection.FSharpValue.MakeRecord(typeof<Axes>, values) :?> Axes

    let newAxes =
        { axes with
            clip_rect = false
            filled = false
            show_knots = true }

    let fontSpiro =
        Font
            { newAxes with
                spline2 = false
                dactyl_spline = false }

    let fontSpline2 =
        Font
            { newAxes with
                spline2 = true
                dactyl_spline = false }

    let fontDSpline =
        Font
            { newAxes with
                spline2 = false
                dactyl_spline = true }

    let fontGuides =
        Font
            { newAxes with
                spline2 = false
                show_knots = false
                debug = false }

    let spline =
        try
            EList(
                [ for c in text.Split(separator_re) do
                      parse_curve (GlyphFsDefs(axes)) c axes.debug ]
            )
            |> fontSpline2.translateByThickness
        with _ ->
            Dot(YX(axes.thickness, axes.thickness))

    let spiro =
        try
            EList(
                [ for c in text.Split(separator_re) do
                      parse_curve (GlyphFsDefs(axes)) c axes.debug ]
            )
            |> fontSpline2.translateByThickness
        with _ ->
            Dot(YX(axes.thickness, axes.thickness))

    let offsetX, offsetY = 0, fontSpline2.charHeight + axes.thickness

    let guidesSvg =
        fontGuides.charToSvg '□' offsetX offsetY grey @ [ svgText 0 0 "Guides" ]

    let svg =
        if not axes.outline then
            guidesSvg
            @ fontSpiro.elementToSvgPath spiro offsetX offsetY 10 blue
            @ fontSpline2.elementToSvgPath spline offsetX offsetY 10 green
            @ fontDSpline.elementToSvgPath spline offsetX offsetY 10 orange
            @ if axes.show_knots then
                  (spline |> fontSpline2.getSvgKnots offsetX offsetY 5 lightGreen)
                  @ (spiro |> fontSpiro.getSvgKnots offsetX offsetY 5 lightBlue)
              else
                  []
        else
            let outlineSpiro =
                try
                    fontSpiro.getOutline spiro
                with _ ->
                    spiro

            let outlineSpiroSvg =
                try
                    fontSpiro.elementToSvgPath outlineSpiro offsetX offsetY 10 blue
                with _ ->
                    []

            let outlineSpline2 =
                try
                    fontSpline2.getOutline spline
                with _ ->
                    spline

            let outlineSpline2Svg =
                try
                    fontSpline2.elementToSvgPath outlineSpline2 offsetX offsetY 10 green
                with _ ->
                    []

            let outlineDSpline =
                try
                    fontDSpline.getOutline spline
                with _ ->
                    spline

            let outlineDSplineSvg =
                try
                    fontDSpline.elementToSvgPath outlineDSpline offsetX offsetY 10 orange
                with _ ->
                    []

            // font_spline.GlyphFsDefs.guidesSvg
            guidesSvg
            @ fontSpiro.elementToSvgPath spiro offsetX offsetY 3 blue
            @ fontSpline2.elementToSvgPath spline offsetX offsetY 3 green
            @ fontDSpline.elementToSvgPath spline offsetX offsetY 3 orange
            @ outlineSpiroSvg
            @ outlineSpline2Svg
            @ outlineDSplineSvg
            @ if axes.show_knots then
                  (spline |> fontSpline2.getSvgKnots offsetX offsetY 3 lightGreen)
                  @ (spiro |> fontSpiro.getSvgKnots offsetX offsetY 3 lightBlue)
                  @ (outlineSpline2 |> fontSpline2.getSvgKnots offsetX offsetY 5 lightGreen)
                  @ (outlineSpiro |> fontSpiro.getSvgKnots offsetX offsetY 5 lightBlue)
              else
                  []

    let svgDoc =
        toSvgDocument -50 fontSpline2.yBaselineOffset 1000 fontSpline2.charHeight svg

    output.innerHTML <- String.concat "\n" svgDoc
// ((document.getElementsByTagName "svg").[0] :?> HTMLElement).setAttribute("style", "height:50%")


let run_compare_splines () =
    let titleFontSpiro =
        Font(
            { Axes.DefaultAxes with
                thickness = 3
                spline2 = false
                dactyl_spline = false }
        )

    let titleFontSpline2 =
        Font(
            { titleFontSpiro.axes with
                spline2 = true }
        )

    let titleFontDSpline =
        Font(
            { titleFontSpiro.axes with
                dactyl_spline = true }
        )

    let titleSvg =
        titleFontSpiro.stringToSvgLines [ "Spiro" ] 80 80 blue
        @ titleFontSpline2.stringToSvgLines [ "Splines" ] 40 40 green
        @ titleFontDSpline.stringToSvgLines [ "DSpline" ] 0 0 orange

    let titleSvgDoc = toSvgDocument -50 -50 2000 1000 titleSvg
    (document.getElementById "title").innerHTML <- String.concat "\n" titleSvgDoc
    let select = document.getElementById "char" :?> HTMLSelectElement

    for c in allChars.Replace("\r\n", "") do
        let option = document.createElement "option" :?> HTMLOptionElement
        option.value <- string c
        option.innerText <- string c
        select.add option

    let getStringDef _ =
        textbox.value <- glyphMap.[select.value.[0]]
        generate_splines ()

    select.oninput <- getStringDef
    textbox.oninput <- generate_splines
    (document.getElementById "reset").onclick <- randomise true generate_splines
    // (document.getElementById "randomise").onclick <- randomise false
    let spline_controls =
        [
          // "new_definitions", Checkbox
          // "spline2", Checkbox
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
          "italic", FracRange(0.0, 1.0)
          "serif", Range(0, 70)
          "end_bulb", FracRange(-1.0, 3.0)
          "flare", FracRange(-1.0, 1.0)
          "axis_align_caps", Checkbox
          "constraints", Checkbox
          // "filled", Checkbox
          "outline", Checkbox
          // "stroked", Checkbox
          // "scratches", Checkbox
          "max_spline_iter", Range(0, 100)
          "show_tangents", Checkbox
          // "joints", Checkbox
          // "smooth", Checkbox
          "debug", Checkbox ]

    init generate_splines spline_controls false
    select.focus ()
    select.selectedIndex <- 2 //'c'
    // select.selectedIndex <- 4  //'e'
    getStringDef ()
// generate_splines ()


let run_visual_tests () =
    output.innerHTML <- VisualTests.splineStaticPage () |> toSvgDocument 0 0 10 12 |> String.concat "\n"
    output.innerHTML <- output.innerHTML.Replace("svg ", "svg style='height: 95vh;' ")

if window.location.href.Contains("splines.html") then
    run_compare_splines ()
elif window.location.href.Contains("visualTests.html") then
    run_visual_tests ()
else
    run_explorer ()
