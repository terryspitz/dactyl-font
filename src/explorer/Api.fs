module Api

open Fable.Core
open Axes
open Font
open GeneratorTypes
open GlyphStringDefs
open SvgHelpers

// Helper to convert the union type Controls to a JS-friendly object
let getControlDetails (name: string, control: Controls, category: string) =
    match control with
    | Range(min, max) ->
        {| name = name
           type_ = "range"
           min = float min
           max = float max
           step = (float max - float min) / 20.0
           category = category |}
    | FracRange(min, max) ->
        {| name = name
           type_ = "range"
           min = min
           max = max
           step = 0.05
           category = category |}
    | Checkbox ->
        {| name = name
           type_ = "checkbox"
           min = 0.0
           max = 1.0
           step = 1.0
           category = category |}

let controlDefinitions = Axes.controls |> List.map getControlDetails |> Array.ofList

let defaultAxes = Axes.DefaultAxes

let allChars =
    "abcdefghijklm
nopqrstuvwxyz
0123456789
ABCDEFGHIJKLM
NOPQRSTUVWXYZ
!\"#£$%&'()*+,-./:;
<=>?@[\\]^_`{|}~"

let generateSvg (text: string) (axes: Axes) (progress: (float -> unit) option) =
    let font = Font axes

    let lines =
        if System.String.IsNullOrEmpty(text) then
            []
        else
            text.Replace("\r\n", "\n").Split('\n') |> List.ofArray

    // Using the same parameters as explorer.fs: 0 0 false black
    // You might want to make these configurable later
    font.stringToSvg lines 0.0 0.0 false "black" progress |> String.concat "\n"

let generateTweenSvg (text: string) (axes: Axes) =
    let font = Font axes

    let lines =
        if System.String.IsNullOrEmpty(text) then
            []
        else
            text.Replace("\r\n", "\n").Split('\n') |> List.ofArray

    // Manually construct SVG to crop tighter
    // Use smaller margin and height based on cap height + thickness, ignoring leading
    let margin = 10.0
    let svg, lineWidths = font.stringToSvgLineInternal lines 0.0 0.0 "black" None
    let width = (List.max lineWidths) + margin * 2.0

    // Calculate vertical bounds to crop leading and alignment space
    // Visual Top of glyph is roughly at: thickness + leading
    // Visual Bottom is at: thickness + leading + (T - D) + thickness*2?
    // Let's use the layout logic: lineOffset places baseline.
    // Top is roughly 'leading' pixels down from 0 if we ignore ascenders going above T.
    // Actually, based on analysis: Visual Top = thickness + leading.
    // So we start viewBox there.
    let metrics = FontMetrics(axes)
    // Add extra padding to minY to prevent top cropping, especially for bold text
    let minY = float axes.thickness + float axes.leading - (margin * 2.0)
    // Increase height to compensate for the lower start point (more height needed)
    let height =
        float axes.height - float metrics.D
        + float axes.thickness * 2.0
        + (margin * 3.0)

    // Use toSvgDocument logic but with our custom bounds
    // We want to center the glyph vertically-ish, or just crop.
    // font.yBaselineOffset handles the descent.
    // We'll use a viewBox starting at -margin, -margin (relative to bottom-left origin of glyphs?)
    // No, standard toSvgDocument uses -margin for X.
    // For Y, stringToSvg uses -margin.
    // But we want to crop the top.
    // Let's set height to exactly what we need.
    toSvgDocument -margin minY width height svg |> String.concat "\n"

let generateSplineDebugSvgFromDefs (defsText: string) (inputAxes: Axes) (progress: (float -> unit) option) =
    let axes =
        { inputAxes with
            clip_rect = false
            filled = false
            show_comb = true
            show_tangents = true }

    // Create fonts with specific settings
    let fontSpiro =
        Font
            { axes with
                spline2 = false
                dactyl_spline = false }

    let fontSpline2 =
        Font
            { axes with
                spline2 = true
                dactyl_spline = false }

    let fontDactylSpline =
        Font
            { axes with
                spline2 = false
                dactyl_spline = true }

    let fontGuides =
        Font
            { axes with
                spline2 = false
                show_knots = false
                show_comb = false
                show_tangents = false
                debug = false }


    let lines =
        defsText.Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)

    let totalChars = lines.Length
    let mutable charIndex = 0.0
    let mutable xOffset = 0.0

    let elements =
        [ for line in lines do
              charIndex <- charIndex + 1.0

              match progress with
              | Some p -> p (float charIndex / float totalChars)
              | None -> ()

              let def =
                  let colonIdx = line.IndexOf(':')

                  if colonIdx >= 0 then
                      line.Substring(colonIdx + 1).Trim()
                  else
                      line.Trim()

              if not (System.String.IsNullOrWhiteSpace(def)) then
                  let elem =
                      GlyphStringDefs.rawDefToElem (FontMetrics(fontSpline2.axes)) def fontSpline2.axes.debug

                  let width = fontSpline2.width elem
                  let translated = translateBy xOffset 0.0 elem
                  xOffset <- xOffset + width

                  yield translated ]

    let combinedElement =
        if List.isEmpty elements then
            Dot(
                { y = axes.thickness
                  x = axes.thickness
                  y_fit = false
                  x_fit = false }
            )
        else
            EList(elements) |> fontSpline2.translateByThickness
        |> fontSpline2.italicise

    let spline = combinedElement
    let spiro = combinedElement

    let offsetX, offsetY = 0.0, fontSpline2.charHeight + float axes.thickness
    let grey = "#e0e0e0"
    let blue = "blue"
    let green = "green"
    let orange = "#FFA500c0"
    let lightGreen = "lightGreen"
    let lightBlue = "lightBlue"
    let lightOrange = "#FFD580"

    let guidesSvg =
        fontGuides.charToSvg '□' offsetX offsetY grey @ [ svgText 0 0 "Guides" ]

    let svgElements =
        if not axes.outline then
            let wrapClass (cls: string) (svgs: string list) =
                if List.isEmpty svgs then
                    []
                else
                    [ sprintf "<g class='%s'>" cls ] @ svgs @ [ "</g>" ]

            let guidesLayer = wrapClass "guides-layer" guidesSvg

            let spiroLayer =
                wrapClass "spiro-layer" (fontSpiro.elementToSvgPath spiro offsetX offsetY 10 blue)

            let spline2Layer =
                wrapClass "spline2-layer" (fontSpline2.elementToSvgPath spline offsetX offsetY 10 green)

            let dsplineLayer =
                wrapClass "dspline-layer" (fontDactylSpline.elementToSvgPath spline offsetX offsetY 10 orange)

            let knotsLayer =
                (wrapClass
                    "spiro-layer knots-layer"
                    (spiro |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightBlue fontSpiro.isJoint))
                @ (wrapClass
                    "spline2-layer knots-layer"
                    (spline
                     |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightGreen fontSpline2.isJoint))
                @ (wrapClass
                    "dspline-layer knots-layer"
                    (spline
                     |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightOrange fontDactylSpline.isJoint))

            let labelsLayer =
                wrapClass "labels-layer" (SvgHelpers.getSvgLabels offsetX offsetY spline)

            guidesLayer
            @ spiroLayer
            @ spline2Layer
            @ dsplineLayer
            @ knotsLayer
            @ labelsLayer
        else
            let getOutline (font: Font) shape =
                try
                    font.getOutline shape
                with _ ->
                    shape

            let safeElementToSvgPath (font: Font) shape color =
                try
                    font.elementToSvgPath shape offsetX offsetY 10 color
                with _ ->
                    []

            let wrapClass (cls: string) (svgs: string list) =
                if List.isEmpty svgs then
                    []
                else
                    [ sprintf "<g class='%s'>" cls ] @ svgs @ [ "</g>" ]

            let outlineSpiro = getOutline fontSpiro spiro
            let outlineSpline2 = getOutline fontSpline2 spline
            let outlineDactylSpline = getOutline fontDactylSpline spline

            let outlineSpiroSvg = safeElementToSvgPath fontSpiro outlineSpiro blue
            let outlineSpline2Svg = safeElementToSvgPath fontSpline2 outlineSpline2 green

            let outlineDactylSplineSvg =
                safeElementToSvgPath fontDactylSpline outlineDactylSpline orange

            let guidesLayer = wrapClass "guides-layer" guidesSvg

            let spiroLayer =
                wrapClass "spiro-layer" (fontSpiro.elementToSvgPath spiro offsetX offsetY 3 blue @ outlineSpiroSvg)

            let spline2Layer =
                wrapClass
                    "spline2-layer"
                    (fontSpline2.elementToSvgPath spline offsetX offsetY 3 green @ outlineSpline2Svg)

            let dsplineLayer =
                wrapClass
                    "dspline-layer"
                    (fontDactylSpline.elementToSvgPath spline offsetX offsetY 3 orange
                     @ outlineDactylSplineSvg)

            let knotsLayer =
                (wrapClass
                    "spiro-layer knots-layer"
                    (spiro |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightBlue fontSpiro.isJoint))
                @ (wrapClass
                    "spiro-layer knots-layer"
                    (outlineSpiro
                     |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightBlue fontSpiro.isJoint))
                @ (wrapClass
                    "spline2-layer knots-layer"
                    (spline
                     |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightGreen fontSpline2.isJoint))
                @ (wrapClass
                    "spline2-layer knots-layer"
                    (outlineSpline2
                     |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightGreen fontSpline2.isJoint))
                @ (wrapClass
                    "dspline-layer knots-layer"
                    (spline
                     |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightOrange fontDactylSpline.isJoint))
                @ (wrapClass
                    "dspline-layer knots-layer"
                    (outlineDactylSpline
                     |> SvgHelpers.getSvgKnots offsetX offsetY 5.0 lightOrange fontDactylSpline.isJoint))

            let labelsLayer =
                wrapClass "labels-layer" (SvgHelpers.getSvgLabels offsetX offsetY spline)

            guidesLayer
            @ spiroLayer
            @ spline2Layer
            @ dsplineLayer
            @ knotsLayer
            @ labelsLayer

    let svgWidth = max 1000.0 (xOffset + 100.0)

    toSvgDocument -50.0 fontSpline2.yBaselineOffset svgWidth fontSpline2.charHeight svgElements
    |> String.concat "\n"

let getGlyphDefs (text: string) =
    if System.String.IsNullOrEmpty(text) then
        ""
    else
        // Deduplicate chars to avoid spamming same def
        let chars = text |> Seq.map id |> Seq.distinct |> Seq.toList

        chars
        |> List.map (fun c ->
            match GlyphStringDefs.glyphMap.TryFind c with
            | Some def -> sprintf "'%c': %s" c def
            | None -> sprintf "'%c': (no definition)" c)
        |> String.concat "\n"

let generateVisualTestsSvg () =
    VisualTests.splineStaticPage ()
    |> toSvgDocument 0 0 10 12
    |> String.concat "\n"
    |> fun s -> s.Replace("svg ", "svg style='height: 95vh;' ")

let generateVisualDiffsSvg (text: string) (axes: Axes) (progress: (float -> unit) option) =
    let fontOff = Font axes

    let fontOn =
        Font
            { axes with
                debug = false
                dactyl_spline = false
                spline2 = true }

    let chars =
        if System.String.IsNullOrEmpty(text) then
            []
        else
            text.Replace("\n", "").Replace("\r", "") |> Seq.toList

    let totalChars = chars.Length

    let marginX = max 200 (axes.thickness * 2)
    let marginY = max 200 (axes.thickness * 2)

    let cols = 5
    let cellWidth = (axes.width + marginX) * 3 + marginX
    let cellHeight = fontOn.charHeight + float (marginY * 2)

    let keyFontSize = (axes.width / 3) |> string

    let keySvg =
        [ sprintf
              "<text x='0' y='%f' font-size='%s' fill='black'>Key: Left = Old, Middle = New, Right = Overlaid Diff (Red=Old, Blue=New)</text>"
              (cellHeight / 2.0)
              keyFontSize ]

    let svgs =
        keySvg
        @ (chars
           |> List.mapi (fun i ch ->
               match progress with
               | Some p -> p (float i / float totalChars)
               | None -> ()

               let row = i / cols
               let col = i % cols

               let xOffset = float col * float cellWidth
               let yOffset = float (row + 1) * cellHeight

               // Col 1: off
               let svgOff = fontOff.charToSvg ch xOffset yOffset "black"

               // Col 2: on
               let svgOn =
                   fontOn.charToSvg ch (xOffset + float axes.width + float marginX) yOffset "black"

               // Col 3: overlaid
               let overlayX = xOffset + (float axes.width + float marginX) * 2.0
               let svgOffRed = fontOff.charToSvg ch overlayX yOffset "rgba(255, 0, 0, 0.5)"
               let svgOnBlue = fontOn.charToSvg ch overlayX yOffset "rgba(0, 0, 255, 0.5)"

               // Add labels
               let fontSize = (axes.width / 5) |> string

               let labels =
                   [ sprintf
                         "<text x='%f' y='%f' font-size='%s' fill='gray'>%c</text>"
                         xOffset
                         (yOffset - float axes.thickness)
                         fontSize
                         ch ]

               labels @ svgOff @ svgOn @ svgOffRed @ svgOnBlue)
           |> List.concat)

    let totalWidth = float cols * float cellWidth
    let totalHeight = float ((chars.Length + cols - 1) / cols + 1) * cellHeight

    toSvgDocument (float -marginX) (float -marginY) totalWidth totalHeight svgs
    |> String.concat "\n"
