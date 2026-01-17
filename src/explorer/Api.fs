module Api

open Fable.Core
open Axes
open Generator
open GeneratorTypes
open GlyphStringDefs
open GlyphFsDefs

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

let allChars = GlyphFsDefs.allChars

let generateSvg (text: string) (axes: Axes) =
    let font = Font axes

    let lines =
        if System.String.IsNullOrEmpty(text) then
            []
        else
            text.Replace("\r\n", "\n").Split('\n') |> List.ofArray

    // Using the same parameters as explorer.fs: 0 0 false black
    // You might want to make these configurable later
    font.stringToSvg lines 0 0 false "black" |> String.concat "\n"

let generateTweenSvg (text: string) (axes: Axes) =
    let font = Font axes

    let lines =
        if System.String.IsNullOrEmpty(text) then
            []
        else
            text.Replace("\r\n", "\n").Split('\n') |> List.ofArray

    // Manually construct SVG to crop tighter
    // Use smaller margin and height based on cap height + thickness, ignoring leading
    let margin = 10
    let svg, lineWidths = font.stringToSvgLineInternal lines 0 0 "black"
    let width = (List.max lineWidths) + margin * 2

    // Calculate vertical bounds to crop leading and alignment space
    // Visual Top of glyph is roughly at: thickness + leading
    // Visual Bottom is at: thickness + leading + (T - D) + thickness*2?
    // Let's use the layout logic: lineOffset places baseline.
    // Top is roughly 'leading' pixels down from 0 if we ignore ascenders going above T.
    // Actually, based on analysis: Visual Top = thickness + leading.
    // So we start viewBox there.
    let gdf = GlyphFsDefs(axes)
    let minY = axes.thickness + axes.leading - margin
    let height = axes.height - gdf._D + axes.thickness * 2 + margin * 2

    // Use toSvgDocument logic but with our custom bounds
    // We want to center the glyph vertically-ish, or just crop.
    // font.yBaselineOffset handles the descent.
    // We'll use a viewBox starting at -margin, -margin (relative to bottom-left origin of glyphs?)
    // No, standard toSvgDocument uses -margin for X.
    // For Y, stringToSvg uses -margin.
    // But we want to crop the top.
    // Let's set height to exactly what we need.
    toSvgDocument -margin minY width height svg |> String.concat "\n"

let generateSplineDebugSvg (text: string) (inputAxes: Axes) =
    let axes =
        { inputAxes with
            clip_rect = false
            filled = false
            show_knots = true }

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

    let fontDSpline =
        Font
            { axes with
                spline2 = false
                dactyl_spline = true }

    let fontGuides =
        Font
            { axes with
                spline2 = false
                show_knots = false
                debug = false }

    // Parse curves (handling errors gracefully as in explorer.fs)
    let separator_re = [| '\r'; '\n' |]

    let safeParseCurve (font: Font) c =
        try
            parse_curve (GlyphFsDefs(font.axes)) c font.axes.debug
        with _ ->
            Dot(YX(font.axes.thickness, font.axes.thickness))

    // Just take the first character's curve if possible, or iterate
    // explorer.fs seems to split by separator and parse. Here we'll just handle the whole string as "text"
    // but the original code `for c in text.Split...` implies it processes segments.
    // However, `parse_curve` takes a string name (like "a" or "one") from `GlyphFsDefs`.
    // The input `text` here is likely just characters.
    // Let's assume the user types characters. `parse_curve` actually parses the *definition* string from GlyphFsDefs.
    // But in `explorer.fs`, it does: `for c in text.Split...`
    // Wait, `explorer.fs` has: `textbox.value` -> `text`. `text.Split` -> `c`.
    // If the textbox contains characters, `parse_curve` fails unless `c` is a glyph name?
    // Looking at `parse_curve` usage in `explorer.fs`: `parse_curve ... c ...`
    // Actually, `parse_curve` expects the *code* string (e.g. "z z z").
    // In `run_compare_splines` it seems `textbox.value` is set to `glyphMap.[select.value.[0]]` which IS the code.
    // So for "Splines" tab, the text area should contain the GLYPH DEFINITION, not the character.
    // We will assume `text` is the glyph definition.

    let chars = text |> Seq.truncate 5 |> List.ofSeq
    let mutable xOffset = 0

    let elements =
        [ for c in chars do
              if Map.containsKey c GlyphStringDefs.glyphMap then
                  let elem =
                      GlyphStringDefs.stringDefsToElem (GlyphFsDefs(fontSpline2.axes)) c fontSpline2.axes.debug

                  // Use full width calculation including tracking/margins/etc
                  // Note: elem is reduced so we should really compute width on the glyph element wrapper or manual calc
                  // font.width expects an Element, so let's wrap it back in Glyph(c) style or just use width logic
                  // Actually fontSpline2.width works on any element.
                  // However, let's verify if we need to account for tracking separately.
                  // font.width includes: elemWidth + tracking + margins.
                  // So we just use fontSpline2.width(elem).
                  // But wait, elem is the *result* of stringDefsToElem, which is reduced.
                  // font.width runs reduce internally. Double reduce is fine usually or we just trust it.
                  // Alternatively, we manually do: elemWidth + tracking + padding
                  // Let's rely on fontSpline2.width(elem) but we need to subtract tracking if we add it manually?
                  // No, font.width includes checking axes.tracking.
                  let width = fontSpline2.width elem

                  let translated = fontSpline2.translateBy xOffset 0 elem

                  // xOffset is the start of the next char.
                  // font.width includes tracking, so we just add it to xOffset.
                  xOffset <- xOffset + width

                  yield translated ]

    let combinedElement =
        if List.isEmpty elements then
            Dot(YX(axes.thickness, axes.thickness))
        else
            EList(elements) |> fontSpline2.translateByThickness

    let spline = combinedElement
    let spiro = combinedElement

    let offsetX, offsetY = 0, fontSpline2.charHeight + axes.thickness
    let grey = "#e0e0e0"
    let blue = "blue"
    let green = "green"
    let orange = "orange"
    let lightGreen = "lightGreen"
    let lightBlue = "lightBlue"

    let guidesSvg =
        fontGuides.charToSvg '□' offsetX offsetY grey @ [ svgText 0 0 "Guides" ]

    let svgElements =
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

            let outlineSpiro = getOutline fontSpiro spiro
            let outlineSpline2 = getOutline fontSpline2 spline
            let outlineDSpline = getOutline fontDSpline spline // Note: dactyl_spline uses dactyl logic but applies to same structure? verify explorer.fs logic

            // Replicating explorer.fs logic exactly:
            let outlineSpiroSvg = safeElementToSvgPath fontSpiro outlineSpiro blue
            let outlineSpline2Svg = safeElementToSvgPath fontSpline2 outlineSpline2 green
            let outlineDSplineSvg = safeElementToSvgPath fontDSpline outlineDSpline orange

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

    // Calculate generic SVG bounds (similar to explorer.fs but returns string)
    // Explorer uses: toSvgDocument -50 fontSpline2.yBaselineOffset 1000 fontSpline2.charHeight svg
    // We need 'toSvgDocument' equivalent or just wrap it.
    // 'toSvgDocument' is likely in Generator/Axes/GlyphStringDefs.
    // Assuming 'toSvgDocument' returns a generic list of strings (lines of SVG).
    let svgWidth = max 1000 (xOffset + 100) // Ensure enough width, defaulting to at least 1000

    toSvgDocument -50 fontSpline2.yBaselineOffset svgWidth fontSpline2.charHeight svgElements
    |> String.concat "\n"
