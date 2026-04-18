module Api

open Fable.Core
open Axes
open SpiroPointType
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

let generateSvg (text: string) (axes: Axes) (autoscale: bool) (progress: (float -> unit) option) =
    let font = Font axes

    let lines =
        if System.String.IsNullOrEmpty(text) then
            []
        else
            text.Replace("\r\n", "\n").Split('\n') |> List.ofArray

    font.stringToSvg lines 0.0 0.0 autoscale "black" progress |> String.concat "\n"

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

let generateTweenDiffSvg (text: string) (axesOff: Axes) (axesOn: Axes) =
    let fontOff = Font axesOff
    let fontOn = Font axesOn

    let lines =
        if System.String.IsNullOrEmpty(text) then []
        else text.Replace("\r\n", "\n").Split('\n') |> List.ofArray

    let margin = 10.0
    let svgOff, lineWidthsOff = fontOff.stringToSvgLineInternal lines 0.0 0.0 "rgba(255, 0, 0, 0.5)" None
    let svgOn,  lineWidthsOn  = fontOn.stringToSvgLineInternal  lines 0.0 0.0 "rgba(0, 0, 255, 0.5)"  None
    let width = (max (List.max lineWidthsOff) (List.max lineWidthsOn)) + margin * 2.0

    let metrics = FontMetrics(axesOff)
    let minY = float axesOff.thickness + float axesOff.leading - (margin * 2.0)
    let height =
        float axesOff.height - float metrics.D
        + float axesOff.thickness * 2.0
        + (margin * 3.0)

    toSvgDocument -margin minY width height (svgOff @ svgOn) |> String.concat "\n"

let generateSplineDebugSvgFromDefs (defsText: string) (inputAxes: Axes) (progress: (float -> unit) option) =
    let axes =
        { inputAxes with
            clip_rect = false
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

            // Spine fonts: never fill the thin centerline paths
            let fontSpiroSpine = Font { fontSpiro.axes with filled=false }
            let fontSpline2Spine = Font { fontSpline2.axes with filled=false }
            let fontDactylSplineSpine = Font { fontDactylSpline.axes with filled=false }

            let guidesLayer = wrapClass "guides-layer" guidesSvg

            let spiroLayer =
                wrapClass "spiro-layer" (fontSpiroSpine.elementToSvgPath spiro offsetX offsetY 3 blue @ outlineSpiroSvg)

            let spline2Layer =
                wrapClass
                    "spline2-layer"
                    (fontSpline2Spine.elementToSvgPath spline offsetX offsetY 3 green @ outlineSpline2Svg)

            let dsplineLayer =
                wrapClass
                    "dspline-layer"
                    (fontDactylSplineSpine.elementToSvgPath spline offsetX offsetY 3 orange
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


let generateSplineViewerSvg () =
    SplineViewer.splineStaticPage ()
    |> SvgHelpers.toSvgDocument 0. 0. 10. 12.
    |> String.concat "\n"


let spiroToSplinePointType (ty: SpiroPointType) =
    match ty with
    | SpiroPointType.Corner -> Curves.SplinePointType.Corner
    | SpiroPointType.G4 | SpiroPointType.G2 -> Curves.SplinePointType.Smooth
    | SpiroPointType.Right -> Curves.SplinePointType.LineToCurve
    | SpiroPointType.Left -> Curves.SplinePointType.CurveToLine
    | _ -> Curves.SplinePointType.Corner

let computeCurvatureData (bezPts: DactylSpline.BezierPoint array) (isClosed: bool) =
    let steps = 20
    let count = if isClosed then bezPts.Length else bezPts.Length - 1
    let samples = ResizeArray()
    let knotArcs = ResizeArray()
    let mutable arcLen = 0.0
    for i in 0 .. count - 1 do
        let p1 = bezPts.[i]
        let p2 = bezPts.[(i + 1) % bezPts.Length]
        let cp1x = p1.x + p1.rd * cos p1.th_out
        let cp1y = p1.y + p1.rd * sin p1.th_out
        let cp2x = p2.x - p2.ld * cos p2.th_in
        let cp2y = p2.y - p2.ld * sin p2.th_in
        let p0x, p0y = p1.x, p1.y
        let p3x, p3y = p2.x, p2.y
        if i = 0 then knotArcs.Add(arcLen)
        for s in 0 .. steps - 1 do
            let tmid = (float s + 0.5) / float steps
            let mt = 1.0 - tmid
            let dx1 = 3.0 * (mt*mt*(cp1x-p0x) + 2.0*mt*tmid*(cp2x-cp1x) + tmid*tmid*(p3x-cp2x))
            let dy1 = 3.0 * (mt*mt*(cp1y-p0y) + 2.0*mt*tmid*(cp2y-cp1y) + tmid*tmid*(p3y-cp2y))
            let dx2 = 6.0 * ((1.0-tmid)*(cp2x-2.0*cp1x+p0x) + tmid*(p3x-2.0*cp2x+cp1x))
            let dy2 = 6.0 * ((1.0-tmid)*(cp2y-2.0*cp1y+p0y) + tmid*(p3y-2.0*cp2y+cp1y))
            let speed = sqrt(dx1*dx1 + dy1*dy1)
            let denom = speed * speed * speed
            let kappa = if denom < 1e-10 then 0.0 else (dx1*dy2 - dy1*dx2) / denom
            samples.Add({| arcLen = arcLen; curvature = kappa |})
            arcLen <- arcLen + speed / float steps
        knotArcs.Add(arcLen)
    {| samples = samples.ToArray(); knotArcs = knotArcs.ToArray() |}

let solveSplineEditor (ctrlPts: DactylSpline.DControlPoint array) (isClosed: bool) (maxIter: int) =
    let spline = DactylSpline.DactylSpline(ctrlPts, isClosed)
    let bezPts, pathSvg, combSvg, tangentSvg = spline.solveAndRenderFull(maxIter, 1.0, false, true, true)
    {| pathSvg = pathSvg |> String.concat ""
       combSvg = combSvg |> String.concat ""
       tangentSvg = tangentSvg |> String.concat ""
       bezierPoints =
           bezPts |> Array.map (fun (bp: DactylSpline.BezierPoint) ->
               {| x = bp.x; y = bp.y
                  th_in = bp.th_in; th_out = bp.th_out
                  ld = bp.ld; rd = bp.rd |})
       curvatureData = computeCurvatureData bezPts isClosed |}

/// Generate a proper ink outline for the current spline via Font.getDactylSansOutlines.
let getSplineOutlinePath (ctrlPts: DactylSpline.DControlPoint array) (isClosed: bool) (inputAxes: Axes) =
    try
        let axes = { inputAxes with outline = true; filled = true; dactyl_spline = true }
        let font = Font axes
        let splineToSpiro (ty: Curves.SplinePointType) =
            match ty with
            | Curves.SplinePointType.CurveToLine -> SpiroPointType.Left
            | Curves.SplinePointType.LineToCurve -> SpiroPointType.Right
            | Curves.SplinePointType.Smooth      -> SpiroPointType.G2
            | _                                  -> SpiroPointType.Corner
        let knots =
            ctrlPts
            |> Array.map (fun cp ->
                { pt = { x = cp.x |> Option.defaultValue 0.0
                         y = cp.y |> Option.defaultValue 0.0
                         x_fit = cp.x.IsNone
                         y_fit = cp.y.IsNone }
                  ty = splineToSpiro cp.ty
                  th_in = cp.th_in
                  th_out = cp.th_out
                  label = None })
            |> Array.toList
        let curve = Curve(knots, isClosed)
        let outline = font.getDactylSansOutlines curve
        let svg, _, _ = font.elementToSvg outline
        String.concat " " svg
    with _ -> ""

let getGuidePositions (axes: Axes) =
    let m = FontMetrics(axes)
    let xg =
        [| {| name = "L"; value = m.L |}
           {| name = "C"; value = m.C |}
           {| name = "N"; value = m.N |}
           {| name = "R"; value = m.R |}
           {| name = "W"; value = m.W |} |]
    let yg =
        [| {| name = "B"; value = m.B |}
           {| name = "M"; value = m.M |}
           {| name = "X"; value = m.X |}
           {| name = "H"; value = m.H |}
           {| name = "T"; value = m.T |}
           {| name = "D"; value = m.D |} |]
    {| xGuides = xg; yGuides = yg |}

let getGlyphList () =
    GlyphStringDefs.glyphMap
    |> Map.toArray
    |> Array.map (fun (c, def) -> {| char = string c; def = def |})

let generateFontGlyphData (axes: Axes) =
    let fontAxes = { axes with outline = true; filled = true }
    let font = Font fontAxes
    let metrics = FontMetrics(axes)
    let chars = allChars.Replace("\n", "")

    let glyphs =
        chars
        |> Seq.map (fun c ->
            try
                let outline = font.CharToOutline c
                let svg, _, _ = font.elementToSvg outline
                {| unicode = int c
                   advanceWidth = font.charWidth c
                   pathData = String.concat " " svg |}
            with _ ->
                {| unicode = int c
                   advanceWidth = font.charWidth c
                   pathData = "" |})
        |> Array.ofSeq

    let thickness = float axes.thickness
    {| glyphs = glyphs
       ascender = metrics.T + thickness
       descender = metrics.D - thickness
       unitsPerEm = font.charHeight |}

let private knotToObj (k: Knot) : obj =
    // When x_fit/y_fit is true the solver treats the coordinate as a free variable (None).
    // Pass null (JS) / None (F#) so the spline editor matches the full font pipeline.
    let x_opt = if k.pt.x_fit then System.Nullable<float>() else System.Nullable<float>(k.pt.x)
    let y_opt = if k.pt.y_fit then System.Nullable<float>() else System.Nullable<float>(k.pt.y)
    {| ty = int (spiroToSplinePointType k.ty)
       x = x_opt
       y = y_opt
       th_in = k.th_in |> Option.toNullable
       th_out = k.th_out |> Option.toNullable
       label = k.label |> Option.defaultValue "" |} :> obj

let parseGlyphToControlPoints (char: string) (axes: Axes) =
    let c = char.[0]
    match GlyphStringDefs.glyphMap.TryFind c with
    | None -> [||]
    | Some def ->
        let glyph = FontMetrics(axes)
        let elem = GlyphStringDefs.stringDefsToElem glyph c false
        let rec extractCurves (e: Element) =
            match e with
            | Curve(knots, isClosed) ->
                let pts = knots |> List.toArray |> Array.map knotToObj
                [| {| isClosed = isClosed; points = pts |} |]
            | Dot(p) ->
                let pt =
                    {| ty = 0; x = p.x; y = p.y; x_fit = p.x_fit; y_fit = p.y_fit
                       th_in = System.Nullable<float>(); th_out = System.Nullable<float>()
                       label = "" |} :> obj
                [| {| isClosed = false; points = [| pt |] |} |]
            | EList(elems) -> elems |> List.toArray |> Array.collect extractCurves
            | Space -> [||]
            | Glyph _ -> [||]
        extractCurves elem


let generateVisualDiffsSvg (text: string) (axes: Axes) (progress: (float -> unit) option) =

    let fontOff =
        Font
            { axes with
                debug = false
                dactyl_spline = false
                spline2 = true }

    let fontOn =
        Font
            { axes with
                debug = false
                dactyl_spline = true
                spline2 = false }


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
              "<text x='0' y='%.0f' font-size='%s' fill='black'>Key:\nLeft = Old Spline, Middle = New Spline, Right = Overlaid Diff (Red=Old, Blue=New)</text>"
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
