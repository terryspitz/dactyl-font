module VisualTests

open Curves
open Axes
open DactylSpline
open GeneratorTypes
open Font
open SpiroPointType


let splineToSpiroPointType ty =
    match ty with
    | SplinePointType.Corner -> SpiroPointType.Corner
    | SplinePointType.Smooth -> SpiroPointType.G4
    | SplinePointType.LineToCurve -> SpiroPointType.Right
    | SplinePointType.CurveToLine -> SpiroPointType.Left
    | _ -> SpiroPointType.Corner

let SPLINE2SCALE = 10.
let dcp = DactylSpline.dcp

let spline2PtsToSvg (spline2Font: Font) ctrlPts =
    let spline2CtrlPts =
        [ for pt in ctrlPts do
              (YX(int (SPLINE2SCALE * pt.y.Value), int (SPLINE2SCALE * pt.x.Value)), splineToSpiroPointType pt.ty) ]

    let (svg, _, _) = spline2Font.Spline2PtsToSvg spline2CtrlPts false
    svg

let splineStaticPage () =
    let curves = 10

    // left point theta rotates 0-180
    let single_curve_rotate_theta =
        let max_iter = 1000

        let one_example ctrlPts i x =
            try
                let spline = DactylSpline(ctrlPts, false)
                let debug: bool = false
                let spline2Font = Font({ Axes.DefaultAxes with spline2 = true })

                [ sprintf "<g id='%d'>" i
                  sprintf "<text x='%f' y='%d' font-size='0.2'>%d</text>" 0.5 (i + 1) i
                  sprintf "<path d='" ]
                @ let svg, _, _ = spline.solveAndRenderSvg (max_iter, 1.0, debug, false, false) in

                  svg
                  @ [ sprintf "' transform='translate(%d,%d) scale(0.9, 0.9)'" x (i + 1)
                      "style='fill:none;stroke:#000000;stroke-width:0.1'/>"
                      "</g>"
                      sprintf "<g id='%d'>" (i + 1000)
                      sprintf "<path d='" ]
                  @ spline2PtsToSvg spline2Font spline.ctrlPts
                  @ [ sprintf "'"
                      sprintf
                          "transform='translate(%d,%d) scale(%f, %f)'"
                          x
                          (i + 1)
                          (0.9 / SPLINE2SCALE)
                          (0.9 / SPLINE2SCALE)
                      "style='fill:none;stroke:#40000060;stroke-width:1'/>"
                      "</g>" ]
            with ex ->
                printfn "ERROR in one_example (i=%d, x=%d): %s" i x ex.Message
                []

        [ for i in 0..curves do
              printfn "one_example %d" i

              // left point theta rotates 0-180
              let ctrlPts =
                  [| dcp SplinePointType.Corner 0. 0. (Some(PI * float (i) / float (curves)))
                     dcp SplinePointType.Corner 1. 0. None |]

              yield! one_example ctrlPts i 1

              // both points theta rotate 0-180
              let ctrlPts2 =
                  [| dcp SplinePointType.Corner 0. 0. (Some(PI * float (i) / float (curves)))
                     dcp SplinePointType.Corner 1. 0. (Some(PI * float (i) / float (curves))) |]

              yield! one_example ctrlPts2 i 2

              // left point theta rotates 0-180, right point theta rotates 0 to -180
              let ctrlPts3 =
                  [| dcp SplinePointType.Corner 0. 0. (Some(PI * float (i) / float (curves)))
                     dcp SplinePointType.Corner 1. 0. (Some(PI * float (-i) / float (curves))) |]

              yield! one_example ctrlPts3 i 3

              // f-shape
              let ctrlPts4 =
                  [| dcp SplinePointType.Corner 0. 0. None
                     dcp SplinePointType.LineToCurve 0.2 0. None
                     dcp SplinePointType.Corner 1. (float (i) / float (curves)) None |]

              yield! one_example ctrlPts4 i 4 ]

    let show_iterations =
        [ for i in 0..curves do
              printfn "show_iterations %d" i

              let splineOf (spline: DactylSpline) x =
                  let spline2Font = Font({ Axes.DefaultAxes with spline2 = true })
                  let SHOW_LEGACY_SPLINE = false

                  try
                      let debug: bool = false // (i = 0)
                      let iter = i * 3
                      let scale = if x >= 6 then 0.0009 else 0.9
                      let strokeWidth = if x >= 6 then 50.0 else 0.05
                      // let iter = if x >= 7 then max 1 (i * 50) else max 1 (i * 3)

                      [ sprintf "<g id='%d'>" i; sprintf "<path d='" ]
                      @ let svg, _, _ = spline.solveAndRenderSvg (iter, 1.0, debug, false, false) in

                        svg
                        @ [ sprintf "' transform='translate(%d,%d) scale(%f, %f)'" x (i + 1) scale scale
                            sprintf "style='fill:none;stroke:#000000;stroke-width:%f'/>" strokeWidth
                            "</g>" ]
                        @ (if SHOW_LEGACY_SPLINE then
                               [ sprintf "<g id='%d'>" (i + 1000); sprintf "<path d='" ]
                               @ spline2PtsToSvg spline2Font spline.ctrlPts
                               @ [ sprintf "'"
                                   sprintf "transform='translate(%d,%d)'" x (i + 1)
                                   "style='fill:none;stroke:#40000060;stroke-width:0.1'/>"
                                   "</g>" ]
                           else
                               [])
                  with ex ->
                      printfn "ERROR in splineOf (i=%d, x=%d): %s" i x ex.Message
                      []

              // U-shape
              yield!
                  splineOf
                      (DactylSpline(
                          [| dcp SplinePointType.Corner 0. 0. None
                             dcp SplinePointType.Smooth 0.5 0.8 None
                             dcp SplinePointType.Corner 1. 0. None |],
                          false
                      ))
                      5

              // C-shape
              yield!
                  splineOf
                      (DactylSpline(
                          [| dcp SplinePointType.Corner 1000. 700. None
                             dcp SplinePointType.Smooth 500. 1000. None
                             dcp SplinePointType.Smooth 0. 500. None
                             dcp SplinePointType.Smooth 500. 0. None
                             dcp SplinePointType.Corner 1000. 300. None |],
                          false
                      ))
                      6

              // quarter circle
              let s45 = 1000.0 * sin (PI / 4.0)

              yield!
                  splineOf
                      (DactylSpline(
                          [| dcp SplinePointType.Smooth 1000.0 0.0 (Some(PI / 2.0))
                             dcp SplinePointType.Smooth s45 s45 None
                             dcp SplinePointType.Smooth 0.0 1000.0 (Some(PI)) |],
                          false
                      ))
                      7


              // Asymmetric fit
              // tl -> hl -> b(c) -> tr
              yield!
                  splineOf
                      (DactylSpline(
                          [| dcp SplinePointType.Smooth 0.0 0.0 None // tl
                             dcp SplinePointType.LineToCurve 0.0 700.0 None // hl
                             { ty = SplinePointType.Smooth
                               x = None
                               y = Some 1000.0
                               th = Some 0.0 }
                             dcp SplinePointType.Smooth 1000.0 0.0 None |],
                          false
                      ))
                      8 ]

    let show_char_iterations =
        //TODO: chars too big, need to scale down
        [ for i in 0..curves do
              let font =
                  Font(
                      { Axes.DefaultAxes with
                          dactyl_spline = true
                          max_spline_iter = i }
                  )

              yield! (font.charToSvg 'c' 6 i black) ]


    [ sprintf "<text x='1' y='0.5' font-size='0.5' fill='black'>Dactyl Spline test</text>"
      sprintf "<text x='1' y='0.8' font-size='0.15' fill='blue'>theta left</text>"
      sprintf "<text x='2' y='0.8' font-size='0.15' fill='blue'>theta opp</text>"
      sprintf "<text x='3' y='0.8' font-size='0.15' fill='blue'>theta same</text>"
      sprintf "<text x='4' y='0.8' font-size='0.15' fill='blue'>f-shape</text>"
      sprintf "<text x='5' y='0.8' font-size='0.15' fill='blue'>u-iterations</text>"
      sprintf "<text x='6' y='0.8' font-size='0.15' fill='blue'>c-iterations</text>"
      sprintf "<text x='7' y='0.8' font-size='0.15' fill='blue'>Quarter Circle</text>"
      sprintf "<text x='8' y='0.8' font-size='0.15' fill='blue'>Asymmetric</text>" ]
    @ single_curve_rotate_theta
    @ show_iterations
// @ show_char_iterations
