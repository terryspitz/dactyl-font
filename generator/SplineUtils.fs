module SplineUtils

open Curves
open Axes
open DactylSpline
open GeneratorTypes
open Generator
open SpiroPointType


let splineToSpiroPointType ty = 
    match ty with
    | SplinePointType.Corner -> SpiroPointType.Corner
    | SplinePointType.Smooth -> SpiroPointType.G4
    | SplinePointType.LineToCurve -> SpiroPointType.Right
    | SplinePointType.CurveToLine -> SpiroPointType.Left
    | _ -> SpiroPointType.Corner

let spline2PtsToSvg (spline2Font: Font) ctrlPts = 
    spline2Font.Spline2ptsToSvg 
        (ctrlPts |> Array.map (fun pt -> (YX(int pt.y.Value, int pt.x.Value), splineToSpiroPointType pt.ty)) |> List.ofArray) false

let splineStaticPage() = 
    let curves = 10

    // left point theta rotates 0-180
    let single_curve_rotate_theta = 
        [for i in 0..curves do
            printfn "spline %d" i
            let one_example ctrlPts i x = 
                let spline = DSpline(ctrlPts, false)
                let debug: bool = false
                let spline2Font = Font({Axes.DefaultAxes with spline2=true})
                [ 
                    sprintf "<g id='%d'>" i
                    sprintf "<text x='%d' y='%d' font-size='0.2'>%d</text>" -1 (i+1) i
                    sprintf "<path d='"
                ] @ (spline.solveAndRender(20, debug)) @ [
                    sprintf "' transform='translate(%d,%d) scale(0.9, 0.9)'" x (i+1)
                    "style='fill:none;stroke:#000000;stroke-width:0.1'/>"
                    "</g>"
                    sprintf "<g id='%d'>" (i+1000)
                    sprintf "<path d='"
                ] @
                    spline2PtsToSvg spline2Font spline.ctrlPts
                @ [
                    sprintf "'"
                    sprintf "transform='translate(%d,%d) scale(0.9, 0.9)'" x (i+1)
                    "style='fill:none;stroke:#40000060;stroke-width:0.1'/>"
                    "</g>"
                ]
            let ctrlPts = [|
                {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI * float(i)/float(curves))};
                {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
            |]
            yield! one_example ctrlPts i 1
            let ctrlPts2 = [|
                {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI * float(i)/float(curves))};
                {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (PI * float(i)/float(curves))};
            |]
            yield! one_example ctrlPts2 i 2
            let ctrlPts3 = [|
                {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI * float(i)/float(curves))};
                {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=Some (PI * float(-i)/float(curves))};
            |]
            yield! one_example ctrlPts3 i 3
            let ctrlPts3 = [|
                {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
                {ty=SplinePointType.LineToCurve; x=Some 0.2; y=Some 0.; th=None};
                {ty=SplinePointType.Corner; x=Some 1.; y=Some (float(i)/float(curves)); th=None};
            |]
            yield! one_example ctrlPts3 i 4
        ]

    let show_iterations =
        [for i in 0..curves do
            printfn "spline %d" i
            let splineOf (spline : DSpline) x =
                let spline2Font = Font({Axes.DefaultAxes with spline2=true})
                [
                    sprintf "<g id='%d'>" i
                    let debug: bool = (i=0)
                    sprintf "<path d='"
                ] @ (spline.solveAndRender(i*3, false)) @ [
                    sprintf "' transform='translate(%d,%d) scale(0.9, 0.9)'" x (i+1)
                    "style='fill:none;stroke:#000000;stroke-width:0.05'/>"
                    "</g>"
                // Old spline2
                //     sprintf "<g id='%d'>" (i+1000)
                //     sprintf "<path d='"
                // ] @ spline2PtsToSvg spline2Font spline.ctrlPts @ [
                //     sprintf "'"
                //     sprintf "transform='translate(%d,%d)'" x (i+1)
                //     "style='fill:none;stroke:#40000060;stroke-width:0.1'/>"
                //     "</g>"
                ]
            yield! splineOf (DSpline([|
                {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
                {ty=SplinePointType.Smooth; x=Some 0.5; y=Some 0.8; th=None};
                {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
            |], false)) 5
            yield! splineOf (DSpline([|
                {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.7; th=None};
                {ty=SplinePointType.Smooth; x=Some 0.5; y=Some 1.; th=None};
                {ty=SplinePointType.Smooth; x=Some 0.; y=Some 0.5; th=None};
                {ty=SplinePointType.Smooth; x=Some 0.5; y=Some 0.; th=None};
                {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.3; th=None};
            |], false)) 6
        ]

    let show_char_iterations =
        //TODO: chars too big, need to scale down
        [for i in 0..curves do
            let font = Font({Axes.DefaultAxes with dactyl_spline=true; max_spline_iter=i})
            yield! (font.charToSvg 'c' 6 i black)
        ]


    single_curve_rotate_theta @ 
        show_iterations
