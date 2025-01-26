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
    let rotate_left_theta = 
        [for i in 0..curves do
            printfn "spline %d" i
            let ctrlPts = [|
                {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=Some (PI * float(i)/float(curves))};
                {ty=SplinePointType.Corner; x=Some 1.; y=Some 0.; th=None};
            |]
            let spline = DSpline(ctrlPts, false)
            yield sprintf "<g id='%d'>" i
            yield sprintf "<text x='%d' y='%d' font-size='0.2'>%d</text>" -1 (i+1) i
            let debug: bool = false
            yield sprintf "<path d='%s'" (spline.solve(20, debug))
            yield sprintf "transform='translate(%d,%d)'" 1 (i+1)
            yield "style='fill:none;stroke:#000000;stroke-width:0.1'/>"
            yield "</g>"

            let spline2Font = Font({Axes.DefaultAxes with spline2=true})
            yield sprintf "<g id='%d'>" (i+1000)
            yield sprintf "<path d='"
            yield! spline2PtsToSvg spline2Font spline.ctrlPts
            yield sprintf "'"
            yield sprintf "transform='translate(%d,%d)'" 1 (i+1)
            yield "style='fill:none;stroke:#40000060;stroke-width:0.1'/>"
            yield "</g>"
        ]

    let show_iterations =
        [for i in 0..curves do
            printfn "spline %d" i
            let th_i = 0.5 * PI
            let spline = DSpline([|
                {ty=SplinePointType.Corner; x=Some 0.; y=Some 0.; th=None};
                {ty=SplinePointType.Smooth; x=Some 1.; y=Some 1.; th=None};
                {ty=SplinePointType.Corner; x=Some 2.; y=Some 0.; th=None};
            |], false)
            yield sprintf "<g id='%d'>" i
            // yield sprintf "<text x='%d' y='%d' font-size='0.2'>%d</text>" -3 (i+1) i
            let debug: bool = (curves=5)
            yield sprintf "<path d='%s'" (spline.solve(i*2, debug))
            yield sprintf "transform='translate(%d,%d)'" 3 (i+1)
            yield "style='fill:none;stroke:#00000080;stroke-width:0.1'/>"
            yield "</g>"

            let spline2Font = Font({Axes.DefaultAxes with spline2=true})
            yield sprintf "<g id='%d'>" (i+1000)
            yield sprintf "<path d='"
            yield! spline2PtsToSvg spline2Font spline.ctrlPts
            yield sprintf "'"
            yield sprintf "transform='translate(%d,%d)'" 3 (i+1)
            yield "style='fill:none;stroke:#40000060;stroke-width:0.1'/>"
            yield "</g>"
        ]

    rotate_left_theta @ 
        show_iterations
