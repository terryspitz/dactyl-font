module Api

open Fable.Core
open Axes
open Generator
open GeneratorTypes
open GlyphStringDefs
open GlyphFsDefs

// Helper to convert the union type Controls to a JS-friendly object
let getControlDetails (name: string, control: Controls) =
    match control with
    | Range(min, max) -> 
        {| name = name; type_ = "range"; min = float min; max = float max; step = (float max - float min) / 20.0 |}
    | FracRange(min, max) -> 
        {| name = name; type_ = "range"; min = min; max = max; step = 0.05 |}
    | Checkbox -> 
        {| name = name; type_ = "checkbox"; min = 0.0; max = 1.0; step = 1.0 |}

let controlDefinitions = 
    Axes.controls 
    |> List.map getControlDetails
    |> Array.ofList

let defaultAxes = Axes.DefaultAxes

let generateSvg (text: string) (axes: Axes) = 
    let font = Font axes
    let lines = 
        if System.String.IsNullOrEmpty(text) then [] 
        else text.Split('\r', '\n') |> List.ofArray
    
    // Using the same parameters as explorer.fs: 0 0 false black
    // You might want to make these configurable later
    font.stringToSvg lines 0 0 false "black" 
    |> String.concat "\n"
