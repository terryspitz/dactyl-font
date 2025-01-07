// Copyright 2018 Raph Levien

// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Utilities for representing and manipulating bezier paths.

module BezPath


open System
open System.Globalization
open System.Text



/// <summary>
/// Format double value using en-GB culture info.
/// </summary>
/// <param name="value"></param>
/// <returns></returns>
let Format (value : float) =
    if abs value < 1e-3 then
        "0"
    else
        // value.ToString("F1", ToStringCulture)
        value.ToString("F3")
        // sprintf "%d" (int value)
        // (int value).ToString()

/// <summary>
/// Bezier context implementation that handles the creation of Path data representation of bézier splines.
/// </summary>
type BezPath() =

    let mutable _needToClose = false
    let _sb = StringBuilder()

    /// <summary>
    /// Get output Path data string format.
    /// </summary>
    /// <returns>The Path data string format.</returns>
    member this.tostring() =
        _sb.ToString()

    member this.moveto(x, y) =
        _sb.AppendLine(sprintf "M %s,%s" (Format x) (Format y)) |> ignore

    member this.lineto(x, y) =
        _sb.AppendLine(sprintf "L %s,%s" (Format x) (Format y)) |> ignore

    member this.curveto(p1x, p1y, p2x, p2y, x, y) =
        let curve = sprintf "C %s,%s %s,%s %s,%s" (Format p1x) (Format p1y) (Format p2x) (Format p2y) (Format x) (Format y)
        _sb.AppendLine(curve) |> ignore

    member this.closepath() =
        _sb.AppendLine("Z")  |> ignore

    member this.mark(i) =
        ()
