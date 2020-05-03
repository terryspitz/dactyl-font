// /*
// SpiroNet.Editor
// Copyright (C) 2019 Wiesław Šoltés

// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General License
// as published by the Free Software Foundation; either version 3
// of the License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General License for more details.

// You should have received a copy of the GNU General License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
// 02110-1301, USA.

// */

module PathBezierContext

open System
open System.Globalization
open System.Text

open IBezierContext


let ToStringCulture = CultureInfo("en-GB")

/// <summary>
/// Format double value using en-GB culture info.
/// </summary>
/// <param name="value"></param>
/// <returns></returns>
let Format (value : float) =
    value.ToString("F1", ToStringCulture)

/// <summary>
/// Bezier context implementation that handles the creation of Path data representation of bézier splines.
/// </summary>
/// <remarks>
/// Internally class used StringBuilder object to append generated Path data.
/// </remarks>
type PathBezierContext() =

    let mutable _needToClose = false
    let _sb = StringBuilder()

    /// <summary>
    /// Get output Path data string format.
    /// </summary>
    /// <returns>The Path data string format.</returns>
    member this.GetData =
        if _needToClose then
            _sb.Append("Z") |> ignore
            _needToClose <- false
        _sb.ToString()


    /// <summary>
    /// Get output Path data string format.
    /// </summary>
    /// <returns>The Path data string format.</returns>
    member this.ToString = this.GetData

    interface IBezierContext with 

        /// <summary>
        /// Start a contour.
        /// </summary>
        /// <param name="x">The X coordinate of the new start point.</param>
        /// <param name="y">The Y coordinate of the new start point.</param>
        /// <param name="isOpen">An boolean flag indicating wheter spline is open (True) or closed (False).</param>
        member this.MoveTo(x, y, isOpen) =
            if _needToClose then
                _sb.AppendLine("Z")  |> ignore

            let move = String.Format("M {0},{1}", Format(x), Format(y))
            _sb.AppendLine(move) |> ignore
            _needToClose <- not isOpen


        /// <summary>
        /// Move from the last point to the next one on a straight line.
        /// </summary>
        /// <param name="x">The X coordinate of the new end point.</param>
        /// <param name="y">The Y coordinate of the new end point.</param>
        member this.LineTo(x, y) =
            let line = String.Format("L {0},{1}", Format(x), Format(y))
            _sb.AppendLine(line) |> ignore


        /// <summary>
        /// Move from the last point to the next along a quadratic bezier spline.
        /// </summary>
        /// <param name="x1">The X coordinate of quadratic bezier bezier control point.</param>
        /// <param name="y1">The Y coordinate of quadratic bezier bezier control point.</param>
        /// <param name="x2">The X coordinate of the new end point.</param>
        /// <param name="y2">The Y coordinate of the new end point.</param>
        member this.QuadTo(x1, y1, x2, y2) =
            let quad = String.Format("Q {0},{1} {2},{3}", Format(x1), Format(y1), Format(x2), Format(y2))
            _sb.AppendLine(quad) |> ignore


        /// <summary>
        /// Move from the last point to the next along a cubic bezier spline.
        /// </summary>
        /// <param name="x1">The X coordinate of first cubic bezier spline off-curve control point.</param>
        /// <param name="y1">The Y coordinate of first cubic bezier spline off-curve control point.</param>
        /// <param name="x2">The X coordinate of second cubic bezier spline off-curve control point.</param>
        /// <param name="y2">The Y coordinate of second cubic bezier spline off-curve control point.</param>
        /// <param name="x3">The X coordinate of the new end point.</param>
        /// <param name="y3">The Y coordinate of the new end point.</param>
        member this.CurveTo(x1, y1, x2, y2, x3, y3) =
            let curve = String.Format("C {0},{1} {2},{3} {4},{5}", Format(x1), Format(y1), Format(x2), Format(y2), Format(x3), Format(y3))
            _sb.AppendLine(curve) |> ignore


        /// <summary>
        /// Mark current control point knot. 
        /// Currenlty not implemented, may be usefull for marking generated curves to original spiro code points.
        /// </summary>
        /// <param name="index">The spiros control point knot index.</param>
        /// <param name="theta">The spiros control point knot theta angle.</param>
        /// <param name="x">The spiros control point X location.</param>
        /// <param name="y">The spiros control point Y location.</param>
        /// <param name="_type">The spiros control point type.</param>
        member this.MarkKnot(index, theta, x, y, _type) =
            ()
