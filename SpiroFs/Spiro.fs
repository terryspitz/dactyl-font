// libspiro - conversion between spiro control points and bezier's
// Copyright (C) 2007 Raph Levien
//               2019 converted to C# by Wiesław Šoltés

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

module Spiro

open SpiroImpl
open SpiroPointType

/// <summary>
/// F# implementation of third-order polynomial spirals.
/// Interface routines for Raph's spiro package.
/// </summary>


/// <summary>
/// Convert a set of spiro control points into a list of SpiroSegments.
/// </summary>
/// <param name="spiros">An array of input spiros.</param>
/// <param name="isClosed">Whether this describes a closed (True) or open (False) contour.</param>
/// <returns>SpiroSegment array or null on failure.</returns>
let SpiroCPsToSegments spiros isClosed =
    SpiroImpl.run_spiro spiros isClosed


/// <summary>
/// Convert spiro segment list into bezier curves, using the output of SpiroCPsToSegments.
/// </summary>
let SpirosToBezier (segs : SpiroSegment.SpiroSegment[]) isClosed bc =
    let nSeg = if isClosed then segs.Length - 1 else segs.Length
    if not isClosed then
        segs.[0].Type <- SpiroPointType.OpenContour
        segs.[segs.Length-1].Type <- SpiroPointType.EndOpenContour
    SpiroImpl.spiro_to_bpath segs nSeg bc
