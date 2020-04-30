// /*
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

// */

module SpiroSegment

open SpiroPointType
open SpiroControlPoint

/// <summary>
/// The run_spiro() uses array of information given in the spiro control point structure 
/// and creates an array in this structure format to use by spiro_to_bpath for building bezier curves.
/// </summary>
type SpiroSegment = {
    /// <summary>
    /// Spiro code point segment_chord startX.
    /// </summary>
    X : float
    /// <summary>
    /// Spiro code point segment_chord startY.
    /// </summary>
    Y : float
    /// <summary>
    /// Spiro code point Type.
    /// </summary>
    mutable Type : SpiroPointType
    /// <summary>
    /// Bend theta between this vector and next vector.
    /// </summary>
    mutable bend_th : float
    /// <summary>
    /// A double's array of size 4 containing curvature and its first 3 derivatives
    /// </summary>
    mutable ks : float[]
    /// <summary>
    /// The segment_chord distance from xy to next spiro code point.
    /// </summary>
    mutable seg_ch : float
    /// <summary>
    /// The segment_theta angle for this spiro code point.
    /// </summary>
    mutable seg_th : float
}

let spiroSegment (s : SpiroControlPoint) =
    {
        X = s.X
        Y = s.Y
        Type = s.Type
        bend_th = 0.0
        ks = Array.zeroCreate 4
        seg_ch = 0.0
        seg_th = 0.0
    }
