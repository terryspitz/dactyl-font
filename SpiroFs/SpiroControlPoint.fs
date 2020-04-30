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

module SpiroControlPoint

open SpiroPointType

/// <summary>
/// User passes an array of spiros control points in this format for Spiro to solve.
/// </summary>
type SpiroControlPoint (X: float, Y: float, Type: SpiroPointType) =

    /// <summary>
    /// Spiro code point X location.
    /// </summary>
    member this.X = X

    /// <summary>
    /// Spiro code point Y location.
    /// </summary>
    member this.Y = Y

    /// <summary>
    /// Spiro code point Type.
    /// </summary>
    member this.Type = Type

    /// <summary>
    /// Returns string rendering of object.
    /// </summary>
    member this.ToString =

        this.X.ToString() + ", " + this.Y.ToString() + ", " + this.Type.ToString();
