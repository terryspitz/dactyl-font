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

module BandMatrix

/// <summary>
/// The band matrix.
/// </summary>
type BandMatrix () = 
    member val a = Array.create 11 0.0 with get, set
    member val al = Array.create 5 0.0 with get, set

    override this.ToString() = sprintf "%A\n%A" this.a this.al

    /// <summary>
    /// The source band matrix.
    /// </summary>
    /// <param name="src">The source band matrix.</param>
    /// <param name="srcIndex">The source band matrix start element index.</param>
    /// <param name="dst">The destination band matrix.</param>
    /// <param name="dstIndex">The destination band matrix start element index.</param>
    /// <param name="length">Number of elements to copy from source band matrix.</param>
    static member Copy (src : BandMatrix array) srcIndex (dst : BandMatrix array) dstIndex length =
        for i in 0..length-1 do
            System.Array.Copy(src.[i + srcIndex].a, 0, dst.[i + dstIndex].a, 0, 11);
            System.Array.Copy(src.[i + srcIndex].al, 0, dst.[i + dstIndex].al, 0, 5);
