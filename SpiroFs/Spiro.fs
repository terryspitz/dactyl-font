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

module Spiro

open SpiroImpl
open SpiroPointType

/// <summary>
/// C# implementation of third-order polynomial spirals.
/// Interface routines for Raph's spiro package.
/// </summary>


/// <summary>
/// Convert a set of spiro control points into a set of bézier curves.
/// 
/// As it does so it will call the appropriate routine in your bézier context with this information 
/// – this should allow you to create your own internal representation of those curves.
/// 
/// Open contours do not need to start with '{', nor to end with '}'.
/// 
/// Close contours do not need to end with 'z'.
/// 
/// This function is kept for backwards compatibility for older programs. 
/// Please use the function that return success/failure replies when done.	
/// </summary>
/// <param name="spiros">An array of input spiros.</param>
/// <param name="n">The number of elements in the spiros array.</param>
/// <param name="isClosed">Whether this describes a closed (True) or open (False) contour.</param>
/// <returns>SpiroSegment array or null on failure.</returns>
/// <example>
/// var points = new SpiroControlPoint[4];
/// points[0].X = -100; points[0].Y = 0; points[0].Type = SpiroPointType.G4;
/// points[1].X = 0; points[1].Y = 100; points[1].Type = SpiroPointType.G4;
/// points[2].X = 100; points[2].Y = 0; points[2].Type = SpiroPointType.G4;
/// points[3].X = 0; points[3].Y = -100; points[3].Type = SpiroPointType.G4;
/// var bc = new PathBezierContext();
/// var success = Spiro.SpiroCPsToBezier0(points, 4, true, bc);
/// Console.WriteLine(bc);
/// Console.WriteLine("Success: {0} ", success);
/// </example>
let SpiroCPsToSegments spiros n isClosed =
    if n <= 0 then
        None
    elif isClosed then
        SpiroImpl.run_spiro spiros n
    else
        let newspiros = Array.copy spiros
        newspiros.[0] <- {newspiros.[0] with Type = SpiroPointType.OpenContour}
        newspiros.[n - 1] <- {newspiros.[n - 1] with Type = SpiroPointType.EndOpenContour}
        SpiroImpl.run_spiro spiros n


/// <summary>
/// Convert a set of spiro control points into a set of bézier curves.
/// 
/// As it does so it will call the appropriate routine in your bézier context with this information 
/// – this should allow you to create your own internal representation of those curves.
/// 
/// Open contours do not need to start with '{', nor to end with '}'.
/// 
/// Close contours do not need to end with 'z'.
/// </summary>
/// <param name="spiros">An array of input spiros.</param>
/// <param name="n">The number of elements in the spiros array.</param>
/// <param name="isClosed">Whether this describes a closed (True) or open (False) contour.</param>
/// <param name="bc">A bézier results output context.</param>
/// <returns>An boolean success flag. True = completed task and have valid bézier results, or False = unable to complete task, bézier results are invalid.</returns>
/// <example>
/// var points = new SpiroControlPoint[4];
/// points[0].X = -100; points[0].Y = 0; points[0].Type = SpiroPointType.G4;
/// points[1].X = 0; points[1].Y = 100; points[1].Type = SpiroPointType.G4;
/// points[2].X = 100; points[2].Y = 0; points[2].Type = SpiroPointType.G4;
/// points[3].X = 0; points[3].Y = -100; points[3].Type = SpiroPointType.G4;
/// var bc = new PathBezierContext();
/// var success = Spiro.SpiroCPsToBezier0(points, 4, true, bc);
/// Console.WriteLine(bc);
/// Console.WriteLine("Success: {0} ", success);
/// </example>
let SpiroCPsToBezier spiros n isClosed bc =

    let s = SpiroCPsToSegments spiros n isClosed
    if s.IsSome then
        SpiroImpl.spiro_to_bpath s.Value n bc
        true
    else
        false
