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

/// <summary>
/// C# implementation of third-order polynomial spirals.
/// Internal implementation of spiro using ORDER equal to 12.
/// </summary>
module SpiroImpl

//TODO incr

open System.Diagnostics.CodeAnalysis
open System

open Arrays
open BandMatrix
open SpiroSegment
open SpiroPointType
open SpiroControlPoint
open IBezierContext


/// <summary>
/// Compute hypotenuse.
/// </summary>
/// <param name="x">The X floating point value corresponding to the legs of a right-angled triangle for which the hypotenuse is computed.</param>
/// <param name="y">The Y floating point value corresponding to the legs of a right-angled triangle for which the hypotenuse is computed.</param>
/// <returns>Returns the hypotenuse of a right-angled triangle whose legs are x and y.</returns>
let hypot x y =
    sqrt (x * x + y * y)


/// <summary>
/// Returns whether x is a finite value.
/// A finite value is any floating-point value that is neither infinite nor NaN (Not-A-Number).
/// 
/// IsFinite() equivalent:
/// http://stackoverflow.com/questions/10030070/isfinite-equivalent
/// References:
/// http://pubs.opengroup.org/onlinepubs/009604499/functions/isfinite.html
/// http://msdn.microsoft.com/en-us/library/system.double.isinfinity.aspx
/// http://msdn.microsoft.com/en-us/library/system.double.isnan.aspx
/// </summary>
/// <param name="x">A floating-point value.</param>
/// <returns>True if x is finite; and False otherwise.</returns>
let IsFinite x =
    not (System.Double.IsInfinity x) && not (System.Double.IsNaN x)



// Integrate polynomial spiral curve over range -.5 .. .5.
//[<SuppressMessage("Hints", "FL0049") >]
let integrate_spiro (ks : float[]) =
    let n = 4
    
    let th1 = ks.[0]
    let th2 = 0.5 * ks.[1]
    let th3 = (1.0 / 6.0) * ks.[2]
    let th4 = (1.0 / 24.0) * ks.[3]
    let mutable x = 0.0
    let mutable y = 0.0
    let ds = 1.0 / float(n)
    let ds2 = ds * ds
    let ds3 = ds2 * ds
    let k0 = ks.[0] * ds
    let k1 = ks.[1] * ds
    let k2 = ks.[2] * ds
    let k3 = ks.[3] * ds
    let mutable s = 0.5 * ds - 0.5

    let mutable x = 0.0
    let mutable y = 0.0

    for i in 0..n-1 do
        let mutable u = 0.0
        let mutable v = 0.0
        let mutable km0 = 0.0
        let mutable km1 = 0.0
        let mutable km2 = 0.0
        let mutable km3 = 0.0

        if n = 1 then
            km0 <- k0
            km1 <- k1 * ds
            km2 <- k2 * ds2
        else
            km0 <- (((1.0 / 6.0) * k3 * s + 0.5 * k2) * s + k1) * s + k0
            km1 <- ((0.5 * k3 * s + k2) * s + k1) * ds
            km2 <- (k3 * s + k2) * ds2

        km3 <- k3 * ds3

        let t1_1 = km0
        let t1_2 = 0.5 * km1
        let t1_3 = (1.0 / 6.0) * km2
        let t1_4 = (1.0 / 24.0) * km3
        let t2_2 = t1_1 * t1_1
        let t2_3 = 2.0 * (t1_1 * t1_2)
        let t2_4 = 2.0 * (t1_1 * t1_3) + t1_2 * t1_2
        let t2_5 = 2.0 * (t1_1 * t1_4 + t1_2 * t1_3)
        let t2_6 = 2.0 * (t1_2 * t1_4) + t1_3 * t1_3
        let t2_7 = 2.0 * (t1_3 * t1_4)
        let t2_8 = t1_4 * t1_4
        let t3_4 = t2_2 * t1_2 + t2_3 * t1_1
        let t3_6 = t2_2 * t1_4 + t2_3 * t1_3 + t2_4 * t1_2 + t2_5 * t1_1
        let t3_8 = t2_4 * t1_4 + t2_5 * t1_3 + t2_6 * t1_2 + t2_7 * t1_1
        let t3_10 = t2_6 * t1_4 + t2_7 * t1_3 + t2_8 * t1_2
        let t4_4 = t2_2 * t2_2
        let t4_5 = 2.0 * (t2_2 * t2_3)
        let t4_6 = 2.0 * (t2_2 * t2_4) + t2_3 * t2_3
        let t4_7 = 2.0 * (t2_2 * t2_5 + t2_3 * t2_4)
        let t4_8 = 2.0 * (t2_2 * t2_6 + t2_3 * t2_5) + t2_4 * t2_4
        let t4_9 = 2.0 * (t2_2 * t2_7 + t2_3 * t2_6 + t2_4 * t2_5)
        let t4_10 = 2.0 * (t2_2 * t2_8 + t2_3 * t2_7 + t2_4 * t2_6) + t2_5 * t2_5
        let t5_6 = t4_4 * t1_2 + t4_5 * t1_1
        let t5_8 = t4_4 * t1_4 + t4_5 * t1_3 + t4_6 * t1_2 + t4_7 * t1_1
        let t5_10 = t4_6 * t1_4 + t4_7 * t1_3 + t4_8 * t1_2 + t4_9 * t1_1
        let t6_6 = t4_4 * t2_2
        let t6_7 = t4_4 * t2_3 + t4_5 * t2_2
        let t6_8 = t4_4 * t2_4 + t4_5 * t2_3 + t4_6 * t2_2
        let t6_9 = t4_4 * t2_5 + t4_5 * t2_4 + t4_6 * t2_3 + t4_7 * t2_2
        let t6_10 = t4_4 * t2_6 + t4_5 * t2_5 + t4_6 * t2_4 + t4_7 * t2_3 + t4_8 * t2_2
        let t7_8 = t6_6 * t1_2 + t6_7 * t1_1
        let t7_10 = t6_6 * t1_4 + t6_7 * t1_3 + t6_8 * t1_2 + t6_9 * t1_1
        let t8_8 = t6_6 * t2_2
        let t8_9 = t6_6 * t2_3 + t6_7 * t2_2
        let t8_10 = t6_6 * t2_4 + t6_7 * t2_3 + t6_8 * t2_2
        let t9_10 = t8_8 * t1_2 + t8_9 * t1_1
        let t10_10 = t8_8 * t2_2
        u <- 1.0
        v <- 0.0
        v <- v + (1.0 / 12.0) * t1_2 + (1.0 / 80.0) * t1_4
        u <- u - (1.0 / 24.0) * t2_2 + (1.0 / 160.0) * t2_4 + (1.0 / 896.0) * t2_6 + (1.0 / 4608.0) * t2_8
        v <- v - (1.0 / 480.0) * t3_4 + (1.0 / 2688.0) * t3_6 + (1.0 / 13824.0) * t3_8 + (1.0 / 67584.0) * t3_10
        u <- u + (1.0 / 1920.0) * t4_4 + (1.0 / 10752.0) * t4_6 + (1.0 / 55296.0) * t4_8 + (1.0 / 270336.0) * t4_10
        v <- v + (1.0 / 53760.0) * t5_6 + (1.0 / 276480.0) * t5_8 + (1.0 / 1.35168e+06) * t5_10
        u <- u - (1.0 / 322560.0) * t6_6 + (1.0 / 1.65888e+06) * t6_8 + (1.0 / 8.11008e+06) * t6_10
        v <- v - (1.0 / 1.16122e+07) * t7_8 + (1.0 / 5.67706e+07) * t7_10
        u <- u + (1.0 / 9.28973e+07) * t8_8 + (1.0 / 4.54164e+08) * t8_10
        v <- v + (1.0 / 4.08748e+09) * t9_10
        u <- u - (1.0 / 4.08748e+10) * t10_10

        if n = 1 then
            x <- u
            y <- v
        else
            let th = (((th4 * s + th3) * s + th2) * s + th1) * s
            let cth = cos th
            let sth = sin th
            x <- x + cth * u - sth * v
            y <- y + cth * v + sth * u
            s <- s + ds

    (x * ds, y * ds)


//let compute_ends (ks : float[]) (ends : float[,]) (seg_ch : float) =
let compute_ends (ks : float[]) (seg_ch : float) =
    let x, y = integrate_spiro ks
    let ch = hypot x y
    let th = atan2 y x
    let l = ch / seg_ch

    let ends = Arrays.MyArray2D(2, 4)
    let th_even = 0.5 * ks.[0] + (1.0 / 48.0) * ks.[2]
    let th_odd = 0.125 * ks.[1] + (1.0 / 384.0) * ks.[3] - th
    ends.[(0,0)] <- th_even - th_odd
    ends.[(1,0)] <- th_even + th_odd
    let k0_even = l * (ks.[0] + 0.125 * ks.[2])
    let k0_odd = l * (0.5 * ks.[1] + (1.0 / 48.0) * ks.[3])
    ends.[(0,1)] <- k0_even - k0_odd
    ends.[(1,1)] <- k0_even + k0_odd
    let l2 = l * l
    let k1_even = l2 * (ks.[1] + 0.125 * ks.[3])
    let k1_odd = l2 * 0.5 * ks.[2]
    ends.[(0,2)] <- k1_even - k1_odd
    ends.[(1,2)] <- k1_even + k1_odd
    let l3 = l2 * l
    let k2_even = l3 * ks.[2]
    let k2_odd = l3 * 0.5 * ks.[3]
    ends.[(0,3)] <- k2_even - k2_odd
    ends.[(1,3)] <- k2_even + k2_odd
    
    ends //return

//let compute_pderivs (s : SpiroSegment) (ends : float[,]) (derivs : float[,,]) (jinc : int) : unit =
let compute_pderivs (s : SpiroSegment) (ends : MyArray2D) (derivs : MyArray3D) (jinc : int) : unit =
    let recip_d = 2e6
    let delta = 1.0 / recip_d
    let try_ks = Array.create 4 0.0
    //let try_ends = Array2D.create 2 4 0.0
    let try_ends = MyArray2D(2, 4)
  
    ends.CopyFrom(compute_ends s.ks s.seg_ch)
    
    for i in 0..jinc-1 do
        for j in 0..3 do
            try_ks.[j] <- s.ks.[j]

        try_ks.[i] <- try_ks.[i] + delta
        try_ends.CopyFrom(compute_ends try_ks s.seg_ch)

        for k in 0..1 do
            for j in 0..3 do
                derivs.[(j,k,i)] <- recip_d * (try_ends.[k,j] - ends.[k,j])


let mod_2pi th =
    let u = th / (2.0 * Math.PI)
    2.0 * Math.PI * (u - floor (u + 0.5))


let setup_path (src : SpiroControlPoint[]) n =

    // #if CHECK_INPUT_FINITENESS
    //     // Verify that input values are within realistic limits
    //     for (i = 0; i < n; i++)
    //         if (IsFinite(src.[i].X) = 0 || IsFinite(src.[i].Y) = 0)
    //             null
    // #endif

    let isOpen = src.[0].Type = SpiroPointType.OpenContour
    let n_seg = if isOpen then n - 1 else n
    let looped_src = if isOpen then src else Array.ofList (List.ofArray src @ [src.[0]])
    let r = Array.map spiroSegment looped_src
    assert (r.Length = (n_seg + 1))
    assert (r.[n_seg].X = src.[n_seg % n].X)

    for i in 0..n_seg-1 do
        let dx = r.[i + 1].X - r.[i].X
        let dy = r.[i + 1].Y - r.[i].Y
        //#if !CHECK_INPUT_FINITENESS
        r.[i].seg_ch <- hypot dx dy
        //#else
        //if (IsFinite(dx) || IsFinite(dy) || IsFinite((r.[i].seg_ch = hypot(dx, dy))))
        //    null
        //#endif
        r.[i].seg_th <- atan2 dy dx

    let mutable ilast = n_seg - 1

    for i in 0..n_seg-1 do
        if r.[i].Type = SpiroPointType.OpenContour || r.[i].Type = SpiroPointType.EndOpenContour || r.[i].Type = SpiroPointType.Corner then
            r.[i].bend_th <- 0.0
        else
            r.[i].bend_th <- mod_2pi(r.[i].seg_th - r.[ilast].seg_th)
        ilast <- i
    r

let bandec11 (m : BandMatrix[]) (perm : int[]) n =
    // pack top triangle to the left.
    for i in 0..4 do
        for j in 0..i+5 do
            m.[i].a.[j] <- m.[i].a.[j + 5 - i]

        for j in (i+6)..10 do
            m.[i].a.[j] <- 0.0

    let mutable l = 5
    let mutable pivot = 0
    let mutable pivot_val = 0.0

    for k in 0..n-1 do
        pivot <- k
        pivot_val <- m.[k].a.[0]

        l <- if l < n then l + 1 else n

        for j in k + 1..l-1 do
            if abs m.[j].a.[0] > abs pivot_val then
                pivot_val <- m.[j].a.[0]
                pivot <- j

        perm.[k] <- pivot

        if pivot <> k then
            for j in 0..10 do
                let tmp = m.[k].a.[j]
                m.[k].a.[j] <- m.[pivot].a.[j]
                m.[pivot].a.[j] <- tmp

        if abs pivot_val < 1e-12 then
            pivot_val <- 1e-12

        let pivot_scale = 1.0 / pivot_val

        for i in k + 1..l-1 do
            let x = m.[i].a.[0] * pivot_scale
            m.[k].al.[i - k - 1] <- x

            for j in 1..10 do
                m.[i].a.[j - 1] <- m.[i].a.[j] - x * m.[k].a.[j]

            m.[i].a.[10] <- 0.0


let banbks11 (m : BandMatrix[]) (perm : int[]) (v : float[]) n =
    // forward substitution
    let mutable l = 5
    let mutable i = 0
    let mutable j = 0

    for k in 0..n-1 do
        i <- perm.[k]
        if i <> k then
            let tmp = v.[k]
            v.[k] <- v.[i]
            v.[i] <- tmp

        if l < n then
            l <- l+1

        for i in (k + 1)..l-1 do
            v.[i] <- v.[i] - m.[k].al.[i - k - 1] * v.[k]

    // back substitution
    l <- 1

    for i in n-1..(-1)..0 do
        let mutable x = v.[i]
        for k in 1..l-1 do
            x <- x - m.[i].a.[k] * v.[k + i]
        assert (m.[i].a.[0] <> 0.0)
        v.[i] <- x / m.[i].a.[0]
        if l < 11 then
            l <- l+1


let compute_jinc ty0 ty1 =

    if ty0 = SpiroPointType.G4 || ty1 = SpiroPointType.G4 || ty0 = SpiroPointType.Right || ty1 = SpiroPointType.Left then
        4
    elif ty0 = SpiroPointType.G2 && ty1 = SpiroPointType.G2 then
        2
    elif (((ty0 = SpiroPointType.OpenContour || ty0 = SpiroPointType.Corner || ty0 = SpiroPointType.Left) && ty1 = SpiroPointType.G2)
        || (ty0 = SpiroPointType.G2 && (ty1 = SpiroPointType.EndOpenContour || ty1 = SpiroPointType.Corner || ty1 = SpiroPointType.Right))) then
        1
    else
        0


let count_vec (s : SpiroSegment[]) nseg =
    Seq.sum [for i in 0..nseg-1 do compute_jinc (s.[i].Type) (s.[i + 1].Type)]


let add_mat_line (m: BandMatrix[], v : float[], derivs : float[], x, y, j, jj, jinc, nmat) =
    let mutable joff = 0
    let mutable k = 0
    if (jj >= 0) then
        if nmat < 6 then
            joff <- j + 5 - jj
        elif nmat = 6 then
            joff <- 2 + (j + 3 - jj + nmat) % nmat
        else        
            joff <- (j + 5 - jj + nmat) % nmat

        v.[jj] <- v.[jj] + x
        for k in 0..jinc-1 do
            m.[jj].a.[joff + k] <- m.[jj].a.[joff + k] + y * derivs.[k]


let spiro_iter (s : SpiroSegment[]) (m: BandMatrix[]) (perm : int[]) (v : float[]) n nmat =
    //let ends = Array2D.create 2 4 0.0
    //let derivs = Array3D.create 4 2 4 0.0
    let ends = MyArray2D(2, 4)
    let derivs = MyArray3D(4, 2, 4)
    let cyclic = s.[0].Type <> SpiroPointType.OpenContour && s.[0].Type <> SpiroPointType.Corner

    for i in 0..nmat-1 do
        v.[i] <- 0.0
        for j in 0..10 do
            m.[i].a.[j] <- 0.0
        for j in 0..4 do
            m.[i].al.[j] <- 0.0

    let mutable jj =
        match s.[0].Type with
        | SpiroPointType.G4 -> nmat - 2
        | SpiroPointType.G2 -> nmat - 1
        | _ -> 0
    
    let mutable j = 0
    let mutable n_invert = 0 

    for i in 0..n-1 do
        let ty0 = s.[i].Type
        let ty1 = s.[i + 1].Type
        let jinc = compute_jinc ty0 ty1
        let th = s.[i].bend_th
        let mutable jthl = -1
        let mutable jthr = -1
        let mutable jk0l = -1
        let mutable jk0r = -1
        let mutable jk1l = -1
        let mutable jk1r = -1
        let mutable jk2l = -1
        let mutable jk2r = -1

        compute_pderivs s.[i] ends derivs jinc

        // constraints crossing left
        if (ty0 = SpiroPointType.G4 || ty0 = SpiroPointType.G2 || ty0 = SpiroPointType.Left || ty0 = SpiroPointType.Right) then
            jthl <- jj
            jj <- jj + 1
            jj <- jj % nmat
            jk0l <- jj
            jj <- jj + 1

            if (ty0 = SpiroPointType.G4) then
                jj <- jj % nmat
                jk1l <- jj
                jj <- jj + 1
                jk2l <- jj
                jj <- jj + 1

        // constraints on left
        if ((ty0 = SpiroPointType.Left || ty0 = SpiroPointType.Corner || ty0 = SpiroPointType.OpenContour || ty0 = SpiroPointType.G2) && jinc = 4) then
            if (ty0 <> SpiroPointType.G2) then
                jk1l <- jj
                jj <- jj + 1

            jk2l <- jj
            jj <- jj + 1

        // constraints on right
        if ((ty1 = SpiroPointType.Right || ty1 = SpiroPointType.Corner || ty1 = SpiroPointType.EndOpenContour || ty1 = SpiroPointType.G2) && jinc = 4) then
            if (ty1 <> SpiroPointType.G2) then
                jk1r <- jj
                jj <- jj + 1

            jk2r <- jj
            jj <- jj + 1

        // constraints crossing right
        if (ty1 = SpiroPointType.G4 || ty1 = SpiroPointType.G2 || ty1 = SpiroPointType.Left || ty1 = SpiroPointType.Right) then
            jthr <- jj
            jk0r <- (jj + 1) % nmat

            if (ty1 = SpiroPointType.G4) then
                jk1r <- (jj + 2) % nmat
                jk2r <- (jj + 3) % nmat

        let getDerivs i j = [|for k in 0..3 do derivs.[i,j,k]|]
        add_mat_line(m, v, getDerivs 0 0, th - ends.[0,0], 1.0, j, jthl, jinc, nmat)
        add_mat_line(m, v, getDerivs 1 0, ends.[0,1], -1.0, j, jk0l, jinc, nmat)
        add_mat_line(m, v, getDerivs 2 0, ends.[0,2], -1.0, j, jk1l, jinc, nmat)
        add_mat_line(m, v, getDerivs 3 0, ends.[0,3], -1.0, j, jk2l, jinc, nmat)
        add_mat_line(m, v, getDerivs 0 1, -ends.[1,0], 1.0, j, jthr, jinc, nmat)
        add_mat_line(m, v, getDerivs 1 1, -ends.[1,1], 1.0, j, jk0r, jinc, nmat)
        add_mat_line(m, v, getDerivs 2 1, -ends.[1,2], 1.0, j, jk1r, jinc, nmat)
        add_mat_line(m, v, getDerivs 3 1, -ends.[1,3], 1.0, j, jk2r, jinc, nmat)

        if (jthl >= 0) then
            v.[jthl] <- mod_2pi(v.[jthl])

        if (jthr >= 0) then
            v.[jthr] <- mod_2pi(v.[jthr])

        j <- j + jinc

    if (cyclic) then
        BandMatrix.Copy m 0 m nmat nmat
        BandMatrix.Copy m 0 m (2 * nmat) nmat
        Array.Copy(v, 0, v, nmat, nmat)
        Array.Copy(v, 0, v, 2 * nmat, nmat)
        n_invert <- 3 * nmat
        j <- nmat
    else
        n_invert <- nmat
        j <- 0

    bandec11 m perm n_invert
    banbks11 m perm v n_invert
    
    let mutable norm = 0.0

    for i in 0..n-1 do
        let jinc = compute_jinc s.[i].Type s.[i + 1].Type

        for k in 0..jinc-1 do
            let dk = v.[j]
            j <- j + 1
            s.[i].ks.[k] <- s.[i].ks.[k] + dk
            norm <- norm + dk * dk

        s.[i].ks.[0] <- 2.0 * mod_2pi (s.[i].ks.[0] / 2.0)

    norm


let check_finiteness (segs : SpiroSegment[]) num_segs =
    Seq.sum [for i in 0..num_segs-1 do
                for j in 0..3 do
                    if not (IsFinite(segs.[i].ks.[j])) then 1 else 0] = 0


let solve_spiro (s: SpiroSegment[]) nseg =
    let nmat = count_vec s nseg
    let mutable n_alloc = nmat
    if nmat = 0 then
        true // no convergence problems
    else
        if (s.[0].Type <> SpiroPointType.OpenContour && s.[0].Type <> SpiroPointType.Corner) then
            n_alloc <- n_alloc * 3
        if n_alloc < 5 then
            n_alloc <- 5

        let m = Array.init n_alloc (fun _ -> BandMatrix())
        let v = Array.create n_alloc 0.0
        let perm = Array.create n_alloc 0

        let threshold = 1e-12
        let mutable norm = 1.0
        let mutable i = 0
        while i < 60 && norm > threshold && check_finiteness s nseg do
            i <- i + 1
            norm <- spiro_iter s m perm v nseg nmat
        norm <= threshold


let get_scale_rot dx dy ks =
    let seg_ch = hypot dx dy
    let seg_th = atan2 dy dx
    let x, y = integrate_spiro ks
    let ch = hypot x y
    let th = atan2 y x
    let scale = seg_ch / ch
    let rot = seg_th - th
    (scale, rot)


let get_mid x0 y0 x1 y1 scale rot (ks : float[]) (ksub : float[]) =
    ksub.[0] <- 0.5 * ks.[0] - 0.125 * ks.[1] + (1.0 / 64.0) * ks.[2] - (1.0 / 768.0) * ks.[3]
    ksub.[1] <- 0.25 * ks.[1] - (1.0 / 16.0) * ks.[2] + (1.0 / 128.0) * ks.[3]
    ksub.[2] <- 0.125 * ks.[2] - (1.0 / 32.0) * ks.[3]
    ksub.[3] <- (1.0 / 16.0) * ks.[3]
    let thsub = rot - 0.25 * ks.[0] + (1.0 / 32.0) * ks.[1] - (1.0 / 384.0) * ks.[2] + (1.0 / 6144.0) * ks.[3]
    let cth = 0.5 * scale * cos thsub
    let sth = 0.5 * scale * sin thsub
    let xsub, ysub = integrate_spiro ksub
    let xmid = x0 + cth * xsub - sth * ysub
    let ymid = y0 + cth * ysub + sth * xsub
    (xmid, ymid)


let rec spiro_seg_to_bpath (ks : float[]) x0 y0 x1 y1 (bc : IBezierContext) depth =
    let bend = abs ks.[0] + abs (0.5 * ks.[1]) + abs (0.125 * ks.[2]) + abs ((1.0 / 48.0) * ks.[3])
    if bend <= 1e-8 then
        bc.LineTo(x1, y1)
    else
        let scale, rot = get_scale_rot (x1 - x0) (y1 - y0) ks

        if depth > 5 || bend < 1.0 then
            let th_even = (1.0 / 384.0) * ks.[3] + (1.0 / 8.0) * ks.[1] + rot
            let th_odd = (1.0 / 48.0) * ks.[2] + 0.5 * ks.[0]
            let ul = (scale * (1.0 / 3.0)) * cos (th_even - th_odd)
            let vl = (scale * (1.0 / 3.0)) * sin (th_even - th_odd)
            let ur = (scale * (1.0 / 3.0)) * cos (th_even + th_odd)
            let vr = (scale * (1.0 / 3.0)) * sin (th_even + th_odd)
            bc.CurveTo(x0 + ul, y0 + vl, x1 - ur, y1 - vr, x1, y1)
        else
            // subdivide
            let ksub = Array.create 4 0.0
            let xmid, ymid = get_mid x0 y0 x1 y1 scale rot ks ksub
            spiro_seg_to_bpath ksub x0 y0 xmid ymid bc (depth + 1)
            ksub.[0] <- ksub.[0] + 0.25 * ks.[1] + (1.0 / 384.0) * ks.[3]
            ksub.[1] <- ksub.[1] + 0.125 * ks.[2]
            ksub.[2] <- ksub.[2] + (1.0 / 16.0) * ks.[3]
            spiro_seg_to_bpath ksub xmid ymid x1 y1 bc (depth + 1)


let run_spiro (src : SpiroControlPoint[]) =
    let n = src.Length
    let s = setup_path src n
    let nseg = if src.[0].Type = SpiroPointType.OpenContour then n - 1 else n
    if nseg <= 1 then
        Some s
    elif solve_spiro s nseg then
        Some s
    else
        None        


let get_knot_th (s : SpiroSegment[]) i =
    if i = 0 then
        let ends = compute_ends s.[i].ks s.[i].seg_ch
        s.[i].seg_th - ends.[0,0]
    else
        let ends = compute_ends s.[i - 1].ks s.[i - 1].seg_ch
        s.[i - 1].seg_th + ends.[1,0]


let spiro_to_bpath (s: SpiroSegment[]) n (bc : IBezierContext) =
    let nsegs = if s.[n - 1].Type = SpiroPointType.EndOpenContour then n - 1 else n
    for i in 0..nsegs-1 do
        let x0 = s.[i].X
        let x1 = s.[i + 1].X
        let y0 = s.[i].Y
        let y1 = s.[i + 1].Y
        if i = 0 then
            bc.MoveTo(x0, y0, s.[0].Type = SpiroPointType.OpenContour)

        bc.MarkKnot(i, get_knot_th s i, s.[i].X, s.[i].Y, s.[i].Type)
        spiro_seg_to_bpath s.[i].ks x0 y0 x1 y1 bc 0

    if nsegs = n - 1 then
        bc.MarkKnot(n - 1, get_knot_th s (n - 1), s.[n - 1].X, s.[n - 1].Y, s.[n - 1].Type)
