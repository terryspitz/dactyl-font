module SpiroTest

open NUnit.Framework

open SpiroPointType
open SpiroControlPoint


[<TestFixture>]
type TestClass() = 

    [<Test>]
    member this.CheckCurves() =
        let scps = [|
            {X=80.;Y=738.;Type=FromChar '{'}
            {X=749.;Y=540.;Type=FromChar 'o'}
            {X=671.;Y=309.;Type=FromChar 'o'}
            {X=521.;Y=396.;Type=FromChar 'o'}
            {X=377.;Y=333.;Type=FromChar 'o'}
            {X=467.;Y=231.;Type=FromChar '}'}
        |]
        match Spiro.SpiroCPsToSegments scps false with
        | Some segs -> Assert.True(scps.Length = segs.Length)
        | _ -> invalidOp "SpiroCPsToSegments failed"

        // rs_check_vals verify_rs1[] = {
        //        /* iteration6 */
        // let points = [|
        // {0.000000, 697.685459, -0.287750}
        // {-1.608688, 243.813453, -1.896438}
        // {-1.770739, 173.404152, 2.616009}
        // {0.937994, 157.178243, -2.729182}
        // {1.881365, 136.029409, -0.847817
        // |]

    // rs_check_vals verify_rs2[] = {        /* iteration26 */
    //     {0.000000, 108.115679, -0.046263},    /* {,233,143 */
    //     {-0.926114, 79.881162, -0.972377},    /* o,341,138 */
    //     {1.844174, 90.138782, 0.871797}    /* o,386, 72 */
    // };                    /* },444,141 */


    [<Test>]
    member this.CheckAnchor() =
        let scps = [|
            //spiro_cp path6[] = { /* verify curve data with ah. */
            {X=  0.;Y=  0.;Type=FromChar '{'}
            {X=100.;Y=100.;Type=FromChar 'c'}
            {X=200.;Y=200.;Type=FromChar 'a'}
            {X=300.;Y=200.;Type=FromChar 'h'}
            {X=300.;Y=150.;Type=FromChar 'c'}
            {X=200.;Y=100.;Type=FromChar 'a'}
            {X=100.;Y=100.;Type=FromChar 'h'}
            {X=150.;Y= 50.;Type=FromChar 'c'}
            {X=100.;Y=  0.;Type=FromChar 'a'}
            {X=  0.;Y=100.;Type=FromChar 'h'}
            {X= 50.;Y=100.;Type=FromChar 'c'}
            {X= 20.;Y=150.;Type=FromChar '}'}
        |]
        let ret =
            match Spiro.SpiroCPsToSegments scps false with
            | Some segs -> segs
            | _ -> invalidOp "SpiroCPsToSegments failed"
        Assert.True(scps.Length = ret.Length)

        //check round trip
        let scps2 = [|for seg in ret do {X=seg.X; Y=seg.Y; Type=seg.Type}|]
        Assert.AreEqual(scps, scps2)
        let ret2 = 
            match Spiro.SpiroCPsToSegments scps false with
            | Some segs -> segs
            | _ -> invalidOp "SpiroCPsToSegments failed"
        Assert.AreEqual(ret, ret2)
        ()

