
module Arrays

///Define my own 2D array since Fable doesn't offer one
type MyArray2D(r, c) =
    //member or even mutable member doesn't seem to allow 
    //setting from Item method, use class local variable
    let _arr = Array.create (r*c) 0.0
    member this.r = r
    member this.c = c
    member this.arr = _arr

    ///Can't override (.[,]) so take a tuple instead
    //member this.(.[]) (i : int * int) = 
    member this.Item
      //with get (i : int * int) : float = arr.[(fst i)*c + (snd i)]
      with get (i : int * int) : float = _arr.[(fst i)*c + (snd i)]
      and set (i : int * int) (value : float) = _arr.[(fst i)*c + (snd i)] <- value

    member this.CopyFrom (src : MyArray2D) =
      assert (this.r = src.r)
      assert (this.c = src.c)
      System.Array.Copy(src.arr, this.arr, this.r*this.c);


type MyArray3D(d1, d2, d3) =
    let d1 = d1
    let d2 = d2
    let d3 = d3
    let arr = Array.create (d1*d2*d3) 0.0
    ///Can't override (.[,]) so take a tuple instead
    member this.Item
      with get (index : int * int * int) : float = 
        let i, j, k = index
        arr.[i*d2*d3 + j*d3 + k]
      and set (index : int * int * int) (value : float) =
        let i, j, k = index
        arr.[i*d2*d3 + j*d3 + k] <- value

let test = 
    let testarr = MyArray2D(2,2)
    testarr.[(2,0)] <- 1.0
    let test2 = testarr.[2,0]
    testarr.[1,0]
