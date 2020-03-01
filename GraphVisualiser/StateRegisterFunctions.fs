module GraphVisualiser.StateRegisterFunctions
open System
open GraphVisualiser.State
open Microsoft.VisualBasic


let PolyFSR (poly: bool List) =
    fun (st:State) -> st |> List.zip poly |> List.fold (fun x (a,b) -> (a && b) <> x) false
let Length7Poly1 =
    let Poly = [true; true; false; true; true; true; true]
    Poly |> PolyFSR
//x^6 + x^5 + x^3 + x^2 + x + 1
let Length7Poly2 (st: State) =
    let Poly = [true; false; true; true; false; false; true]
    Poly |> PolyFSR
// x^6 + x^4 + x^3 + 1

let ParsePoly (length:int) (poly: string) =
    let stripSpaces (str: string) = str |> String.filter (fun x -> x <> ' ')
    let splitTokens (tokens: string) = tokens.Split '+'
    let tokens = poly |> stripSpaces |> splitTokens
    let getPosition (token: string) =
        match token with
        |x when x.Contains("x^") -> Int32.Parse (x.Substring(x.Length-1))
        |"x" -> 1
        | _ -> 0
    let positions = tokens |> Array.toList |> List.map getPosition
    [0 .. length-1] |> List.map (fun x -> positions |> List.contains x)

let MakePolyFSR (length:int) (poly: string) =
    poly |> ParsePoly length |> PolyFSR
let CSRFunc (st:State) = st |> List.fold (fun x y -> x <> y) true
let CSR state =
    FSR CSRFunc state