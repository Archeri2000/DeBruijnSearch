module InputParser

open System
open Core
open FiniteStateRegisters

/// Parse a polynomial string into a bool list representation
let ParsePoly (length:int) (poly: string) =
    let stripSpaces (str: string) = str |> String.filter (fun x -> x <> ' ')
    let splitTokens (tokens: string) = tokens.Split '+'
    let tokens = poly |> stripSpaces |> splitTokens
    let getPosition (token: string) =
        match token with
        |x when x.Contains("x^") -> Int32.Parse (x.Substring(x.Length-1))
        |"x" -> 1
        | _ -> 0
    let positions = tokens |> Array.map getPosition
    [0 .. length-1] |> List.map (fun x -> positions |> Array.contains x)
    
/// Make a polynomial feedback shift register from a string
let MakePolyFeedbackFunction (length:int) (poly: string) =
    poly |> ParsePoly length |> PolyFeedbackFunction
    
/// Make a complemented polynomial feedback shift register from a string
let MakePolyComplementedFeedbackFunction (length:int) (poly: string) =
    poly |> ParsePoly length |> PolyComplementedFeedbackFunction
    
/// Parse a binary number as a boolean list representation
let ParseBinary (length:int) (binary: string) =
    let rec padDiff (numPad: int) (arr: bool list) =
        match arr with
        |x::rem -> x::(padDiff numPad rem)
        |[] -> if numPad = 0 then [] else (false::(padDiff (numPad - 1) []))
    let arr = binary |> Seq.toList |> List.map (fun x -> x = '1')
    let diff = length - arr.Length
    if diff < 0
    then failwith "Length too short!"
    else arr |> padDiff diff
    
/// Make a polynomial feedback shift register from a binary number
let MakeBinaryFeedbackFunction (length:int) (binary: string) =
    binary |> ParseBinary length |> PolyFeedbackFunction
    
/// Make a complemented polynomial feedback shift register from a binary number
let MakeBinaryComplementedFeedbackFunction (length:int) (binary: string) =
    binary |> ParseBinary length |> PolyComplementedFeedbackFunction