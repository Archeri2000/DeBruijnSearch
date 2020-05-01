module State

open Core

/// Maps a State to a binary string
let SPrintState (state: State) =
    state |> List.fold (fun x y -> sprintf "%s%d" x (if y then 1 else 0)) ""
    
/// Prints a State to Console
let PrintState (state: State) =
    printf "%s\n" (SPrintState state)

/// Converts a State to its integer representation (Base 2 with rightmost number being 2^0)
let StateToInt (st: State) =
    let rec toInt (state:State) curr =
        match state with
        |x::rem -> toInt rem (curr * 2 + (if x then 1 else 0)) 
        |[] -> curr
    toInt st 0
    
/// Converts an integer into a State (Base 2 with leftmost number being 2^0)
let StateFromInt (length: int) (num: int) =
    let rec fromInt length curr (state:State) =
        match length with
        |0 -> state
        |i -> fromInt (length-1) (curr / 2) ((curr % 2 = 1)::state)
    fromInt length num []
        
/// Gets the companion of the current state (All bits are equal except the rightmost bit is inverted)
let GetCompanion (a: State) =
    let rec MakeCompanion (a: State) =
        match a with
        |x::y::rem -> x::(MakeCompanion (y::rem))
        |x::[] -> [not x]
        |[] -> failwith "Empty State!"
    MakeCompanion a
    
/// Gets the companion of the current state (All bits are equal except the leftmost bit is inverted)
let GetConjugate (a: State) =
    match a with
    |x::rem -> (not x)::rem
    |[] -> failwith "Empty State!"