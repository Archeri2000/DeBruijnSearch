module Cycle

open Core
open State
open Orders

/// Maps a Cycle to a List of Strings
let SPrintCycle cycle =
    cycle |> List.map (fun x -> sprintf "%s\n" (SPrintState x))
    
/// Prints a Cycle to Console
let PrintCycle cycle =
    (SPrintCycle cycle) |> List.iter (fun x -> printf "%s" x)
    
/// Checks if a State is in the Cycle
let CycleContains (state:State) = List.contains state
    
/// Generates the cycle originating from a State using a FSR function
let GenerateCycle (st: State) (next:FSR) =
    let rec genCyc (state: State) (cycle: Cycle) =
        let _,newState = next state
        if cycle |> CycleContains newState
        then cycle
        else genCyc newState (newState::cycle)
    List.rev (genCyc st [st])
    
/// Gets the lexicographically least state in the cycle
let GetNecklace (cycle: Cycle) =
    cycle |> List.sortWith CompareLexicographic |> Seq.head