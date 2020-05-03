module CycleStructure

open Core
open Cycle
open State
open Orders
open FiniteStateRegisters

///Maps a Cycle Structure to a List of Cycles in string representation
let SPrintCycleStructure (cs:CycleStructure) = cs.cycles |> List.map SPrintCycle

/// Prints a cycle set to console
let PrintCycleStructure (cs:CycleStructure) = cs.cycles |> List.iter (fun x ->
    PrintCycle x
    printf "\n")

/// Generates all cycles for a given FSR and length
let GenerateAllCycles length (feedback:FeedbackFunction) =
    let set = Set.ofList [0 .. (int)(2.0 ** (float)length - 1.0)]
    let rec genCycs (stateSet:Set<int>) (cycles: Cycle list) =
        let state = stateSet.MinimumElement
        let cycle = GenerateCycle (state |> StateFromInt length) (MakeFSR feedback)
        let remain = Set.difference stateSet (Set.ofList (cycle |> List.map StateToInt))
        if remain |> Set.isEmpty
        then cycle::cycles
        else genCycs remain (cycle::cycles)
    {cycles = genCycs set []; feedback = feedback}

/// Checks if the CycleStructure contains the State
let ContainsState (state: State) (cycles: Cycle list) =
    cycles |> List.exists (CycleContains state)
    
/// Sorts all cycles by comparing the cycle representatives with the comparator
let SortCyclesBy (compare:StateComparator, getRep:CycleRep) =
    List.sortWith (fun x y -> compare (getRep x) (getRep y))

/// Order by the necklaces in lexicographical order
let NecklaceOrder = CompareLexicographic, GetNecklace

/// Creates ordered cycles from a comparator, representative function and FSR with specified length
let GenerateOrderedCycles (stateComp:StateComparator, repFunc:CycleRep) (feedback:FeedbackFunction) (length:int) =
    let cs = GenerateAllCycles length feedback
    {cs with cycles = cs.cycles |> SortCyclesBy (stateComp, repFunc)}
    
/// Gets the cycle representatives according to a cycle representative function from a Cycle Structure
let GetCycleRepresentativesBy (repFunc:CycleRep) (cs: CycleStructure) =
    cs.cycles |> List.map repFunc

let GetStatesByPredicateOnLargerCycle (predicate:Cycle list->State->bool) (cs: CycleStructure) =
    let rec GetStates (cycles:Cycle list) =
        match cycles with
        |first::rem -> (first |> List.filter (predicate rem)) @ GetStates(rem)
        |[] -> []
    GetStates cs.cycles

/// Get a list of states which fulfil a criteria
let GetStatesByPredicate (predicate: State -> bool) =
    GetStatesByPredicateOnLargerCycle (fun _ -> predicate)
    
let GetStatesWithCompanionInLargerCycle =
    GetStatesByPredicateOnLargerCycle (fun cycles state -> ContainsState (GetCompanion state) cycles)