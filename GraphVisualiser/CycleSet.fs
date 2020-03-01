module GraphVisualiser.CycleSet

open GraphVisualiser.State
open GraphVisualiser.Cycle

type CycleSet = Cycle List

let PrintCycleSet cycleset =
    cycleset |> List.iter (
        fun x ->
        x |> PrintCycle
        printf "\n")

let GenerateAllCycles length (func:(State -> State)) =
    let set = Set.ofList [0 .. (int)(2.0 ** (float)length - 1.0)]
    let rec genCycs (stateSet:Set<int>) (cycles: CycleSet) =
        let state = stateSet.MinimumElement
        let cycle = GenerateCycle (StateFromInt state length) func
        let remain = Set.difference stateSet (Set.ofList (cycle |> List.map StateToInt))
        if remain |> Set.isEmpty
        then cycle::cycles
        else genCycs remain (cycle::cycles)
    genCycs set []

let rec ContainsState (state: State) (cycleset: CycleSet) =
    match cycleset with
    |x::rem ->
        if (x |> List.contains state)
        then true
        else rem |> ContainsState state
    |[] -> false
