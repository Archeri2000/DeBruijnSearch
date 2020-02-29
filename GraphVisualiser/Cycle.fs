module GraphVisualiser.Cycle

open GraphVisualiser.State

type Cycle = State List

let SPrintCycle cycle =
    cycle |> List.map (fun x -> sprintf "%s\n" (SPrintState x))
    
let PrintCycle cycle =
    (SPrintCycle cycle) |> List.iter (fun x -> printf "%s" x)
    
let IsInCycle (cycle: Cycle) (st: State) =
    List.contains st cycle
    
let GenerateCycle (st: State) func =
    let rec genCyc (state: State) (cycle: Cycle) =
        let next = func state
        if IsInCycle cycle next
        then cycle
        else genCyc next (next::cycle)
    List.rev (genCyc st [st])