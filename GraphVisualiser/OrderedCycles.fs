module GraphVisualiser.OrderedCycles

open GraphVisualiser.CycleSet
open GraphVisualiser.Cycle
open GraphVisualiser.State

type CycleRep = (Cycle -> State)
type StateComparator = (State -> State -> int)
type OrderedCycles = {cycles: CycleSet; cycleRepFunc: CycleRep; stateComparator: StateComparator; fsr: Option<FSRFunction>}

let SortCyclesFull (func:Option<FSRFunction>) (stateComp:StateComparator, repFunc:CycleRep) (cycleset:CycleSet) =
    {cycles = (cycleset |> List.sortWith (fun x y -> stateComp (repFunc x) (repFunc y))); cycleRepFunc = repFunc; stateComparator = stateComp; fsr = func}

let SortCyclesBy = SortCyclesFull None

let NecklaceOrder = CompareLexicographic, GetNecklace

let MakeOrderedCycles (stateComp:StateComparator, repFunc:CycleRep) (func:FSRFunction) (length:int) =
    let cycles = GenerateAllCycles length (FSR func)
    cycles |> SortCyclesFull (Some func) (stateComp, repFunc)
    
let GetCycleRepresentatives (OC: OrderedCycles) =
    OC.cycles |> List.map OC.cycleRepFunc

let GetStatesWithComplementInLargerCycle (OC: OrderedCycles) =
    let IsCompanionIn (cycles:CycleSet) (state:State) =
        cycles |> ContainsState (GetCompanion state)
    let rec GetStates (cycles:CycleSet) =
        match cycles with
        |first::rem -> (first |> List.filter (IsCompanionIn rem))::GetStates(rem)
        |[] -> []
    let states = GetStates OC.cycles
    states |> List.collect (fun x -> x)
    