module FSRExplorer.Misc

open System.Text
open Core
open Cycle
open State
open CycleStructure
open InputParser
open GraphVisualisation.FormatGraph
open GraphVisualisation.DrawGraph
open FiniteStateRegisters

let drawCycles (comp:StateComparator,rep:CycleRep) (length: int) (count: int, polynomial: string) =
        let CSRCycles = GenerateOrderedCycles (comp, rep) (polynomial |> MakePolyFeedbackFunction length) length
        let necklaces = CSRCycles |> GetCycleRepresentativesBy rep |> List.map MakeNode |> SetNodesBold
        let next = CSRCycles |> GetStatesWithCompanionInLargerCycle  |> List.map MakeNode |> SetNodesColour Aquamarine
        let nodes = necklaces |> ConcatNodes next
        MakeGraph (GetGraphString ((GraphCycles polynomial (List.rev CSRCycles.cycles)) |> AddNodes (FormatNodes nodes))) (Some (sprintf "L%dP%d" length count))

let drawCyclesByNecklaceOrder = drawCycles NecklaceOrder

let drawAllCycles drawer (length: int) (polyList:string list)=
        polyList |> List.zip [1..polyList.Length] |> List.iter (drawer length)
        
let drawAllCyclesByNecklaceOrder = drawAllCycles drawCyclesByNecklaceOrder

let makeDeBruijn (length: int) (threshold: int) (func:FeedbackFunction) (necklaces: State list)=
    let predicate (necklaces:State list) (st:State) =
        necklaces |> Seq.contains st || necklaces |> Seq.contains ((not st.Head) :: st.Tail)
    let succfunc = MakePredicatedFSR (predicate necklaces) func        
    let zeroes = "000000" |> ParseBinary length
    let rec generator (ref:State) (counter:int) (curr: State) (str:StringBuilder) =
        if counter > threshold
        then None
        else if curr = ref
        then
            if counter = pown 2 length
            then Some (str.ToString())
            else None
        else
            let (bit,next) = succfunc curr
            generator ref (counter+1) next (str.Append(if bit then "1" else "0"))
    let dbgen = generator zeroes 1
    let bit,next = (succfunc zeroes)
    let result = dbgen next (StringBuilder().Append(if bit then "1" else "0"))
    match result with
    |Some x -> x
    |None -> failwith "Did not form De Bruijn"
    