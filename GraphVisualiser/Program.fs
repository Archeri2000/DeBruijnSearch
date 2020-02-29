// Learn more about F# at http://fsharp.org
open GraphVisualiser
open GraphVisualiser.State
open GraphVisualiser.DrawGraph
open GraphVisualiser.CycleSet
open GraphVisualiser.FormatGraph

[<EntryPoint>]
let main argv =
    let cycset = CSR |> GenerateAllCycles 6
    //PrintCycleSet cycset
    MakeGraph (GetGraphString (GraphCycles "test" cycset)) (Some "test")
    0