// Learn more about F# at http://fsharp.org
open GraphVisualiser.Debug
open GraphVisualiser.DrawGraph
open GraphVisualiser.OrderedCycles
open GraphVisualiser.StateRegisterFunctions
open GraphVisualiser.FormatGraph

[<EntryPoint>]
let main argv =
    let polynomials8 = ["x^7 + x^6 + x^5 + x^4 + x + 1";
                        "x^7 + x^4 + x^3 + x^2 + x + 1";
                        "x^5 + x^4 + x^3 + 1";
                        "x^7 + x^6 + x^4 + x^3 + x^2 + 1";
                        "x^7 + x^5 + x^4 + 1";
                        "x^7 + x^6 + x^5 + x^4 + x^3 + 1";
                        "x^7 + x^3 + x + 1";
                        "x^4 + x^3 + x + 1";
                        "x^6 + x^5 + x^4 + x^2 + x + 1";
                        "x^5 + x^4 + x^3 + x^2 + x + 1";
                        "x^7 + x^5 + x + 1";
                        "x^7 + x^5 + x^4 + x^3 + x^2 + 1";
                        "x^7 + x^6 + x^4 + x^2 + x + 1";
                        "x^6 + x^5 + x^4 + x^3 + x + 1"]

    let polynomials7 = [" x^6 + x^5 + x^3 + x^2 + 1";
                       "x^4 + x^2 + 1";
                       "x^6 + x^4 + x^3 + x + 1";
                       "x^5 + x^4 + x^2 + x + 1";
                       "x^5 + x + 1";
                       "x^4 + x^3 + x^2 + x + 1";
                       "x^6 + x^2 + 1";
                       "x^5 + x^3 + 1";
                       "x^6 + x^5 + x^4 + x^3 + 1"]
    
    let polynomials6 = ["x^5 + x^3 + x^2 + x + 1";
                        "x^4 + x^3 + 1";
                        "x^5 + x^4 + 1";
                        "x^5 + x^4 + x^3 + x + 1";
                        "x^3 + x^2 + 1";
                        "x^2 + x + 1"]
    let printCycles (length: int) (count: int, polynomial: string) = 
        let CSRCycles = MakeOrderedCycles NecklaceOrder (polynomial |> MakePolyFSR length) length
        let necklaces = CSRCycles |> GetCycleRepresentatives |> List.map MakeNode |> SetNodesBold
        let next = CSRCycles |> GetStatesWithComplementInLargerCycle  |> List.map MakeNode |> SetNodesColour aquamarine
        let nodes = necklaces |> ConcatNodes next
        MakeGraph (GetGraphString ((GraphCycles polynomial (List.rev CSRCycles.cycles)) |> AddNodes (FormatNodes nodes))) (Some (sprintf "L%dP%d" length count))
    polynomials8 |> List.zip [1..polynomials8.Length] |> List.iter (printCycles 8)
    polynomials7 |> List.zip [1..polynomials7.Length] |> List.iter (printCycles 7)
    polynomials6 |> List.zip [1..polynomials6.Length] |> List.iter (printCycles 6)
    0