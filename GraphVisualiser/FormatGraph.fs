module GraphVisualiser.FormatGraph

open GraphVisualiser.CycleSet
open GraphVisualiser.State
open GraphVisualiser.Cycle

type Graph = {graphType: string; label: string; edges: string List; nodes: string List}

let MakeEdge (x:State) (y:State) = (sprintf "%s -> %s" (SPrintState x) (SPrintState y))

let FmtDirectedCycle (cyc: Cycle) =
    match cyc with
    |[] -> failwith "Empty List!"
    |first::rem ->
    let rec fmtCycle (cycle: Cycle) (ls: string List) =
        match cycle with
        | x::y::rem -> fmtCycle (y::rem) ((MakeEdge x y)::ls)
        | [x] -> (MakeEdge x first)::ls
        | _ -> failwith "Empty List!"
    fmtCycle cyc []
    
let MakeDiGraph (title: string) =
    {graphType = "digraph"; label = title; edges = []; nodes = []}

let AddEdges (edges: string List) (graph: Graph) =
    {graphType = graph.graphType; label = graph.label; edges = (List.append graph.edges edges); nodes = graph.nodes}
    
let AddNodes (nodes: string List) (graph: Graph) =
    {graphType = graph.graphType; label = graph.label; edges = graph.edges; nodes = (List.append graph.nodes nodes)}
    
let GraphCycles (title: string) (cycles: CycleSet) =
    let edges = cycles |> List.fold (fun x y -> x |> List.append (FmtDirectedCycle y)) []
    (MakeDiGraph title) |> AddEdges edges
    
let GetGraphString (graph:Graph) =
    let label = sprintf "label=\"%s\"; labelloc=b" graph.label
    let contents = (List.append graph.edges graph.nodes) |> List.fold (fun x y -> sprintf "%s; %s" x y) label
    sprintf "%s{%s}" graph.graphType contents
    