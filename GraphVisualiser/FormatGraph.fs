module GraphVisualiser.FormatGraph

open GraphVisualiser.CycleSet
open GraphVisualiser.State
open GraphVisualiser.Cycle

type Graph = {graphType: string; label: string; edges: string List; nodes: string List}
type Node = {name: string; data: Map<string, string>}
let red = "red"
let gold = "gold"
let aquamarine = "aquamarine"
let blue = "blue"
let yellow = "yellow"
let white = "white"
let black = "black"
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

let MakeNode (state: State) =
    {name = (SPrintState state); data = Map.empty}
let ColourNodeWith (colour: string) (node: Node) =
    {name = node.name; data = node.data.Add("style", "filled").Add("fillcolor", colour)}
    //sprintf "\"%s\" [style=filled, fillcolor=%s, fontname=\"times bold\"]" (SPrintState state) colour
    
let SetNodesColour (colour: string) (nodes: Node List) =
    nodes |> List.map (ColourNodeWith colour)
    
let BoldNode (node: Node) =
    {name = node.name; data = node.data.Add("fontname", "\"times bold\"")}
    
let SetNodesBold (nodes: Node List) =
    nodes |> List.map BoldNode

let UnpackNode (node: Node) =
    let attrList = node.data |> Map.toList |> List.map (fun (k,v) -> sprintf "%s=%s" k v)
    match attrList with
        |first::rem ->
            let attrStr = rem |> List.fold (fun x y -> sprintf "%s, %s" x y) first
            sprintf "%s[%s]" node.name attrStr
        |[] -> ""

let ContainsNode (node: Node) (list: Node List) =
    list |> List.exists (fun x -> x.name = node.name)
    
let GetNode (str: string) (nodes: Node List) =
    nodes |> List.find (fun x -> x.name = str)
    
let ConcatNodes (list1: Node List) (list2: Node List) =
    let rec updateNodesWith (update: Node List) (nodes: Node List)  =
        match nodes with
        | first::remaining ->
            if (update |> ContainsNode first)
            then
                let nd = update |> GetNode first.name
                {name = first.name; data = (nd.data |> Map.toList |> List.fold (fun x y -> x.Add(y)) first.data)}::(remaining |> updateNodesWith update)
            else first::(remaining |> updateNodesWith update)
        | [] -> []
    let diff = list1 |> List.filter (fun x -> not (list2 |> ContainsNode x))
    list2 |> updateNodesWith list1 |> List.append diff
    
let FormatNodes (nodes: Node List) =
    nodes |> List.map UnpackNode