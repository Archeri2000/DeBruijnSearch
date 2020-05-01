module GraphVisualisation.FormatGraph

open Core
open State

type Colour =
    |Red
    |Gold
    |Aquamarine
    |Blue
    |Yellow
    |White
    |Black
type Attribute =
    |FillColour of Colour
    |Bold
    |FontSize of int
type Graph = {graphType: string; label: string; edges: string list; nodes: string list}

type Node = {name: string; data: Attribute list}

let getColourString colour =
    match colour with
    |Red -> "red"
    |Gold -> "gold"
    |Aquamarine -> "aquamarine"
    |Blue -> "blue"
    |Yellow -> "yellow"
    |White -> "white"
    |Black -> "black"
    
let getColourAttribute colourString=
    match colourString with
    |"red" -> Some Red
    |"gold" -> Some Gold
    |"aquamarine" -> Some Aquamarine
    |"blue" -> Some Blue
    |"yellow" -> Some Yellow
    |"white" -> Some White
    |"black" -> Some Black
    |_ -> None
    
let MakeEdge (x:State) (y:State) = (sprintf "%s -> %s" (SPrintState x) (SPrintState y))

let MakeDirectedCycle (cyc: Cycle) =
    match cyc with
    |[] -> failwith "Empty List!"
    |first::_ ->
    let rec fmtCycle (cycle: Cycle) (ls: string list) =
        match cycle with
        | x::y::rem -> fmtCycle (y::rem) ((MakeEdge x y)::ls)
        | [x] -> (MakeEdge x first)::ls
        | [] -> failwith "Unreachable State!"
    fmtCycle cyc []
    
let MakeDiGraph (title: string) =
    {graphType = "digraph"; label = title; edges = []; nodes = []}

let AddEdges (edges: string list) (graph: Graph) =
    {graphType = graph.graphType; label = graph.label; edges = (List.append graph.edges edges); nodes = graph.nodes}
    
let AddNodes (nodes: string list) (graph: Graph) =
    {graphType = graph.graphType; label = graph.label; edges = graph.edges; nodes = (List.append graph.nodes nodes)}
    
let GraphCycles (title: string) (cycles: Cycle list) =
    let edges = cycles |> List.fold (fun x y -> x |> List.append (MakeDirectedCycle y)) []
    (MakeDiGraph title) |> AddEdges edges
    
let GetGraphString (graph:Graph) =
    let label = sprintf "label=\"%s\"; labelloc=b" graph.label
    let contents = (List.append graph.edges graph.nodes) |> List.fold (fun x y -> sprintf "%s; %s" x y) label
    sprintf "%s{%s}" graph.graphType contents

let MakeNode (state: State) =
    {name = (SPrintState state); data = []}
    
let ColourNodeWith (colour: Colour) (node: Node) =
    {node with data = (FillColour colour)::(node.data |> List.filter
                                                         (fun x -> match x with |FillColour _ -> false |_ -> true))}
    //sprintf "\"%s\" [style=filled, fillcolor=%s, fontname=\"times bold\"]" (SPrintState state) colour
    
let SetNodesColour (colour: Colour) (nodes: Node list) =
    nodes |> List.map (ColourNodeWith colour)
    
let BoldNode (node: Node) =
    {node with data = Bold::(node.data |> List.filter
                                                         (fun x -> match x with |Bold _ -> false |_ -> true))}
    
let SetNodesBold (nodes: Node list) =
    nodes |> List.map BoldNode
let NodeFontSize (size:int) (node: Node) =
    {node with data = (FontSize size)::(node.data |> List.filter
                                                         (fun x -> match x with |FontSize _ -> false |_ -> true))}
    
let SetNodesFontSize (size:int) (nodes:Node list) =
    nodes |> List.map (NodeFontSize size)
    
let AttributeToString (attr:Attribute) =
    match attr with
    |FillColour colour -> "style=filled, fillcolor=" + (getColourString colour)
    |Bold -> "fontname=\"times bold\""
    |FontSize size -> "fontsize=" + (string size)

let UnpackNode (node: Node) =
    let attrList = node.data |> List.map AttributeToString
    match attrList with
        |first::rem ->
            let attrStr = rem |> List.fold (fun x y -> sprintf "%s, %s" x y) first
            sprintf "%s[%s]" node.name attrStr
        |[] -> ""

let ContainsNode (node: Node) (list: Node list) =
    list |> List.exists (fun x -> x.name = node.name)
    
let GetNode (str: string) (nodes: Node list) =
    nodes |> List.tryFind (fun x -> x.name = str)
    
let CombineAttribute (attrList:Attribute List) (attr:Attribute) =
    match attr with
    |FillColour _ -> attr::(attrList |> List.filter (fun x -> match x with |FillColour _ -> false |_ -> true))
    |Bold -> attr::(attrList |> List.filter (fun x -> match x with |Bold _ -> false |_ -> true))
    |FontSize _ -> attr::(attrList |> List.filter (fun x -> match x with |FontSize _ -> false |_ -> true))

let CombineAttributes (attrList:Attribute List) = List.fold CombineAttribute attrList

let AddNodeToList (NodeList: Node list) (node: Node) =
    if NodeList |> ContainsNode node
    then
        let newNode = {node with data=
                                    let folder =
                                        match (NodeList |> GetNode node.name |> Option.map (fun x -> CombineAttributes x.data)) with
                                        |Some x -> x
                                        |None -> (fun x -> x)
                                    node.data |> folder
        }
        newNode::(NodeList |> List.filter (fun x -> x.name <> node.name))
    else node::NodeList
let ConcatNodes (list1: Node list) (list2: Node list) =
    list1 |> List.fold AddNodeToList list2
    
let FormatNodes (nodes: Node list) =
    nodes |> List.map UnpackNode