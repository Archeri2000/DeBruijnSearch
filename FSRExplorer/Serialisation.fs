module Serialisation

open Core
open State
open InputParser

let serialiseCycle (c:Cycle) =
    let rec serialiser (ls:State list) (curr:string) =
        match ls with
        |[] -> curr
        |x::rem -> (curr + " " + (SPrintState x)) |> serialiser rem
    (serialiser c "").[1..]
let serialiseCycles (cs:Cycle list) =
    let rec serialiser (ls:Cycle list) (curr:string) =
        match ls with
        |[] -> curr
        |x::rem -> (curr + "\n" + (serialiseCycle x)) |> serialiser rem
    let length = cs.Head.Head.Length
    (serialiser cs (length.ToString()))
    
let deserialiseCycle (length: int) (str: string) =
    let tokens = str.Split " "
    tokens |> Seq.map (ParseBinary length) |> Seq.toList
    
let deserialiseCycles (str: string) =
    let tokens = str.Split "\n"
    match tokens |> Seq.toList with
    |[] -> failwith "Invalid Input!"
    |first::rem -> rem |> Seq.map (deserialiseCycle (int first)) |> Seq.toList