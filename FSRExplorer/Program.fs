// Learn more about F# at http://fsharp.org

open FSRExplorer
open InputParser
open CycleStructure
open State
open Cycle
open Orders
open Misc

//let main2 argv =
//    let polynomials8 = ["x^7 + x^6 + x^5 + x^4 + x + 1";
//                        "x^7 + x^4 + x^3 + x^2 + x + 1";
//                        "x^5 + x^4 + x^3 + 1";
//                        "x^7 + x^6 + x^4 + x^3 + x^2 + 1";
//                        "x^7 + x^5 + x^4 + 1";
//                        "x^7 + x^6 + x^5 + x^4 + x^3 + 1";
//                        "x^7 + x^3 + x + 1";
//                        "x^4 + x^3 + x + 1";
//                        "x^6 + x^5 + x^4 + x^2 + x + 1";
//                        "x^5 + x^4 + x^3 + x^2 + x + 1";
//                        "x^7 + x^5 + x + 1";
//                        "x^7 + x^5 + x^4 + x^3 + x^2 + 1";
//                        "x^7 + x^6 + x^4 + x^2 + x + 1";
//                        "x^6 + x^5 + x^4 + x^3 + x + 1"]
//
//    let polynomials7 = [" x^6 + x^5 + x^3 + x^2 + 1";
//                       "x^4 + x^2 + 1";
//                       "x^6 + x^4 + x^3 + x + 1";
//                       "x^5 + x^4 + x^2 + x + 1";
//                       "x^5 + x + 1";
//                       "x^4 + x^3 + x^2 + x + 1";
//                       "x^6 + x^2 + 1";
//                       "x^5 + x^3 + 1";
//                       "x^6 + x^5 + x^4 + x^3 + 1"]
//
//    let polynomials6 = ["x^5 + x^3 + x^2 + x + 1";
//                        "x^4 + x^3 + 1";
//                        "x^5 + x^4 + 1";
//                        "x^5 + x^4 + x^3 + x + 1";
//                        "x^3 + x^2 + 1";
//                        "x^2 + x + 1"]
//    polynomials8 |> List.zip [1..polynomials8.Length] |> List.iter (printCycles 8)
//    polynomials7 |> List.zip [1..polynomials7.Length] |> List.iter (printCycles 7)
//    polynomials6 |> List.zip [1..polynomials6.Length] |> List.iter (printCycles 6)
//    0

    
let constructK (length: int) (i: int list) =
    let rec constructor (ls: int list) =
        match ls with
        |x::rem -> x::(constructor rem)
        |[] -> [length-1]
    constructor (1::i)

let GenerateKs (length: int) =
    [for i in 1..length do
        for j in 1..length do
            for k in 1..length do
                for l in 1..length do
                    for m in 1..length do
                        yield [i;j;k;l;m]]
let construct6 = constructK 6

[<EntryPoint>]
let main argv =
    let length = 6
    let func = ("111111" |> MakeBinaryFeedbackFunction length)
    let CSRCycles = GenerateOrderedCycles ((fun x y -> CompareHammingWeight y x), MixedWeightCycleRep) func length
//    let Rule1 = [construct6 []; construct6 [2]; construct6 [3];construct6 [4];construct6 [2;3];construct6 [2;4];
//     construct6 [3;4]; construct6 [2;3;4]] |> Seq.map (fun x ->
//        let db = CSRCycles.cycles |> Seq.map (RepFunc1 x) |> Seq.toList |> makeDeBruijn length 5000 func
//        x,db)
//    let Rule2 = [1..6] |> Seq.map (fun x ->
//        let db = CSRCycles.cycles |> Seq.map (RepFunc2 x) |> Seq.toList |> makeDeBruijn length 5000 func
//        x,db)
//    let Rule3 = [1..30] |> Seq.map (fun x ->
//        let db = CSRCycles.cycles |> Seq.map (RepFunc3 x) |> Seq.toList |> makeDeBruijn length 5000 func
//        x,db)
    let Rule4 = (GenerateKs 5)|> Seq.map (fun ls ->
        let mutable l = ls
        let reps = CSRCycles.cycles |> Seq.map (fun x ->
            if x |> getCandidateCount > 1
            then
                let h = l.Head
                l <- l.Tail
                x |> RepFunc3 h
            else
                x |> RepFunc3 1)
        let db = reps |> Seq.toList |>makeDeBruijn length 5000 func
        ls,db)
    
    let lstostring ls = ls |> List.fold (fun x y -> x + ", " + (string y)) ""
    Rule4 |> Seq.distinctBy (fun (_,y) -> y) |> Seq.iter (fun (x,y) -> printf "\{%s\}&\{%s\}\\\\\n" (lstostring x) y)
    
//    let candidates = CSRCycles.cycles |> Seq.map getCandidateCount
//    let candidates = deserialiseCycleSet (File.ReadAllText(@"D:\Documents\NTU\Year2\Sem2\CY2001Research\F#Implementation\GraphVisualiser\cycles\"+"10101110001"+".txt"))
//                     |> Seq.map getCandidateCount
//    candidates |> Seq.filter (fun x -> x <> 1) |> Seq.iter (fun x -> Debug.Print (string x))
//    let sequences =
//        candidates
//        |> Seq.filter (fun x -> x <> 1)
//        |> Seq.map uint32
//        |> Seq.reduce (*)
//    Debug.Print(string sequences)
    
//    let len = 6
//    let getCandidates (cycle:Cycle) = cycle |> List.filter (fun x -> x.Head)
//    let CSRCycles =
//        MakeOrderedCycles ((fun x y -> CompareLexicographic y x), GetNecklace)
//            ("110001" |> MakeBinaryFSR len) len
//    let candidates = CSRCycles.cycles |> List.map (fun x ->
//        match x.Length with
//        |k when k = 5 ->  getCandidates x
//        |_ -> [GetNecklace x])
//    candidates |> List.iter (fun x ->
//        printf "\n"
//        x |> List.iter PrintState)
    0
