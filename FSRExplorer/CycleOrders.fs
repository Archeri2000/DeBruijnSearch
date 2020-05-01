module Orders

open Core
open State

let HammingWeight (st: State) =
    st |> Seq.fold (fun x y -> x + (if y then 1 else 0)) 0
    
let LongestOneSequence (st: State) =
    let rec findLongestSeq (curr: int) (currmax: int) (state: State) =
        match state with
        |[] -> max curr currmax
        |x::rem -> if x then (rem |> findLongestSeq (curr+1) currmax) else (rem |> findLongestSeq 0 (max curr currmax))
    st |> findLongestSeq 0 0
    
let LongestZeroSequence (st: State) =
    let rec findLongestSeq (curr: int) (currmax: int) (state: State) = 
        match state with
        |[] -> max curr currmax
        |x::rem -> if not x then (rem |> findLongestSeq (curr+1) currmax) else (rem |> findLongestSeq 0 (max curr currmax))
    st |> findLongestSeq 0 0
    
let StartsWith1 (st:State) = st.Head
    
let rec EndsWith1 (st:State) =
    match st with
    |[] -> failwith "Empty State!"
    |[a] -> a
    |_::rem -> EndsWith1 rem

//Comparators
/// Compare states
let CompareStates (c:ComparatorMapper) (a: State) (b: State) =
    compare (c a) (c b)

/// Compares two states lexicographically returning 1 if a > b, 0 if they are equal, -1 if a < b 
let CompareLexicographic:StateComparator = CompareStates StateToInt

let CompareHammingWeight:StateComparator = CompareStates HammingWeight
let CompareMixed (st1:State) (st2:State) =
    match CompareHammingWeight st1 st2 with
    |0 -> -(CompareLexicographic st1 st2)
    |wt -> wt

let getMixedOrderCandidates (cycle:Cycle) =
    let cyclerep = cycle |> List.sortWith CompareMixed |> Seq.head
    let minWeight = cyclerep |> HammingWeight
    let options = cycle |> List.filter (fun x -> StartsWith1 x && (HammingWeight x) = minWeight)
    (cyclerep, options)

let THasOptions (noOptions:State->'a) (hasOptions:State list -> 'a) (rep: State,opts: State list) =
    if opts.Length > 0
    then hasOptions opts
    else noOptions rep
        
let IfHasOptions = THasOptions (fun x -> x)

let RepFunc1 (klist: int list) =
    let getIndex (ls:State list) = (klist |> List.reduce (fun x y -> if y <= ls.Length then y else x))
    getMixedOrderCandidates >> IfHasOptions (fun x -> x |> Seq.item (getIndex x))
    
let RepFunc2 (k:int) =
    getMixedOrderCandidates >> IfHasOptions (fun options ->
        match (options.Length % k) with
            |0 -> options |> Seq.item (k - 1)
            |x -> options |> Seq.item (x - 1)
    )
    
let RepFunc3 (k:int) =
    getMixedOrderCandidates >> IfHasOptions (fun x -> x |> Seq.item (k % x.Length))
    
let MixedWeightCycleRep = RepFunc3 1
    
let getCandidateCount =
    getMixedOrderCandidates >> THasOptions (fun _ -> 1) (fun opts -> opts.Length)