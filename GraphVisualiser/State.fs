module GraphVisualiser.State

type State = bool List
type FSRFunction = (State -> bool)

let FSR func (st: State) =
    match st with
    |_::rem ->
        let rec result ls =
            match ls with
            |[ed] -> ed::[(func st)]
            |a::b -> a::result(b)
            |_ -> [func st]
        //Return result
        result rem
    |_ -> failwith "Empty List"

let MakeState (st:int List) =
    st |> List.map (fun x -> x <> 0)
    
let SPrintState (state: State) =
    state |> List.fold (fun x y -> sprintf "%s%d" x (if y then 1 else 0)) ""
let PrintState (state: State) =
    printf "%s\n" (SPrintState state)
let StateToInt (st: State) =
    let rec toInt (state:State) curr =
        match state with
        |x::rem -> toInt rem (curr * 2 + (if x then 1 else 0)) 
        |[] -> curr
    toInt st 0
    
let StateFromInt (num: int) (length: int) =
    let rec fromInt length curr (state:State) =
        match length with
        |0 -> state
        |i -> fromInt (length-1) (curr / 2) ((curr % 2 = 1)::state)
    fromInt length num []
    
let CompareLexicographic (a: State) (b: State) =
    compare (StateToInt a) (StateToInt b)
        
let GetCompanion (a: State) =
    let rec MakeCompanion (a: State) =
        match a with
        |x::y::rem -> x::(MakeCompanion (y::rem))
        |x::[] -> [not x]
        |[] -> failwith "Empty State!"
    MakeCompanion a
    
let GetConjugate (a: State) =
    match a with
    |x::rem -> (not x)::rem
    |[] -> failwith "Empty State!"
        
    