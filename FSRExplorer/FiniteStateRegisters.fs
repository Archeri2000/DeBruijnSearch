module FiniteStateRegisters

open Core
let ShiftAndAppend (bit:bool) (state:State) =
    let rec appendBit state = 
        match state with
        |x::rem -> x::(rem |> appendBit)
        |[] -> [bit]
    (state |> appendBit) |> (fun x -> x.Head, x.Tail)

/// Turn a Feedback Function into a FSR
let MakeFSR (feedback: FeedbackFunction) (state:State) =
    state |> ShiftAndAppend (feedback state)
    
/// Predicated FSR
let MakePredicatedFSR predicate feedback state=
    if state |> predicate
    then MakeFSR (not<<feedback) state
    else MakeFSR feedback state

    
/// A polynomial for use with FeedbackFunctions
type Polynomial = bool list

///ANDs the polynomial with the state and XORs the results
let ANDWithThenSUM (initial:bool) (poly:Polynomial) = List.map2 (&&) poly >> List.fold (fun x y -> x <> y) initial

/// Create a polynomial FSR function that takes in a bool list denoting the polynomial
let PolyFeedbackFunction (poly: bool list) = poly |> ANDWithThenSUM false
    
/// Create a complemented polynomial FSR function that takes in a bool list denoting the polynomial
let PolyComplementedFeedbackFunction (poly: bool list) = poly |> ANDWithThenSUM true
    
/// The Complemented Summing Register function
let CSRFeedbackFunc = List.fold (fun x y -> x <> y) true

/// The CSR as an FSR
let CSR = MakeFSR CSRFeedbackFunc