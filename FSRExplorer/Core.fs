module Core

/// A list of booleans denoting a State
type State = bool list

/// A list of States that form a cycle
type Cycle = State list

/// A Feedback Function maps a State to a single bit
type FeedbackFunction = (State -> bool)

/// A collection of Cycles and the Feedback Function used to generate them
type CycleStructure = {cycles: Cycle list; feedback: FeedbackFunction}

/// A Finite State Register (FSR) maps a State to a State
type FSR = (State -> bool * State)

/// A cycle rep function maps a Cycle to a State
type CycleRep = (Cycle -> State)

/// State comparator compares two states and returns an int
type StateComparator = (State -> State -> int)

/// Comparator Mapper
type ComparatorMapper = (State -> int)