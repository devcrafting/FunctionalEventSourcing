module CrazyEights

type Rank =
    | Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King
type Suit = Club | Spade | Diamond | Heart

type Card = {
    Rank: Rank
    Suit: Suit
}

let (^) rank suit = { Rank = rank; Suit = suit }

[<Struct>]
type Players = private Players of int

module Players =
    let from n =
        if n <= 2 then
            Error "Too few players"
        else
            Ok (Players n)

type Command =
    | StartGame of StartGame
and StartGame = {
    Players: Players
}

type Event =
    | GameStarted of GameStarted
and GameStarted = {
    Players: Players
}

type State =
    | InitialState
    | Started

let initialState = InitialState

let decide (command: Command) (state:State) : Event list =
    match command with
    | StartGame c ->
        match state with
        | InitialState ->
            [ GameStarted { Players = c.Players } ]
        | Started ->
            []

let evolve (state: State) (event:Event) : State =
    match state, event with
    | InitialState, GameStarted _ -> Started
    | _ -> state

[<EntryPoint>]
let main argv =
    printfn "Hello world"
    0 // return an integer exit code
