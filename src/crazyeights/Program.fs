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
    | Play of Play
and StartGame = {
    Players: Players
    FirstCard: Card
}
and Play = {
    Card: Card
}

type Event =
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed
and GameStarted = {
    Players: Players
    FirstCard: Card
}
and CardPlayed = {
    Card: Card
}

type State =
    | NotStarted
    | Started

let initialState = NotStarted

let decide (command: Command) (state:State) : Event list =
    match state, command with
    | NotStarted, StartGame c ->
        [ GameStarted { Players = c.Players; FirstCard = c.FirstCard } ]
    | Started, Play c ->
        [ CardPlayed { Card = c.Card }]
    | _ ->
        []

let evolve (state: State) (event:Event) : State =
    match state, event with
    | NotStarted, GameStarted _ -> Started
    | _ -> state

[<EntryPoint>]
let main argv =
    printfn "Hello world"
    0 // return an integer exit code
