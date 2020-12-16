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

type Player = Player of int

type Command =
    | StartGame of StartGame
    | Play of Play
and StartGame = {
    Players: Players
    FirstCard: Card
}
and Play = {
    Card: Card
    Player: Player
}

type Event =
    | GameStarted of GameStarted
    | CardPlayed of CardPlayed
    | WrongCardPlayed of CardPlayed
    | WrongPlayerPlayed of CardPlayed
and GameStarted = {
    Players: Players
    FirstCard: Card
}
and CardPlayed = {
    Card: Card
    Player: Player
}

type State =
    | NotStarted
    | Started of Started
and Started = {
    TopCard: Card
    NextPlayer: Player
}

let initialState = NotStarted

let decide (command: Command) (state:State) : Event list =
    match state, command with
    | NotStarted, StartGame c ->
        [ GameStarted { Players = c.Players; FirstCard = c.FirstCard } ]
    | Started s, Play c when c.Player <> s.NextPlayer ->
        [ WrongPlayerPlayed { Card = c.Card; Player = c.Player } ]
    | Started s, Play c when c.Card.Suit <> s.TopCard.Suit && c.Card.Rank <> s.TopCard.Rank ->
        [ WrongCardPlayed { Card = c.Card; Player = c.Player } ]
    | Started _, Play c ->
        [ CardPlayed { Card = c.Card; Player = c.Player }]
    | _ ->
        []

let evolve (state: State) (event:Event) : State =
    match state, event with
    | NotStarted, GameStarted e ->
        Started { TopCard = e.FirstCard; NextPlayer = Player 1 }
    | Started s, CardPlayed { Card = c; Player = Player p } ->
        Started { s with TopCard = c; NextPlayer = Player (p+1) }
    | _ -> state

[<EntryPoint>]
let main argv =
    printfn "Hello world"
    0 // return an integer exit code
