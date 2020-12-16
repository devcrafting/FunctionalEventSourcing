module Tests

open System
open Xunit
open Swensen.Unquote
open CrazyEights

let (=>) (events: Event list) (cmd: Command) : Event list =
    events
    |> List.fold evolve initialState
    |> decide cmd

let players n =
    match Players.from n with Ok n -> n

[<Fact>]
let ``Game should start`` () =
    []
    => StartGame { Players = players 4; FirstCard = Three ^ Spade }
    =! [ GameStarted { Players = players 4; FirstCard = Three ^ Spade }]

[<Fact>]
let ``It's not fun to play alone`` () =
    Players.from 1 =! Error "Too few players"

[<Fact>]
let ``Game should do nothing when start a started game`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade } ]
    => StartGame { Players = players 4; FirstCard = Three ^ Spade }
    =! []

[<Fact>]
let ``Game should allow playing card with same suit`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade } ]
    => Play { Card = Four ^ Spade }
    =! [ CardPlayed { Card = Four ^ Spade }]

[<Fact>]
let ``Game should NOT allow playing card with different suit and rank`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade } ]
    => Play { Card = Four ^ Heart }
    =! [ WrongCardPlayed { Card = Four ^ Heart }]
