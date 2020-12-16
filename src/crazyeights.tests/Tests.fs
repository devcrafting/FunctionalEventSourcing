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
    => StartGame { Players = players 4 }
    =! [ GameStarted { Players = players 4 }]

[<Fact>]
let ``It's not fun to play alone`` () =
    Players.from 1 =! Error "Too few players"

[<Fact>]
let ``Game should do nothing when start a started game`` () =
    [ GameStarted { Players = players 4 } ]
    => StartGame { Players = players 4 }
    =! []