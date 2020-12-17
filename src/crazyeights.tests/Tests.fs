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
    => Play { Card = Four ^ Spade; Player = Player 1 }
    =! [ CardPlayed { Card = Four ^ Spade; Player = Player 1 }]

[<Fact>]
let ``Game should allow playing card with same rank`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade } ]
    => Play { Card = Three ^ Heart; Player = Player 1 }
    =! [ CardPlayed { Card = Three ^ Heart; Player = Player 1 }]

[<Fact>]
let ``Game should NOT allow playing card with different suit and rank`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade } ]
    => Play { Card = Four ^ Heart; Player = Player 1 }
    =! [ WrongCardPlayed { Card = Four ^ Heart; Player = Player 1 }]

[<Fact>]
let ``Game should take in account last card played`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade }
      CardPlayed { Card = Four ^ Spade; Player = Player 1 } ]
    => Play { Card = Four ^ Diamond; Player = Player 2 }
    =! [ CardPlayed { Card = Four ^ Diamond; Player = Player 2 }]

[<Fact>]
let ``Game should NOT allow a player to play not when not their turn`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade } ]
    => Play { Card = Three ^ Diamond; Player = Player 2 }
    =! [ WrongPlayerPlayed { Card = Three ^ Diamond; Player = Player 2 }]

[<Fact>]
let ``Game should cycle players' turn`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade }
      CardPlayed { Card = Three ^ Diamond; Player = Player 1 }
      CardPlayed { Card = Four ^ Diamond; Player = Player 2 }
      CardPlayed { Card = Five ^ Diamond; Player = Player 3 } ]
    => Play { Card = Six ^ Diamond; Player = Player 0 }
    =! [ CardPlayed { Card = Six ^ Diamond; Player = Player 0 }]

[<Fact>]
let ``Game should skip next player when current player plays a 7`` () =
    [ GameStarted { Players = players 4; FirstCard = Three ^ Spade }
      CardPlayed { Card = Seven ^ Heart; Player = Player 1 } ]
    => Play { Card = Three ^ Heart; Player = Player 3 }
    =! [ CardPlayed { Card = Three ^ Heart; Player = Player 3 }]
