namespace BulletHell

open Godot

type ScoreDisplayFs() =
    inherit Label()

    let mutable score = 0

    member this.addScore toAdd () =
        score <- score + toAdd
        this.Text <- score.ToString()

    member __.getScore() = score
