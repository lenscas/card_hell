namespace BulletHell

open Godot
open GDUtils

type RestartFs() as this =
    inherit Node2D()

    let currentScoreDisplay = this.getNode<Label> ("CurrentScore")
    let highScoreContainer = this.getNode ("HighScoreContainer")

    member this.newScore(score: int) =
        currentScoreDisplay.Value.Text <- "Score:" + score.ToString()
        this.Show()

    member this.OnRestartButtonPressed() =
        this.EmitSignal("RestartGameSameConfig")
