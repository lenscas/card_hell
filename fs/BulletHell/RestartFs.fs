namespace BulletHell

open Godot

type RestartFs() =
    inherit Node2D()

    member this.OnRestartButtonPressed() =
        this.EmitSignal("RestartGameSameConfig")
