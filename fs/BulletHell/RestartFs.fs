namespace BulletHell

open Godot

type RestartFs() =
    inherit Button()

    member this.OnRestartButtonPressed() =
        this.GetTree().ChangeScene("res://MainScene.tscn")
        |> ignore
