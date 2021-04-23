namespace BulletHell

open Godot

type RestartFs() =
    inherit Button()

    [<Export>]
    member val Text = "Hello World!" with get, set

    override this._Ready() = GD.Print(this.Text)

    member this.OnRestartButtonPressed() =
        this.GetTree().ChangeScene("res://MainScene.tscn")
        |> ignore
