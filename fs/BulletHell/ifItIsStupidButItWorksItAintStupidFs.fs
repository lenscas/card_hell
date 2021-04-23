namespace BulletHell

open Godot

type ifItIsStupidButItWorksItAintStupidFs() =
    inherit Node2D()

    [<Export>]
    member val Text = "Hello World!" with get, set

    override this._Ready() =
        GD.Print(this.Text)
