namespace BulletHell

open Godot

type PlayModeMenuFs() =
    inherit Node2D()

    [<Export>]
    member val Text = "Hello World!" with get, set

    override this._Ready() = GD.Print(this.Text)

    member this.OpenJam() =
        this.EmitSignal("ChosenPlayMode", TimerType.Jam)

    member this.OpenRelaxed() =
        this.EmitSignal("ChosenPlayMode", TimerType.Relaxed)

    member this.OpenResetting() =
        this.EmitSignal("ChosenPlayMode", TimerType.EverShrinking)
