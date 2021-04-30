namespace BulletHell

open Godot
open GDUtils

type SceneManagerFs() as this =
    inherit Node2D()

    let restartScene = this.getNode<RestartFs> ("Restart")
    let modeSelectScene = this.getNode<Node2D> ("PlayModeMenu")

    let mutable lastScene : Option<Node2DFs> = None

    let mutable timerType : Option<TimerType> = None

    member this.OnChoosePlayMode newTimerType =
        GD.Print "got here"
        timerType <- Some newTimerType
        modeSelectScene.Value.Hide()
        this.OnRestartGameSameConfig()


    member __.OnGameEnd(score: int) =
        match lastScene with
        | Some x ->
            x.QueueFree()
            lastScene <- None
        | None ->
            GD.PrintErr "Game ended without a last game set! HOW!?"
            ()

        restartScene.Value.newScore score

    member this.OnRestartGameSameConfig() =
        match timerType with
        | None -> ()
        | Some timerType ->
            restartScene.Value.Hide()

            let gameScene =
                GD
                    .Load<PackedScene>("res://scenes/Game.tscn")
                    .Instance()
                :?> Node2DFs

            gameScene.addTimer timerType

            this.AddChild(gameScene)

            gameScene.Connect("End", this, "OnGameEnd")
            |> ignore

            lastScene <- Some gameScene
