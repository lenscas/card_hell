namespace BulletHell

open Godot
open GDUtils
open FSharp.Json

type RestartFs() as this =
    inherit Node2D()

    let currentScoreDisplay = this.getNode<Label> ("CurrentScore")
    let highScoreContainer = this.getNode ("HighScoreContainer")

    member this.newScore(score: int) =
        currentScoreDisplay.Value.Text <- "Score:" + score.ToString()

        for v in highScoreContainer.Value.getChildren () do
            highScoreContainer.Value.RemoveChild(v)
            v.QueueFree()

        let mutable inserted = false

        let mutable highscores =
            match readFileAs<int list> "highscores.json" with
            | Ok x -> x
            | Result.Error _ -> []
            |> Seq.collect
                (fun x ->
                    if (not inserted) && x < score then
                        inserted <- true
                        [ score; x ]
                    else
                        [ x ])
            |> Seq.truncate 10
            |> Seq.toList

        if highscores.Length < 10 && not inserted then
            highscores <- (score :: (highscores |> List.rev)) |> List.rev
            inserted <- true


        highscores
        |> Seq.map string
        |> Seq.mapi (fun x y -> x, y)
        |> Seq.iter
            (fun (index, highscore) ->
                let label = new Label()
                GD.Print(index)
                label.SetPosition(Vector2(384F, 117F + float32 (index * 35)))
                label.SetSize(Vector2(256F, 14F))
                label.Text <- highscore.ToString()
                this.AddChild(label))

        if inserted then
            writeObjectToFile "highscores.json" highscores
            |> ignore

        this.Show()

    member this.OnRestartButtonPressed() =
        this.EmitSignal("RestartGameSameConfig")
