namespace BulletHell

open Godot
open GDUtils

type TimerElementFs() as this =
    inherit ProgressBar()

    let label = this.getNode<Label> ("TimerLabel")

    let updateColor () =
        match this.timer with
        | Some timerE ->
            let timeLeft = timer.getTimeLeft timerE
            let maxTime = timerE |> timer.toType |> timer.maxTime
            let nonReds = timeLeft / maxTime
            label.Value.AddColorOverride("font_color", Color(1F - nonReds, nonReds, 0F))
            label.Value.Text <- Mathf.Round(timeLeft).ToString()
            this.MaxValue <- float maxTime
            this.Value <- float timeLeft
        | None -> ()


    member val timer: Option<BulletHell.Timer> = None with get, set

    member this.AddTurn() =
        this.timer <- this.timer |> Option.map timer.addTurn
        updateColor ()

    member this.AddTime time =
        this.timer <-
            this.timer
            |> Option.map (timer.collectBattery time)

        updateColor ()

    member this.SetTimer timerType =
        this.timer <- timerType |> timer.create |> Some
        updateColor ()

    member this.Tick delta whenZero =
        this.timer <-
            this.timer
            |> Option.map (
                timer.tick delta
                >> (fun x ->
                    if timer.getTimeLeft x <= 0F then
                        whenZero ()
                        timer.reset x
                    else
                        x)
            )

        updateColor ()
