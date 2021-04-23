namespace BulletHell

open Godot

type GameState =
    | CastTime
    | AnimationTime of (int * int)

type Node2DFs() as this =
    inherit Node2D()

    let player =
        lazy (this.GetNode<PlayerFs>(new NodePath("Player")))

    let hand =
        lazy (this.GetNode<HandFs>(new NodePath("Hand")))

    let timerElement =
        lazy (this.GetNode<RichTextLabel>(new NodePath("Timer")))

    let bullets =
        lazy (this.GetNode<BulletsFs>(new NodePath("Bullets")))

    let enemies =
        lazy (this.GetNode<EnemiesFs>(new NodePath("Enemies")))

    let batteries =
        lazy (this.GetNode<BatteriesFs>(new NodePath("Batteries")))

    let random = new System.Random()

    let mutable state = AnimationTime(0, 0)
    let mutable timer = 10F

    let updateDone () =
        match state with
        | AnimationTime (toDo, hasDone) -> state <- AnimationTime(toDo, hasDone + 1)
        | _ -> ()

    let spawnbullets isPlayer location =
        let directions =
            [ new Vector2(0F, -1F)
              new Vector2(0F, 1F)
              new Vector2(1F, 0F)
              new Vector2(-1F, 0F)
              new Vector2(1F, 1F)
              new Vector2(1F, -1F)
              new Vector2(-1F, 1F)
              new Vector2(-1F, -1F) ]

        for direction in directions do
            bullets.Value.AddBullet direction location isPlayer

    override this._Process delta =
        match state with
        | AnimationTime (toDo, hasDone) ->
            if toDo = hasDone then
                let count =
                    bullets.Value.getAmountHitPlayer player.Value.Position

                hand.Value.AddCard Dead count

                let time =
                    batteries.Value.getExtraTimeAmount player.Value.Position

                timer <- (timer + time) |> min 10F


                state <- CastTime

                hand.Value.StartCastTime
                    (fun (casted: Cards) ->
                        match casted with
                        | Dead ->
                            this
                                .GetTree()
                                .ChangeScene("res://restart_screen.tscn")
                            |> ignore
                        | _ -> ()

                        player.Value.GoTo(casted |> cards.ToVec) (updateDone)

                        if cards.WillSpawnBullets casted then
                            spawnbullets true player.Value.Position

                        if random.Next() > System.Int32.MaxValue / 2 then
                            enemies.Value.AddEnemy(
                                new Vector2(float32 (random.Next(15) + 1), float32 (random.Next(6) + 1))
                            )

                        if random.Next() > System.Int32.MaxValue / 3 then
                            10
                            |> random.Next
                            |> float32
                            |> batteries.Value.addBattery (
                                new Vector2(float32 (random.Next(15) + 1), float32 (random.Next(6) + 1))
                            )


                        bullets.Value.startAnimation updateDone
                        enemies.Value.startAnimation updateDone (spawnbullets false)
                        state <- AnimationTime(3, 0))

        | CastTime ->
            timer <- timer - delta
            timerElement.Value.Text <- Mathf.Round(timer).ToString()

            if timer < 0F then
                this
                    .GetTree()
                    .ChangeScene("res://restart_screen.tscn")
                |> ignore
