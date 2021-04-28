namespace BulletHell

open Godot
open GDUtils

type GameState =
    | CastTime
    | AnimationTime of (int * int)

type Node2DFs() as this =
    inherit Node2D()

    let player = this.getNode<PlayerFs> "Player"

    let hand = this.getNode<HandFs> "Hand"

    let timerElement = this.getNode<TimerElementFs> "Timer"

    let bullets = this.getNode<BulletsFs> "Bullets"

    let enemies = this.getNode<EnemiesFs> "Enemies"

    let batteries = this.getNode<BatteriesFs> "Batteries"

    let random = new System.Random()

    let mutable state = AnimationTime(0, 0)

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

    let tick delta timerE =
        let timerElement = timerElement.Value

        match state with
        | AnimationTime (toDo, hasDone) ->
            if toDo = hasDone then
                let count =
                    bullets.Value.getAmountHitPlayer player.Value.Position

                if count > 0 then
                    hand.Value.AddCard Dead count

                batteries.Value.getExtraTimeAmount player.Value.Position
                |> timerElement.AddTime

                state <- CastTime
                timerElement.Show()

                hand.Value.StartCastTime
                    (fun (casted: Cards) ->
                        timerElement.Hide()

                        match casted with
                        | Dead -> this.EmitSignal("End", 0)
                        | _ -> ()

                        timerElement.AddTurn()

                        player.Value.GoTo(casted |> cards.ToVec) updateDone

                        if cards.WillSpawnBullets casted then
                            spawnbullets true player.Value.Position

                        if random.Next() > System.Int32.MaxValue / 2 then
                            test.getRandomInBetween ()
                            |> enemies.Value.AddEnemy

                        if batteries.Value.count () < 3
                           || (random.Next() > System.Int32.MaxValue / 3) then
                            timerE
                            |> timer.maxBatterySize
                            |> (fun x -> x - 1)
                            |> random.Next
                            |> (fun x -> x + 1)
                            |> float32
                            |> batteries.Value.addBattery (test.getRandomInBetween ())

                        bullets.Value.startAnimation updateDone
                        enemies.Value.startAnimation updateDone (spawnbullets false)
                        state <- AnimationTime(3, 0))

        | CastTime -> timerElement.Tick delta (fun () -> hand.Value.AddCard Dead 1)

    member __.addTimer = timerElement.Value.SetTimer

    override __._Process delta =
        match timerElement.Value.timer with
        | Some timer -> tick delta timer
        | None -> ()
