namespace BulletHell

open Godot
open GDUtils

type EnemyState =
    | Animation of float32 * (unit -> unit)
    | Paused

type Enemy =
    { sprite: Sprite
      movementFunc: (Sprite -> float32 -> unit)
      deck: Library }

type EnemiesFs() as this =
    inherit Node2D()

    let mutable state = Paused

    let mutable enemies = []

    let map = this.getNode<TileMap> "../Ground"

    member __.checkEnemies func =
        enemies <-
            enemies
            |> List.map
                (fun x ->
                    let count = func x.sprite.Position
                    x.deck.addCard Dead count
                    x)

    member __.startAnimation func spawnBullets =
        state <- Animation(0F, func)

        enemies <-
            enemies
            |> Seq.map (
                (fun enemy ->
                    let card = enemy.deck.getRandomFromHand ()
                    let movement = card |> cards.ToVec

                    let movement =
                        test.keepContained enemy.sprite.Position movement map.Value

                    enemy.deck.invalidateHand ()

                    let movementFunc =
                        test.getNextMovementFunc map.Value enemy.sprite.Position movement

                    let mutable passedTime = 0F
                    let mutable hasSpawnedBulelts = false

                    (card,
                     { enemy with
                           movementFunc =
                               (fun sprite delta ->
                                   passedTime <- passedTime + delta
                                   let percentage = min 1F passedTime
                                   sprite.Position <- movementFunc percentage

                                   if (not hasSpawnedBulelts)
                                      && cards.WillSpawnBullets card then
                                       hasSpawnedBulelts <- true
                                       spawnBullets sprite.Position) }))

            )
            |> Seq.filter
                (fun (card, enemy) ->
                    match card with
                    | Dead ->
                        enemy.sprite.QueueFree()
                        false
                    | _ -> true)
            |> Seq.map ((fun (_, enemy) -> enemy))
            |> Seq.toList


    member this.AddEnemy location =
        let location =
            (map.Value.MapToWorld location)
            + (new Vector2(32F, 32F))

        let sprite = this.addSprite "enemy" location

        enemies <-
            { sprite = sprite
              movementFunc = (fun _ _ -> ())
              deck =
                  new Library(
                      [ MoveUp
                        MoveDown
                        MoveLeft
                        MoveRight
                        MoveLeft
                        MoveRight
                        SpawnBullets ]

                  ) }
            :: enemies

    override __._Process delta =
        match state with
        | Animation (time, func) ->
            let time = time + delta

            for v in enemies do
                v.movementFunc v.sprite delta


            if time >= 1F then
                state <- Paused
                func ()
            else
                state <- Animation(time, func)
        | Paused -> ()
