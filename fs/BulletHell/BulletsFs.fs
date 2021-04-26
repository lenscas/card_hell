namespace BulletHell

open Godot

type state =
    | Animation of float32 * (unit -> unit)
    | Paused

type Bullet =
    { sprite: Sprite
      movement: Vector2
      movementFunc: (Sprite -> float32 -> unit) }

type BulletsFs() as this =
    inherit Node()

    let mutable state = Paused

    let mutable bulletsPlayer = []
    let mutable bulletsGame = []

    let map =
        lazy (this.GetNode<TileMap>(new NodePath("../Ground")))


    member __.startAnimation func =
        state <- Animation(0F, func)

        bulletsPlayer <-
            bulletsPlayer
            |> List.map (
                (fun bullet ->
                    let movementFunc =
                        test.getNextMovementFunc map.Value bullet.sprite.Position bullet.movement

                    let mutable passedTime = 0F

                    { bullet with
                          movementFunc =
                              (fun sprite delta ->
                                  passedTime <- passedTime + delta
                                  let percentage = min 1F passedTime
                                  sprite.Position <- movementFunc percentage

                                  ) })
            )

        bulletsGame <-
            bulletsGame
            |> List.map (
                (fun bullet ->
                    let movementFunc =
                        test.getNextMovementFunc map.Value bullet.sprite.Position bullet.movement

                    let mutable passedTime = 0F

                    { bullet with
                          movementFunc =
                              (fun sprite delta ->
                                  passedTime <- passedTime + delta
                                  let percentage = min 1F passedTime
                                  sprite.Position <- movementFunc percentage

                                  ) })
            )

    member __.getAmountHitPlayer playerLoc =
        let map = map.Value
        let playerLoc = map.WorldToMap playerLoc

        bulletsGame
        |> Seq.filter ((fun x -> map.WorldToMap(x.sprite.Position) = playerLoc))
        |> Seq.length

    member __.getAmountHitEnemy enemyLoc =
        let map = map.Value
        let enemyLoc = map.WorldToMap(enemyLoc)

        bulletsPlayer
        |> Seq.filter ((fun x -> map.WorldToMap(x.sprite.Position) = enemyLoc))
        |> Seq.length

    member this.AddBullet movement location isPlayer =
        let path = "res://bullet.png"

        let sprite = new Sprite()
        sprite.Texture <- GD.Load<Texture>(path)
        sprite.Position <- location

        this.AddChild(sprite)

        if isPlayer then
            bulletsPlayer <-
                { sprite = sprite
                  movement = movement
                  movementFunc = (fun _ _ -> ()) }
                :: bulletsPlayer
        else
            bulletsGame <-
                { sprite = sprite
                  movement = movement
                  movementFunc = (fun _ _ -> ()) }
                :: bulletsGame

    override __._Process delta =
        match state with
        | Animation (time, func) ->
            let time = time + delta
            let map = map.Value

            let checker =
                (fun v ->
                    v.movementFunc v.sprite delta

                    if test.isInGrid map v.sprite.Position then
                        true
                    else
                        v.sprite.QueueFree()
                        false)

            bulletsPlayer <- bulletsPlayer |> List.filter checker

            bulletsGame <- bulletsGame |> List.filter checker

            if time >= 1F then
                state <- Paused
                func ()
            else
                state <- Animation(time, func)
        | Paused -> ()
