namespace BulletHell

open Godot
open GDUtils

type battery = { sprite: Sprite; storedTime: float32 }

type BatteriesFs() as this =
    inherit Node2D()

    let map =
        lazy (this.GetNode<TileMap>(new NodePath("../Ground")))

    let mutable batteries = []

    member __.count() = batteries.Length

    member this.addBattery location storedTime =
        let location =
            (map.Value.MapToWorld location)
            + (new Vector2(32F, 32F))

        let sprite = this.addSprite "battery" location

        batteries <-
            { sprite = sprite
              storedTime = storedTime }
            :: batteries

    member __.getExtraTimeAmount location onCollision =
        let map = map.Value
        let playerLoc = map.WorldToMap(location)
        let mutable time = 0F

        batteries <-
            batteries
            |> List.filter (
                (fun x ->
                    if map.WorldToMap(x.sprite.Position) = playerLoc then
                        time <- time + x.storedTime
                        x.sprite.QueueFree()
                        onCollision ()
                        false
                    else
                        true)
            )

        time
