namespace BulletHell

open Godot

type battery = { sprite: Sprite; storedTime: float32 }

type BatteriesFs() as this =
    inherit Node2D()

    let map =
        lazy (this.GetNode<TileMap>(new NodePath("../Ground")))

    let mutable batteries = []

    member this.addBattery location storedTime =
        let location =
            (map.Value.MapToWorld location)
            + (new Vector2(32F, 32F))

        //let path = "res://battery.png"
        //let tex = new ImageTexture()
        //let img = new Image()
        //ignore (img.Load path)
        //tex.CreateFromImage img

        let sprite = new Sprite()
        sprite.Texture <- GD.Load<Texture>("res://battery.png")
        sprite.Position <- location

        this.AddChild(sprite)

        batteries <-
            { sprite = sprite
              storedTime = storedTime }
            :: batteries

    member __.getExtraTimeAmount location =
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
                        false
                    else
                        true)
            )

        time
