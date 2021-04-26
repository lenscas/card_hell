namespace BulletHell

type Cards =
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | SpawnBullets
    | Dead

open Godot

module cards =
    let ToVec card =
        match card with
        | MoveUp -> new Vector2(0F, 1F)
        | MoveDown -> new Vector2(0F, -1F)
        | MoveLeft -> new Vector2(1F, 0F)
        | MoveRight -> new Vector2(-1F, 0F)
        | SpawnBullets
        | Dead -> new Vector2(0F, 0F)


    let WillSpawnBullets card =
        match card with
        | SpawnBullets -> true
        | _ -> false

    let ToImage card =
        let img =
            match card with
            | MoveUp -> "up"
            | MoveDown -> "down"
            | MoveLeft -> "left"
            | MoveRight -> "right"
            | SpawnBullets -> "bullets"
            | Dead -> "dead"

        GDUtils.loadCard img
