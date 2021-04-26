namespace BulletHell

open Godot
open GDUtils

module test =
    let gridSizeX = 16
    let gridSizeY = 7
    let gridStart = 1

    let random = (new System.Random())

    let getRandomInBetween () =
        new Vector2(
            float32 (random.Next(gridSizeX - gridStart) + gridStart),
            float32 (random.Next(gridSizeY - gridStart) + gridStart)
        )

    let isInGrid (map: TileMap) position =
        let position = map.WorldToMap position

        position.x >= float32 gridStart
        && position.x <= float32 gridSizeX
        && position.y >= float32 gridStart
        && position.y <= float32 gridSizeY

    let ProcPercentage after_done func =
        let mutable passedTime : float32 = 0F

        (fun (x: float32) ->
            passedTime <- passedTime + x

            let percentage = min 1F passedTime
            if percentage >= 1F then after_done ()

            percentage >= 1F, func percentage)

    let movePart old size percentage = old - size * percentage

    let getNextMovementFunc (map: TileMap) (position: Vector2) (nextPos: Vector2) =
        let tileSizeY = map.CellSize.y
        let tileSizeX = map.CellSize.x

        let oldPosX = position.x
        let oldPosY = position.y

        (fun x -> new Vector2(movePart oldPosX (tileSizeX * nextPos.x) x, movePart oldPosY (tileSizeY * nextPos.y) x))

    let keepContained currentLocation movement (map: TileMap) =
        let currentMapCoords = map.WorldToMap(currentLocation)
        let mapCoords = currentMapCoords - movement

        let mutable newCoords = mapCoords

        newCoords.x <- max 1F mapCoords.x
        newCoords.x <- min 15F mapCoords.x
        newCoords.y <- max 1F mapCoords.y
        newCoords.y <- min 6F mapCoords.y

        let newMovement = currentMapCoords - newCoords

        newMovement


type PlayerFs() as this =
    inherit Sprite()

    let map = this.getNode<TileMap> "../Ground"

    let mutable run : Option<(float32 -> bool * unit)> = None

    member this.GoTo (nextPos: Vector2) (afterDone: unit -> unit) =
        let movementFunc =
            test.keepContained this.Position nextPos map.Value
            |> test.getNextMovementFunc map.Value this.Position

        run <-
            (fun (x: float32) -> this.Position <- movementFunc x)
            |> test.ProcPercentage afterDone
            |> Some

    override this._Process delta =
        match run with
        | Some x ->
            let (res, ()) = x delta
            if res then run <- None
        | None -> ()
