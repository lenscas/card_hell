namespace BulletHell

open Godot

type TimerType =
    | Jam = 0
    | Relaxed = 1
    | EverShrinking = 2

type Timer =
    | Jam of float32
    | Relaxed of float32
    | EverShrinking of float32 * float32

module timer =
    let tick delta timer =
        match timer with
        | Jam x -> Jam(x - delta)
        | Relaxed x -> Relaxed x
        | EverShrinking (x, y) -> EverShrinking((x - delta), y)

    let addTurn =
        function
        | Jam x -> Jam x
        | Relaxed x -> Relaxed(x - 1F)
        | EverShrinking (x, y) ->
            let y = y - 1F
            EverShrinking(y, y)

    let collectBattery amount timer =
        match timer with
        | Jam x -> Jam(x + amount)
        | Relaxed x -> Relaxed(x + amount)
        | EverShrinking (x, y) -> EverShrinking(x, (y + amount))

    let maxBatterySize =
        function
        | Jam _ -> 10
        | Relaxed _ -> 5
        | EverShrinking _ -> 5

    let maxTime =
        function
        | TimerType.Jam -> 10F
        | TimerType.Relaxed -> 5F
        | TimerType.EverShrinking -> 10F
        | x ->
            let error =
                (new System.Exception(x.ToString() + " Is an invalid timer type"))

            error.Data.Add("type", x)
            raise error

    let toType =
        function
        | Jam _ -> TimerType.Jam
        | Relaxed _ -> TimerType.Relaxed
        | EverShrinking (_, _) -> TimerType.EverShrinking

    let create timerType =
        let max = maxTime timerType

        match timerType with
        | TimerType.Jam -> Jam max
        | TimerType.Relaxed -> Relaxed max
        | TimerType.EverShrinking -> EverShrinking(max, max)
        | x ->
            let error =
                (new System.Exception(x.ToString() + " Is an invalid timer type"))

            error.Data.Add("type", x)
            raise error

    let getTimeLeft =
        function
        | Jam x -> x
        | Relaxed x -> x
        | EverShrinking (x, _) -> x

    let reset =
        function
        | Jam _ -> create TimerType.Jam
        | Relaxed _ -> create TimerType.Relaxed
        | EverShrinking (_, _) -> create TimerType.EverShrinking
