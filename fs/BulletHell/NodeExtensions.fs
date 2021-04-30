namespace BulletHell

module GDUtils =
    open Godot
    open FSharp.Json

    let loadTexture name =
        GD.Load<Texture>("res://assets/" + name + ".png")

    let setTexture name (sprite: Sprite) = sprite.Texture <- loadTexture name

    let loadCard name = loadTexture ("cards/" + name)

    type WrappedError<'a> =
        | Godot of Error
        | Custom of 'a

    let errorToResult (err: Error) =
        match err with
        | Error.Ok -> Ok()
        | x -> Result.Error(x)

    let errorToWrappedResult<'a> =
        function
        | Error.Ok -> Ok()
        | x -> x |> WrappedError<'a>.Godot |> Result.Error

    let godotResultToWrapped<'a, 'b> (res: Result<'b, _>) =
        match res with
        | Ok x -> Ok x
        | Result.Error x -> x |> WrappedError<'a>.Godot |> Result.Error

    let writeFile path text =
        let file = new File()

        let res =
            file.Open("user://" + path, File.ModeFlags.Write)
            |> errorToResult
            |> Result.map (fun _ -> file.StoreString text)

        file.Close()
        res

    let writeObjectToFile path obj =
        let asTxt = Json.serialize obj
        writeFile path asTxt


    let readFile path =
        let file = new File()

        let content =
            file.Open("user://" + path, File.ModeFlags.Read)
            |> errorToResult
            |> Result.map file.GetAsText

        file.Close()
        content

    let readFileAs<'t> path =
        path
        |> readFile
        |> godotResultToWrapped<exn, _>
        |> Result.bind
            (fun x ->
                try
                    x |> Json.deserialize<'t> |> Ok
                with x -> x |> Custom |> Result.Error)

    type Node with
        member this.getNode<'a when 'a :> Node and 'a: not struct>(path: string) =
            lazy (this.GetNode<'a>(new NodePath(path)))

        member this.addSprite name location =
            let sprite = new Sprite()
            setTexture name sprite
            sprite.Position <- location
            this.AddChild sprite

            sprite

        member this.getChildren() = this.GetChildren() |> Seq.cast<Node>
