namespace BulletHell

module GDUtils =
    open Godot

    let loadTexture name =
        GD.Load<Texture>("res://assets/" + name + ".png")

    let setTexture name (sprite: Sprite) = sprite.Texture <- loadTexture name

    let loadCard name = loadTexture ("cards/" + name)

    type Node with
        member this.getNode<'a when 'a :> Node and 'a: not struct>(path: string) =
            lazy (this.GetNode<'a>(new NodePath(path)))

        member this.addSprite name location =
            let sprite = new Sprite()
            setTexture name sprite
            sprite.Position <- location
            this.AddChild sprite

            sprite
