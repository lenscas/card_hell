namespace BulletHell

open Godot

type State =
    | Hidden
    | Cast of (Cards -> unit)
    | Discard of (int * (Cards -> unit))

type HandFs() as this =
    inherit Node2D()

    let cardImages =
        lazy
            (let children = this.GetChildren()

             [ children.[0] :?> TextureButton
               children.[1] :?> TextureButton
               children.[2] :?> TextureButton
               children.[3] :?> TextureButton ])

    let deck =
        new Library(
            [ MoveUp
              MoveDown
              MoveLeft
              MoveRight
              MoveLeft
              MoveRight
              SpawnBullets ]
        )

    let mutable currentState = Hidden

    member __.AddCard card times = deck.addCard card times

    member __.ClickedCard(index: int) =

        match currentState with
        | Hidden -> ()
        | Cast func ->
            currentState <- Discard((index), func)

            let card = cardImages.Value.[index]
            card.Hide()
        | Discard (card, func) ->
            if card = index then
                ()
            else
                card |> deck.getCardFromHand |> func
                currentState <- Hidden
                deck.removeFromHand card index

                for card in cardImages.Value do
                    card.Hide()

    member public __.StartCastTime func =
        deck.fillHand ()
        let images = deck.getImages ()

        for (index, card) in cardImages.Value |> Seq.indexed do
            card.set_TextureNormal (images.[index])
            card.Show()


        currentState <- Cast func

    override this._Ready() =
        for (i, card) in cardImages.Value |> Seq.indexed do
            let bindings = new Collections.Array()
            i |> bindings.Add |> ignore

            ignore (card.Connect("pressed", this, nameof (this.ClickedCard), bindings))
