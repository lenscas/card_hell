namespace BulletHell

open Godot

type Library(newCards) =
    let mutable deck : List<Cards> = []
    let mutable casted : List<Cards> = newCards
    let mutable hand : List<Cards> = []

    let shuffle () =
        let random = new System.Random()

        let x =
            List.sortBy (fun _ -> random.Next()) casted

        deck <- x
        casted <- []

    member __.getImages() = hand |> List.map cards.ToImage

    member this.addCard card times =
        if times = 0 then
            ()
        else
            casted <- card :: casted
            this.addCard card (times - 1)

    member this.fillHand() =
        if hand.Length = 4 then
            ()
        else
            match deck with
            | [] ->
                shuffle ()

                if deck.Length = 0 then
                    raise (new System.Exception("deck is empty"))

                this.fillHand ()
            | head :: tail ->
                hand <- head :: hand
                deck <- tail
                this.fillHand ()

    member __.getCardFromHand index =
        let (_, card) =
            hand
            |> Seq.indexed
            |> Seq.find (fun (x, _) -> x = index)

        card

    member this.getRandomFromHand() =
        if hand.Length <> 4 then
            this.fillHand ()

        let random = new System.Random()
        (hand |> List.sortBy (fun _ -> random.Next())).[0]

    member __.invalidateHand() =
        casted <- List.concat [ hand; casted ]
        hand <- []

    member this.removeFromHand first second =
        let clonedCards =
            hand
            |> Seq.mapi (fun v i -> (v, i))
            |> Seq.filter ((fun (index, _) -> index = first || index = second))
            |> Seq.map (fun (_, card) -> card)
            |> Seq.toList

        GD.Print(first, second)
        GD.Print(clonedCards)
        casted <- List.concat [ clonedCards; casted ]

        hand <-
            hand
            |> Seq.mapi (fun v i -> (v, i))
            |> Seq.filter ((fun (index, _) -> index <> first && index <> second))
            |> Seq.map (fun (_, card) -> card)
            |> Seq.toList

        GD.Print("Hand length: ", hand.Length)
        this.fillHand ()
        ()
