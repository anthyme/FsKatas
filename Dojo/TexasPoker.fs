module TexasPoker

open Helpers

type Suit = Spade|Heart|Club|Diamond
type Rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
type Card = Rank * Suit  
type Combination = HighCard|OnePair|TwoPair|ThreeSame|Straight|Flush|FullHouse|FourSame|StraightFlush|RoyalStraightFlush
type Hand = Combination * Card list

[<AutoOpen>]
module Helpers =
    let rankOf card = fst card
    let suitOf card = snd card

    let parseSuit = function 's' -> Spade | 'h' -> Heart | 'c' -> Club | 'd' -> Diamond

    let parseRank = function 
        | 'T' -> Ten | 'J' -> Jack | 'Q' -> Queen | 'K' -> King | 'A' -> Ace
        | x -> [Two;Three;Four;Five;Six;Seven;Eight;Nine].[Int.ofChar x - 2]

    let parseCard (txt: string) = (parseRank txt.[0], parseSuit txt.[1])
            
    let convertCardSet (txt:string) = txt |> String.split ' ' |> Seq.map parseCard |> List.ofSeq

    let getGroups groupFun groupSize cards = 
        Seq.groupBy groupFun cards 
        |> Seq.map (fun (fr,cards) -> (fr, Seq.length cards, cards))
        |> Seq.filter (fun (_,count,_) ->  count = groupSize)

    let extract5BestCards combinationCards allCards =
        allCards
        |> Seq.except combinationCards
        |> Seq.sort
        |> Seq.rev
        |> Seq.take (5 - (Seq.length combinationCards))
        |> Seq.append combinationCards
        |> List.ofSeq

    let (|RankFollowing|SameRank|Nothing|) (a,b) =
        match a, b, compare a b with
        | Ace,Two,_ | _,_,-1 -> RankFollowing
        | _,_, 0 -> SameRank
        | _ -> Nothing

    let (|CardFollowing|Nothing|) cards =
        let folder cards card =
            match cards,card with
            | [],c -> [c]
            | cards,_ when Seq.length cards = 5 -> cards
            | head::tail,c -> Tuple.map rankOf (head,c) |> function 
                              | RankFollowing -> c :: cards 
                              | SameRank -> cards 
                              | _ -> []
            | _ -> []

        cards
        |> Seq.fold folder (List.last cards |> function | Ace,s -> [Ace,s] | _ -> []) 
        |> Seq.length
        |> function
        | 5 -> CardFollowing 
        | _ -> Nothing

[<AutoOpen>]
module CombinationActivePatterns =
    let (|IsFlush|_|) cards = getGroups suitOf 5 cards |> Seq.length >= 1 |> ifSome (IsFlush [])

    let (|IsGroup|_|) expected2 expected3 expected4 (cards: Card list) =   
        let check groupSize expected = 
            let groups = getGroups rankOf groupSize cards
            match Seq.length groups >= expected with
            | true -> (true, groups |> Seq.collect (fun (_, _, c) -> c) |> Seq.sortBy snd |> List.ofSeq)
            | _ -> (false, [])
        
        [(4, expected4) ; (3, expected3) ; (2, expected2)] 
        |> List.map (fun x -> x ||> check) 
        |> List.unzip
        |> Tuple.mapFst (Seq.fold (&&) true) 
        |> Tuple.mapSnd List.concat
        |> fun (groupMatched, matchedCards) -> ifSome (IsGroup (extract5BestCards matchedCards cards)) groupMatched

    let (|IsStraight|IsStraightFlush|IsRoyalStraightFlush|Nothing|) (cards: Card list) = 
        let flushs = Seq.groupBy suitOf (cards:Card list) 
                    |> Seq.filter (suitOf >> Seq.length >> (<=) 5) 
                    |> Seq.map (suitOf >> List.ofSeq) 
                    |> Seq.tryHead
        match cards,flushs with
        | _, Some([_; Ten,_; Jack,_; Queen,_; King,_; Ace,_]) -> IsRoyalStraightFlush (Option.get flushs)
        | _, Some(CardFollowing) -> IsStraightFlush (Option.get flushs)
        | CardFollowing, _  -> IsStraight cards
        | _ -> Nothing ([] :> Card list)

let getHand (cardSet:string) = 
    match cardSet |> convertCardSet |> List.sort with
    | IsRoyalStraightFlush cards -> RoyalStraightFlush, cards
    | IsStraightFlush cards -> StraightFlush, cards
    | IsGroup 0 0 1 cards -> FourSame, cards
    | IsGroup 1 1 0 cards -> FullHouse, cards
    | IsFlush       cards -> Flush, cards
    | IsStraight    cards -> Straight, cards
    | IsGroup 0 1 0 cards -> ThreeSame, cards
    | IsGroup 2 0 0 cards -> TwoPair, cards
    | IsGroup 1 0 0 cards -> OnePair, cards
    | cards -> HighCard, cards

let compareCardSets cardSet1 cardSet2  = 
    [cardSet1;cardSet2] |> List.map (getHand >> (Tuple.mapSnd (List.map rankOf))) |> Tuple.ofList ||> compare

module Tests =
    open FsUnit.Xunit
    open Xunit

    type ``Given a card set`` () =
        let shouldConvertTo result cardSet = cardSet |> convertCardSet |> should equal result
        let shouldMatchCombination result cardSet = cardSet |> getHand |> fst |> should equal result
        let shouldBeBetterThan cardSet1 cardSet2 = compareCardSets cardSet1 cardSet2 |> should be (lessThan 0)

        [<Fact>] 
        let ``is convertible to model``() = 
            "Ad 2h 3d 8h Qh Tc Kc" |> shouldConvertTo [Ace,Diamond; Two,Heart; Three,Diamond; Eight,Heart; Queen,Heart; Ten,Club; King,Club]

        [<Fact>] let ``is a high card`` ()                      = "Ad 2h 3d 8h Qh Tc Kc" |> shouldMatchCombination HighCard
        [<Fact>] let ``is a one pair`` ()                       = "3d 8h Qh Tc Kc 2d 2h" |> shouldMatchCombination OnePair
        [<Fact>] let ``is a two pair`` ()                       = "2d 2h 3d 3h Qh Tc Kc" |> shouldMatchCombination TwoPair
        [<Fact>] let ``is a three same`` ()                     = "2d 2h 2c 4d 3h Qh Tc" |> shouldMatchCombination ThreeSame
        [<Fact>] let ``is a straight`` ()                       = "2d 3h 4c 5d 6h Qh Tc" |> shouldMatchCombination Straight
        [<Fact>] let ``is a straight starting by ace`` ()       = "Ad 2h 3c 4d 5h Qh Tc" |> shouldMatchCombination Straight
        [<Fact>] let ``is a straight ending by ace`` ()         = "Ad Kh Qc Jd Th Qh Tc" |> shouldMatchCombination Straight
        [<Fact>] let ``is a flush`` ()                          = "2h 5h 3h 8h Qh Tc Kc" |> shouldMatchCombination Flush
        [<Fact>] let ``is a full house`` ()                     = "2d 2h 2c 3d 3h Qh Tc" |> shouldMatchCombination FullHouse
        [<Fact>] let ``is a four same`` ()                      = "2d 2h 2c 2s 3h Qh Tc" |> shouldMatchCombination FourSame
        [<Fact>] let ``is a straight flush`` ()                 = "2h 3h 4h 5h 6h Qh Tc" |> shouldMatchCombination StraightFlush
        [<Fact>] let ``is a straight flush starting by ace`` () = "Ah 2h 3h 4h 5h Qh Tc" |> shouldMatchCombination StraightFlush
        [<Fact>] let ``is a royal straight flush`` ()           = "Ah Kh Qh Jh Th 5h Tc" |> shouldMatchCombination RoyalStraightFlush

        [<Fact>] let ``one pair is better than high card`` ()   = "3d 8h Qh Tc Kc 2d 2h" |> shouldBeBetterThan "Ad 2h 3d 8h Qh Tc Kc"
        [<Fact>] let ``four same is better than one pair`` ()   = "2d 2h 2c 2s 3h Qh Tc" |> shouldBeBetterThan "3d 8h Qh Tc Kc 2d 2h"

        [<Fact>] let ``four Aces is better than four Twos`` ()  = "Ad Ah Ac As 3h Qh Tc" |> shouldBeBetterThan "2d 2h 2c 2s 3h Qh Tc"
        [<Fact>] let ``two pairs ace is better than king`` ()   = "2c 3c 2s 3s Qh Ac Kc" |> shouldBeBetterThan "2d 2h 3d 3h Qh Tc Kc"
        [<Fact>] let ``full with ace is better than twos`` ()   = "Ad Ah Ac Qs 3h Qh Tc" |> shouldBeBetterThan "2d 2h 2c Qc 3s Qs Ts"
