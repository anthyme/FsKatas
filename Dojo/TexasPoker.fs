module TexasPoker

open Helpers

type Suit = Spade|Heart|Club|Diamond
type Rank = Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King|Ace
type Card = Rank * Suit  
type Combination = HighCard|OnePair|TwoPair|ThreeSame|Straight|Flush|FullHouse|FourSame|StraightFlush|RoyalStraightFlush

let rankOf card = fst card
let suitOf card = snd card

let parseSuit = function 's' -> Spade | 'h' -> Heart | 'c' -> Club | 'd' -> Diamond

let parseRank = function 
    | 'T' -> Ten | 'J' -> Jack | 'Q' -> Queen | 'K' -> King | 'A' -> Ace
    | x -> [Two;Three;Four;Five;Six;Seven;Eight;Nine].[(int << string) x - 2]
            
let convertCardSet (txt:string) = txt.Split([|' '|]) |> Seq.map (fun x -> parseRank x.[0], parseSuit x.[1]) |> List.ofSeq

let getGroups f size cards = 
    Seq.groupBy f cards |> Seq.map (Tuple.mapSnd Seq.length) |> (Seq.filter (snd >> (=) size) >> Seq.length)

let (|IsGroup|_|) pair three four cards = 
    let check size expected = getGroups fst size cards >= expected
    ((check 4 four) && (check 2 pair) && (check 3 three)) |> ifSome IsGroup
    
let (|IsFlush|_|) cards = getGroups suitOf 5 cards >= 1 |> ifSome IsFlush

let (|RankStraight|SameRank|Nothing|) (a,b) =
    match a, b, compare a b with
    | Ace,Two,_ | _,_,-1 -> RankStraight
    | _,_, 0 -> SameRank
    | _ -> Nothing

let (|CardStraight|Nothing|) cards =
    let folder cards card =
        match cards,card with
        | [],c -> [c]
        | cards,_ when Seq.length cards = 5 -> cards
        | head::tail,c -> Tuple.map rankOf (head,c) |> function RankStraight -> c :: cards | SameRank -> cards | _ -> []
        | _ -> []
    let initial = match List.last cards with Ace,s -> [Ace,s] | _ -> []
    match Seq.fold folder initial cards |> Seq.length with 5 -> CardStraight | _ -> Nothing 

let (|IsStraight|IsStraightFlush|IsRoyalStraightFlush|Nothing|) cards = 
    let flushs = Seq.groupBy suitOf cards |> Seq.filter (suitOf >> Seq.length >> (<=) 5) |> Seq.map (suitOf >> List.ofSeq) |> Seq.tryHead
    match cards,flushs with
    | _, Some([_; Ten,_; Jack,_; Queen,_; King,_; Ace,_]) -> IsRoyalStraightFlush
    | _, Some(CardStraight) -> IsStraightFlush
    | CardStraight, _  -> IsStraight
    | _ -> Nothing

let findCombination (cardSet:string) = 
    match cardSet |> convertCardSet |> List.sort with
    | IsRoyalStraightFlush -> RoyalStraightFlush
    | IsStraightFlush -> StraightFlush
    | IsGroup 0 0 1 -> FourSame
    | IsGroup 1 1 0 -> FullHouse
    | IsFlush       -> Flush
    | IsStraight    -> Straight
    | IsGroup 0 1 0 -> ThreeSame
    | IsGroup 2 0 0 -> TwoPair
    | IsGroup 1 0 0 -> OnePair
    | _ -> HighCard

let compareCardSets cardSet1 cardSet2  = compare (findCombination cardSet1) (findCombination cardSet2)

open FsUnit.Xunit
open Xunit

type ``Given a card set`` () =
    let shouldConvertTo result cardSet = cardSet |> convertCardSet |> should equal result
    let shouldMatchCombination result cardSet = cardSet |> findCombination |> should equal result
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

