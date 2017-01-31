module Reversi

type Player = White | Black
type Position = int * int
type Direction = int * int
type PawnPosition = Player * Position

let BOARD_SIZE = 8
let directions = [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)]

type ValidFinder<'a> = Continue of 'a | Found of bool
let bind f = function | Continue s -> f s | Found x -> Found x
let (>>=) a b = bind b a 

let nextPosition ((x,y),(dx,dy)) = (x+dx , y+dy) |> Continue

let isInBoard (x,y) = if x >= 0 && y >= 0 && x < BOARD_SIZE && y < BOARD_SIZE then Continue (x,y) else Found false

let hasPound pawns (x,y) = 
    pawns 
    |> Seq.tryFind (snd >> (=) (x,y))
    |> function | Some x -> Continue x | _ -> Found false

let foundFriend player surronding ((pawnPlayer,_) as pawn) = 
    if pawnPlayer = player && surronding then Found true else Continue pawn

let checkSurronding player surronded (pawnPlayer,next) = 
    let surrounding = if pawnPlayer <> player then true else surronded in Continue (next, surrounding)

let rec isValidDirection position pawns surronding player direction =
    (position,direction)
    |> nextPosition 
    >>= isInBoard
    >>= (hasPound pawns)
    >>= (foundFriend player surronding)
    >>= (checkSurronding player surronding)
    |> function | Found isValid -> isValid | Continue (next,surronding) -> isValidDirection next pawns surronding player direction

let isValidPosition pawns player position =
    directions
    |> Seq.map (isValidDirection position pawns false player)
    |> Seq.exists ((=) true)

let getReversiAvailableBoxes pawns nextPlayer =
    [for x in [0..BOARD_SIZE-1] do for y in [0..BOARD_SIZE-1] do yield (x,y)] |> List.filter (isValidPosition pawns nextPlayer)



module Tests = 
    open Xunit
    open FsUnit.Xunit

    type ``Given an get reversi available boxes``() = 
        [<Fact>] 
        let ``sample 1``() = 
            getReversiAvailableBoxes [] Black |> should be Empty

        [<Fact>] 
        let ``sample 2``() = 
            let input = 
                [Black, (4,4)
                 Black, (3,3)
                 White, (4,3)
                 White, (3,4)]
            getReversiAvailableBoxes input Black |> set |> should equal (set [(4,2);(2,4);(5,3);(3,5)])
