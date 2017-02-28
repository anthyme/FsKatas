module Reversi

type Player = White | Black
type Position = int * int
type Pawn = { Player:Player; Position:Position }

let boardSize = 8
let directions = [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)]

type PathParser<'a> = Continue of 'a | Result of bool with static member map = function Some x -> Continue x | _ -> Result false
let (>>=) v f = match v with | Continue s -> f s | Result x -> Result x
    
let nextPosition (x,y) (dx,dy) = (x+dx , y+dy)

let isInBoard (x,y) = if x >= 0 && y >= 0 && x < boardSize && y < boardSize then Continue (x,y) else Result false

let hasPound pawns (x,y) = pawns |> Seq.tryFind (fun p -> p.Position = (x,y)) |> PathParser.map

let foundFriend player surronding pawn = if pawn.Player = player && surronding then Result true else Continue pawn

let checkSurronding player surronding pawn = (pawn, if pawn.Player <> player then true else surronding) |> Continue

let rec isValidPath pawns player surronding position direction =
    nextPosition position direction
    |> isInBoard
    >>= (hasPound pawns)
    >>= (foundFriend player surronding)
    >>= (checkSurronding player surronding)
    |> function | Result isValid -> isValid 
                | Continue (pawn,surronding) -> isValidPath pawns player surronding pawn.Position direction

let isValidBox pawns player position = directions |> Seq.map (isValidPath pawns player false position) |> Seq.exists ((=) true)

let nextReversi pawns player = [for x in [0..boardSize-1] do for y in [0..boardSize-1] do yield (x,y)]
                               |> List.filter (isValidBox pawns player)


module Tests = 
    open Xunit
    open Helpers

    type ``Given an next reversi finder``() = 
        [<Fact>] 
        let ``sample 1``() = nextReversi [] Black |> ``should be empty``

        [<Fact>] 
        let ``sample 2``() = 
            let input = [{ Player = Black; Position = (4,4)}
                         { Player = Black; Position = (3,3)}
                         { Player = White; Position = (4,3)}
                         { Player = White; Position = (3,4)}]
            nextReversi input Black |> set |> ``should equal`` (set [(4,2);(2,4);(5,3);(3,5)])
