module Sokoban

type Cell =  
    | EmptyCell
    | Wall 
    | Box 
    | Target 
    | BoxOnTarget 
    | Player
    | PlayerOnTarget

type Direction = Left | Up | Right | Down
type Position = int * int
type GameState = Cell[,]


let cellMap = 
    [' ',EmptyCell;
     '#',Wall;
     '$',Box;
     '.',Target;
     '*',BoxOnTarget;
     '@',Player;
     '+',PlayerOnTarget]

let charToCell = cellMap |> Map.ofList 
let cellToChar = cellMap |> Seq.map (fun (x,y) -> y,x) |> Map.ofSeq

let parseCell x = Map.find x charToCell
let cellToString cell = Map.find cell cellToChar

let parseGameState (lines:string list) =
    let result = lines |> List.map (fun x -> Seq.map parseCell x |> Seq.toList)
    let width, height = lines.Head.Length, lines.Length
    Array2D.init  height width (fun i j -> result.[j].[i])

let parseLines (text:string) = text.Split [|'\n'|] |> Seq.toList

let printGameState state = 
    let width = Array2D.length1 state
    Array2D.iteri (fun i j x -> (if j = width - 1 then printfn else printf) "%c" (cellToString x)) state

let findPlayerPosition state  =
    seq { for i in 0 .. Array2D.length1 state - 1 do
          for j in 0 .. Array2D.length2 state - 1 do
              match state.[i,j] with
              | Player | PlayerOnTarget -> yield  i,j,state.[i,j]
              | _ -> () } |> Seq.head

let getCell (state:GameState) (x,y) = 
    try state.[x,y] with | ex -> EmptyCell   

let move direction (state:GameState) =
    let px,py,pc = findPlayerPosition state
    let getCell = getCell state
    let (nx,ny), (bx,by) =
        match direction,px,py with
        | Left,x,y  -> (x-1,y),(x-2,y)
        | Right,x,y -> (x+1,y),(x+2,y)
        | Up,x,y    -> (x,y-1),(x,y-2)
        | Down,x,y  -> (x,y+1),(x,y+2)

    let currentIfMoved = match pc with | Player -> EmptyCell | PlayerOnTarget -> Target | _ -> failwith "paf"

    match getCell (nx,ny), getCell (bx,by) with
    | EmptyCell,_           -> Some Player, None
    | Target,_              -> Some PlayerOnTarget, None
    | Box,Target            -> Some Player, Some BoxOnTarget
    | Box,EmptyCell         -> Some Player, Some Box
    | BoxOnTarget,EmptyCell -> Some PlayerOnTarget, Some Box
    | BoxOnTarget,Target    -> Some PlayerOnTarget, Some BoxOnTarget
    | _ -> None,None
    |> function
    | Some(nextCell),None               -> state.[px,py] <- currentIfMoved;state.[nx,ny] <- nextCell;state
    | Some(nextCell),Some(behindCell)   -> state.[px,py] <- currentIfMoved;state.[nx,ny] <- nextCell;state.[bx,by] <- behindCell;state
    | _,_                               -> state

        










let myBoard = 
    """#######
#.  # #
#$*@$ #
#   $ #
# ..  #
#  *  #
#######"""

let gameState = myBoard |> parseLines |> parseGameState 


gameState |> printGameState

let testUp = move Up gameState          |> printGameState
let testDown = move Down gameState      |> printGameState
let testLeft = move Left gameState      |> printGameState
let testRight = move Right gameState    |> printGameState
