module MarsRover

let split char (txt:string) = txt.Split([| char |])
type Area = int * int 
type Position = int * int 
type Direction = North | East | South | West
type Movement = Forward | Left | Right | Backward
type RobotState = Direction * Position

let parseDirection = function | "N" -> North | "E" -> East | "S" ->  South | "W" -> West
let parseMove = function | 'F' -> Forward | 'L' -> Left | 'R' ->  Right | 'B' -> Backward

let turnRight = function | North -> East | East -> South | South ->  West | West -> North
let turnLeft  = function | North -> West | East -> North | South ->  East | West -> South
let revertDirection = function | North -> South | East -> West | South ->  North | West -> East

let move position (w,h) direction = 
    match position,direction with
    | (x,0) , North -> x,h-1
    | (x,y) , East  when x = w - 1 -> 0,y
    | (x,y) , South when y = h - 1 -> x,0
    | (0,y) , West  -> w-1,y
    | (x,y) , North -> x,y-1
    | (x,y) , East  -> x+1,y
    | (x,y) , South -> x,y+1
    | (x,y) , West  -> x-1,y

let formatDirection = function | North -> "N"  | East -> "E" | South -> "S" | West -> "W"
let formatState (direction, (x,y)) = sprintf "%d %d %s" x y (formatDirection direction)


let sphereMove (input:string) = 
    let lines = split '\n' input
    let area:Area = lines.[0] |> split ' ' |> fun x -> int x.[0], int x.[1]
    let initialState  = lines.[1] |> split ' ' |> fun x -> parseDirection x.[2], (int x.[0], int x.[1])
    let moves = lines.[2] |> Seq.map parseMove |> List.ofSeq

    let reduce (direction,position) = function
        | Right -> turnRight direction, position
        | Left -> turnLeft direction, position
        | Forward -> direction, move position area direction
        | Backward -> direction, move position area (revertDirection direction)

    Seq.fold reduce initialState moves |> formatState



open Xunit
open FsUnit.Xunit

module Tests = 
    type ``Given a stuff``() = 
        [<Fact>] let ``simple move``() =  "20 20\n5 15 N\nFFFFFRBBLFLLLFFFFF" |> sphereMove |> should equal "8 9 E"

        [<Fact>]
        let ``sphere move1``() = 
            "5 5\n\
            0 0 N\n\
            F" 
            |> sphereMove |> should equal "0 4 N"

        [<Fact>]
        let ``sphere move2``() = 
            "5 5\n\
            0 0 E\n\
            BLL" 
            |> sphereMove |> should equal "4 0 W"