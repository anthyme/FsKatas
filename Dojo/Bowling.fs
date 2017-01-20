module Bowling

open Helpers

type Shot =
| Miss
| Knock of int
| Spare
| Strike

type Frame =
| Start
| FirstShot of Shot
| SimpleShots
| Spared
| Striked

type State = { Score:int ; Last:Frame }

let computeScore line =
    let shotOf = function
        | '-' -> Miss
        | '/' -> Spare
        | 'X' -> Strike
        | x -> Knock (Int.ofChar x)

    let valueOf = function | Knock x -> x | _ -> 0

    let folder (state:State) shot = 
        let increment, frame =
            match state.Last, shot with
            | _, Strike -> 10, Striked
            | _, Spare -> 10, Spared
            | Spared, shot -> (valueOf shot, FirstShot shot)
            | FirstShot(shot1), shot2 -> (valueOf shot1 + valueOf shot2, SimpleShots)
            | _, shot -> 0, FirstShot(shot)

        {Score = state.Score + increment ; Last = frame}

    let result = line |> Seq.map shotOf |> Seq.fold folder ({Score = 0; Last = Start})
    result.Score

module Tests =
    open FsUnit.Xunit
    open Xunit

    type ``Given a computeScore`` () =

        [<Fact>]
        let ``compute simple facts`` () = 
            "9-9-9-9-9-9-9-9-9-9-" |> computeScore |> should equal (9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9)

        [<Fact>]
        let ``compute spares`` () = 
            "5/5/5/5/5/5/5/5/5/5/5" |> computeScore |> should equal (10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5 + 10+5)

        [<Fact>]
        let ``compute mixed simple and spares`` () = 
            "5/2/5/5/5/5/345/5/5/5" |> computeScore |> should equal (10+5 + 10+2 + 10+5 + 10+5 + 10+5 + 10+3 + 3+4 + 10+5 + 10+5 + 10+5)

        [<Fact>]
        let ``compute mixed simple, miss and spares`` () = 
            "5/2/5/5/5/5/-45/5/5/5" |> computeScore |> should equal (10+5 + 10+2 + 10+5 + 10+5 + 10+5 + 10+0 + 0+4 + 10+5 + 10+5 + 10+5)

        [<Fact>]
        let ``compute strikes`` () = 
            "XXXXXXXXXXXX" |> computeScore |> should equal (10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10 + 10+10+10)
        
