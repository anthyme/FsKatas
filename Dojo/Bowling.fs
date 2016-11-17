module Bowling

open Helpers

type Shot =
| Miss
| Knock of int
| Spare

type Frame =
| Start
| FirstShot of Shot
| SimpleShots
| Spared

type State = { Score:int ; Last:Frame }

let computeScore line =
    let shotOf = function
        | '-' -> Miss
        | '/' -> Spare
        | x -> Knock (Int.ofChar x)

    let folder (state:State) shot = 
        let increment, frame =
            match state.Last, shot with
            | Spared, Knock x -> (x + 10, FirstShot shot)
            | FirstShot(Knock x), Knock y -> (x + y, SimpleShots)
            | FirstShot(Knock x), Miss -> x, SimpleShots
            | FirstShot(_), Miss -> 0, SimpleShots
            | _, Spare -> 0, Spared
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
        