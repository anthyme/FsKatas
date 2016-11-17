module Bowling

open Helpers

let computeScore line =
    let valueOf = function
        | '-' -> 0
        | x -> Int.ofChar x
    line |> Seq.sumBy valueOf 

module Tests =
    open FsUnit.Xunit
    open Xunit

    type ``Given a computeScore`` () =

        [<Fact>] let ``compute simple facts`` () = "9-9-9-9-9-9-9-9-9-9-" |> computeScore |> should equal (9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9 + 9)
