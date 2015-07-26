namespace Tests

open Xunit
open FsUnit.Xunit

open Dojo

module Tests = 
    type ``Given a Class1 object``() = 
        let sut = new Class1()
        [<Fact>]let ``when created then X should be F#``() = sut.X |> should equal "F#"
        [<Fact>]let ``true should be true``() = true |> should be True
        [<Fact>]let ``false should not be false``() = false |> should not' (be True)

