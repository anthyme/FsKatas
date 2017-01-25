module Anagram


let findAnagram word = ["god"]


module Tests = 
    open Xunit
    open FsUnit.Xunit

    type ``Given an anagram finder``() = 
        [<Fact>] 
        let ``foo should give oof``() = findAnagram "dog" |> should contain "god"
