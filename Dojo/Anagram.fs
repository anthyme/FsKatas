module Anagram

open Helpers

let words = Http.get "http://codekata.com/data/wordlist.txt" |> String.split '\n' |> List.ofSeq
let getKey word = word |> Seq.sortBy int |> Seq.map string |> String.concat ""
let dictionary = words |> Seq.groupBy getKey |> Map.ofSeq
let findAnagram word = dictionary.[getKey word] |> Seq.filter ((<>) word) 
module Tests = 
    open Xunit
    open FsUnit.Xunit

    type ``Given an anagram finder``() = 
        [<Fact>] 
        let ``dog should give god``() = findAnagram "dog" |> should contain "god"
        [<Fact>] 
        let ``hello has no anagram``() = findAnagram "hello" |> should be Empty
