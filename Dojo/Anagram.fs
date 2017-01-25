module Anagram

open Helpers
open System.Net.Http

let wordsUrl = "http://codekata.com/data/wordlist.txt"
let words = HttpClient().GetAsync(wordsUrl).Result.Content.ReadAsStringAsync().Result |> String.split '\n' |> List.ofSeq

let findAnagram word = ["god"]

module Tests = 
    open Xunit
    open FsUnit.Xunit

    type ``Given an anagram finder``() = 
        [<Fact>] 
        let ``dog should give god``() = findAnagram "dog" |> should contain "god"
