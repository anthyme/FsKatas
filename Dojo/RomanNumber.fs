module RomanNumber

let arabicToRoman (value:int) =
    let symbols = ["I"; "V"; "X"; "L"; "C"; "D"; "M"]
    let generate x symbol = List.replicate x symbol |> List.fold (+) ""
    let romanPattern symbol1 symbol2 = function
        | 4 -> symbol1 + symbol2
        | x -> symbol2 + (generate (x-5) symbol1)
    let rec arabicToRoman' symbolPosition value =
        let symbol i = symbols.[symbolPosition + i]
        let roman =
            match value % 10 with
            | x when x < 4 -> generate x (symbol 0)
            | x when x < 9 -> romanPattern (symbol 0) (symbol 1) x
            | 9 -> (symbol 0) + (symbol 2)
        match value / 10 with | x when x > 0 -> (arabicToRoman' (symbolPosition + 2) x) + roman | _ -> roman
    arabicToRoman' 0 value

module Tests = 
    open Xunit
    open FsUnit.Xunit

    let testCases = 
        Map [
            1    , "I"
            2    , "II"
            3    , "III"
            4    , "IV"
            5    , "V"
            6    , "VI"
            7    , "VII"
            8    , "VIII"
            9    , "IX"
            10   , "X"
            11   , "XI"
            12   , "XII"
            13   , "XIII"
            14   , "XIV"
            15   , "XV"
            16   , "XVI"
            17   , "XVII"
            18   , "XVIII"
            42   , "XLII"
            42   , "XLII"
            666  , "DCLXVI"
            2016 , "MMXVI"
            3999 , "MMMCMXCIX"
        ]
    type ``Given an arabicToRoman converter``() = 
        [<Fact>] 
        let ``should match cases``() = 
            testCases |> Map.iter (fun arabic roman -> arabic |> arabicToRoman |> should equal roman)