module FizzBuzz

let fizzBuzz x = match x % 3, x % 5 with 0,0 -> "FizzBuzz" | 0,_ -> "Fizz" | _,0 -> "Buzz" | _ -> string x

module Tests = 
    open Xunit
    open Swensen.Unquote

    type ``Given a fizzbuzz``() =
        let cases = 
            [
                (1,"1")
                (2,"2")
                (3,"Fizz")
                (4,"4")
                (5,"Buzz")
                (6,"Fizz")
                (7,"7")
                (8,"8")
                (9,"Fizz")
                (10,"Buzz")
                (11,"11")
                (12,"Fizz")
                (13,"13")
                (14,"14")
                (15,"FizzBuzz")
                (16,"16")
            ]
        [<Fact>]
        let ``should match cases``() = 
            cases |> Seq.iter (fun (input,expected) -> test <@ fizzBuzz input = expected @>)
            
