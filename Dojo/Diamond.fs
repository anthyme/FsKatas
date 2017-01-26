module Diamond

[<Literal>]
let A = 'A'

let indexOf letter = (int letter) - (int A)

let spaces count = String.replicate count " "

let middleSpaces letter = spaces (1 + (indexOf letter-1) * 2)

let lineOf higherLetter letter = 
    match letter with
    | A -> (spaces (indexOf higherLetter), "", string A)
    | letter -> (spaces (indexOf higherLetter - indexOf letter), string letter, middleSpaces letter)
    |> fun (side,between,middle) -> side + between + middle + between + side + "\n"

let createDiamond letter =
    let letters = [A..int letter - 1 |> char]
    letters @ [letter] @ (List.rev letters) |> Seq.map (lineOf letter) |> Seq.reduce (+)




module Tests = 
    open Xunit
    open FsUnit.Xunit

    type ``Given a letter should return the right diamond``() = 
        let diamondB = " A \n" +
                       "B B\n" +
                       " A \n"

        let diamondC = "  A  \n" +
                       " B B \n" +
                       "C   C\n" +
                       " B B \n" +
                       "  A  \n"
    
        let diamondF = "     A     \n" +
                       "    B B    \n" +
                       "   C   C   \n" +
                       "  D     D  \n" +
                       " E       E \n" +
                       "F         F\n" +
                       " E       E \n" +
                       "  D     D  \n" +
                       "   C   C   \n" +
                       "    B B    \n" +
                       "     A     \n"
    
        [<Fact>] member x.``Given A should return the A diamond``() = createDiamond 'A' |> should equal "A\n"
        [<Fact>] member x.``Given B should return the B diamond``() = createDiamond 'B' |> should equal diamondB
        [<Fact>] member x.``Given C should return the C diamond``() = createDiamond 'C' |> should equal diamondC
        [<Fact>] member x.``Given F should return the F diamond``() = createDiamond 'F' |> should equal diamondF

    type ``Given a char capital letter should return the correct position in alphabet``() = 
        [<Fact>] member x.``Given 'A' should return 0 ``() = indexOf 'A' |> should equal 0
        [<Fact>] member x.``Given 'B' should return 1 ``() = indexOf 'B' |> should equal 1
        [<Fact>] member x.``Given 'Z' should return 25``() = indexOf 'Z' |> should equal 25
