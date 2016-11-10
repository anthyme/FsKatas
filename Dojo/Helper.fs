namespace Helpers

[<AutoOpen>]
module Option =
    let ifSome value boolean = if boolean then Some value else None

module Tuple =
    let map f (a,b) = (f a, f b)
    let map2 f1 f2 (a,b) = (f1 a, f2 b)
    let mapFst f (a,b) = (f a, b)
    let mapSnd f (a,b) = (a, f b)
    let toArgs f (a,b) = f a b
    let ofList (list:'t list) = (list.[0],list.[1])

module String =
    let split char (txt:string) = txt.Split([| char |])

module Int =
    let ofChar = (string >> int)

