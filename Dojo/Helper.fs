namespace Helpers

[<AutoOpen>]
module Option =
    let ifSome value boolean = if boolean then Some value else None

module Tuple =
    let map f (a,b) = (f a, f b)
    let mapFst f (a,b) = (f a, b)
    let mapSnd f (a,b) = (a, f b)