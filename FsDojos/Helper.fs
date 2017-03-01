namespace Helpers

[<AutoOpen>]
module Option =
    let ifSome value boolean = if boolean then Some value else None
    let (|?) = defaultArg

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

module Http =
    open System.Net.Http
    let get (url:string) = 
        use httpClient = new HttpClient()
        httpClient.GetAsync(url).Result.Content.ReadAsStringAsync().Result

[<AutoOpen>]
module Assert =
    open Xunit

    let ``should equal`` (expected:obj) (actual:obj) = 
        match (expected,actual) with 
        | (:? string as e), (:? string as a) -> Assert.Equal(e, a)
        | (:? int as e), (:? int as a) -> Assert.Equal(e, a)
        | (:? double as e), (:? double as a) -> Assert.Equal(e, a)
        | _,_ -> Assert.Equal(expected, actual)

    let ``should be less than`` max actual = Assert.True(actual < max)
    let ``should be more than`` min actual = Assert.True(actual > min)

    let ``should be true`` actual = Assert.Equal(true, actual)
    let ``should be false`` actual = Assert.Equal(false, actual)

    let ``should be empty`` actual = Assert.True(Seq.length actual = 0)
    let ``should contain`` expected actual = Assert.Contains(actual, (fun x -> x = expected))