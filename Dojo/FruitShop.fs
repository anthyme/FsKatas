module FruitShop

type Product = Apples|Pommes|Mele|Bananes|Cerises 

let priceOf = function Apples|Pommes|Mele -> 100 |Bananes -> 150 |Cerises -> 75

let parseProduct = function |"Apples" -> Apples |"Mele" -> Mele |"Pommes" -> Pommes |"Bananes" -> Bananes |"Cerises" -> Cerises

let applyReduction (products:Product list) total = 
    let count product = products |> Seq.filter ((=) product) |> Seq.length
    let applyReduction total reduction = total + reduction()
    let cherries()  = -20 * (count Cerises / 2)
    let bananas()   = -priceOf Bananes * (count Bananes / 2)
    let apples()    = -(3 * priceOf Apples - 200) * (count Apples / 3)
    let mele()      = -100 * (count Mele / 2)
    let allApples() = -100 * ((count Mele + count Apples + count Pommes) / 4)
    let all()       = -200 * (products.Length / 5)
    [cherries; bananas; apples; mele; allApples; all] |> List.fold applyReduction total

let computeResult products = products, products |> Seq.map priceOf |> Seq.sum |> applyReduction products

let checkout basket (command:string) = command.Split(',') |> Seq.map parseProduct |> Seq.append basket |> Seq.toList |> computeResult









module Tests = 
    open Xunit
    open FsUnit.Xunit

    type ``Given checkout``() =
        let emptyBasket = []

        let check input expected basket = 
            let newBasket, result = checkout basket input
            expected |> should equal result
            newBasket

        [<Fact>]
        let ``when adding products price apply step 6 reductions``() =
            emptyBasket
            |> check "Mele,Apples,Apples,Pommes,Mele" 100
            |> check "Bananes" 250

        [<Fact>]
        let ``when adding products price apply step 6 reductions for more fruits``() = 
            emptyBasket
            |> check "Mele,Apples,Apples,Mele" 200
            |> check "Bananes" 150
            |> check "Mele,Apples,Apples,Pommes,Mele" 150
