module Castle

[<AutoOpen>]
module Models =
    type Jauge = int

    type Cité = {
        Or : Jauge
        Blé : Jauge
        Santé : Jauge
        PopulationCivile : Jauge
        Armée : Jauge
    } with 
        static member vide =     { Or = 0 ; Blé = 0 ; Santé = 0 ; PopulationCivile = 0 ; Armée = 0 }
        static member nouvelle = { Or = 5 ; Blé = 5 ; Santé = 5 ; PopulationCivile = 5 ; Armée = 5 }

    type Dé = Récolte | Commerce | Peste | Grêle | Médicaments

    type EtatCité = CitéVivante of Cité | CitéMorte

[<AutoOpen>]
module Helpers =
    let maxJauge = 10
    let minJauge = 0

    let estMaximum jauge = jauge = maxJauge
    let ajouter1 jauge = if jauge < maxJauge then jauge + 1 else jauge
    let enlever1 jauge = if jauge > minJauge then jauge - 1 else jauge
    let enlever2 jauge = if jauge > minJauge + 1 then jauge - 2 else enlever1 jauge

    let toutPareil = id

    let private tirer1DéDe6 =
        let random = System.Random()
        fun () -> random.Next(1, 6)

    let tirer1Dé () = 
        match tirer1DéDe6() with
        | 1 | 2 -> Peste 
        | 3 -> Récolte 
        | 4 -> Commerce 
        | 5 -> Grêle 
        | 6 -> Médicaments

[<AutoOpen>]
module Logic =
    let evolue (cité: Cité) = 
        match tirer1Dé() with
        | Récolte ->     { cité with Blé    = ajouter1 cité.Blé }
        | Commerce ->    { cité with Or     = ajouter1 cité.Or }
        | Peste ->       { cité with Santé  = enlever1 cité.Santé }
        | Grêle ->       { cité with Blé    = enlever1 cité.Blé }
        | Médicaments -> { cité with Santé  = ajouter1 cité.Santé }
        |> CitéVivante

    let enroler1armée (cité: Cité) =
        if cité.Armée < 10 && cité.Or > 0 && cité.PopulationCivile > 0
        then { cité with Or = enlever1 cité.Or
                         PopulationCivile = enlever1 cité.PopulationCivile
                         Armée = ajouter1 cité.Armée }
        else cité

    let effetSanté = function
        | santé when santé < 2 -> enlever2
        | santé when santé < 4 -> enlever1
        | santé when santé > 8 -> ajouter1
        | _ -> toutPareil
    
    let effetBlé = function
        | blé when blé < 1 -> enlever2
        | blé when blé < 3 -> enlever1
        | blé when blé > 8 -> ajouter1
        | _ -> toutPareil

    let appliquerEffets cité = 
        { cité with PopulationCivile = cité.PopulationCivile |> effetSanté cité.Santé
                    Santé = cité.Santé |> effetBlé cité.Blé }
        |> CitéVivante

    let citéTombeOuPas cité =
        if cité.PopulationCivile = minJauge
            || estMaximum cité.PopulationCivile 
            || estMaximum cité.Armée 
        then CitéVivante cité
        else CitéMorte

let (>=>) switch1 switch2 x = 
    match switch1 x with
    | CitéVivante x -> switch2 x
    | CitéMorte -> CitéMorte

let jouer1Tour =
    evolue 
    >=> appliquerEffets 
    >=> citéTombeOuPas
