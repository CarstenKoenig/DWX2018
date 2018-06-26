
type Unter40 =
    | Null
    | Fünfzehn
    | Dreißig
    override this.ToString() =
        match this with
        | Null -> "0"
        | Fünfzehn -> "15"
        | Dreißig -> "30"


type Spielstand =
    | BeideUnter40 of Unter40 * Unter40
    | Ich40 of Unter40
    | Gegner40 of Unter40
    | Einstand
    | VorteilIch
    | VorteilGegner
    | Gewonnen
    | Verloren
    override this.ToString() =
        match this with
        | BeideUnter40 (ich,gegner) -> sprintf "%O:%O"  ich gegner
        | Ich40 gegner -> sprintf "40:%O" gegner
        | Gegner40 ich -> sprintf "%O:40" ich
        | Einstand -> "Einstand"
        | VorteilIch -> "Vorteil A"
        | VorteilGegner -> "Vorteil B"
        | Gewonnen -> "Spiel A"
        | Verloren -> "Spiel B"


let anfangsStand = 
    BeideUnter40 (Null, Null)


let ichPunkte = function
    | BeideUnter40 (Null, gegner) -> BeideUnter40 (Fünfzehn, gegner)
    | BeideUnter40 (Fünfzehn, gegner) -> BeideUnter40 (Dreißig, gegner)
    | BeideUnter40 (Dreißig, gegner) -> Ich40 gegner
    | Ich40 _ -> Gewonnen
    | Gegner40 Null -> Gegner40 Fünfzehn
    | Gegner40 Fünfzehn -> Gegner40 Dreißig
    | Gegner40 Dreißig -> Einstand
    | Einstand -> VorteilIch
    | VorteilIch -> Gewonnen
    | VorteilGegner -> Einstand
    | Gewonnen -> failwith "ich habe schon gewonnen"
    | Verloren -> failwith "zu spät"


let gegnersicht = function
    | BeideUnter40 (a,b) -> BeideUnter40 (b,a)
    | Ich40 a -> Gegner40 a
    | Gegner40 a -> Ich40 a
    | Einstand -> Einstand
    | VorteilIch -> VorteilGegner
    | VorteilGegner -> VorteilIch
    | Gewonnen -> Verloren
    | Verloren -> Gewonnen


let gegnerPunktet =
    gegnersicht >> ichPunkte >> gegnersicht


type Spieler = A | B

let spiele punktGewinneDurch =
    let spielerPunktet stand = function
        | A -> ichPunkte stand
        | B -> gegnerPunktet stand
    punktGewinneDurch
    |> List.scan spielerPunktet anfangsStand


// Test
spiele [A;A;B;A;B;B;A;B;B;B]
|> List.iter (printfn "%O")