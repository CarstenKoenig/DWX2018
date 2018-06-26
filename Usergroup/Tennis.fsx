
type Unter40 =
    | Null
    | Fünfzehn
    | Dreißig
    override this.ToString() =
        match this with
        | Null -> "0"
        | Fünfzehn -> "15"
        | Dreißig -> "30"


type Spieler = 
    | A | B

let gegenSpieler = 
    function
    | A -> B
    | B -> A


type Spielstand =
    | BeideUnter40 of Unter40 * Unter40
    | Spieler40 of Spieler * Unter40
    | Einstand
    | Vorteil of Spieler
    | Gewonnen of Spieler
    override this.ToString() =
        match this with
        | BeideUnter40 (a,b) -> sprintf "%O:%O" a b
        | Spieler40 (A, gegner) -> sprintf "40:%O" gegner
        | Spieler40 (B, gegner) -> sprintf "%O:40" gegner
        | Einstand -> "Einstand"
        | Vorteil spieler -> sprintf "Vorteil %A" spieler
        | Gewonnen spieler -> sprintf "Spiel %A" spieler


let anfangsStand = 
    BeideUnter40 (Null, Null)


let aPunktet = 
    function
    | BeideUnter40 (Null, gegner) -> BeideUnter40 (Fünfzehn, gegner)
    | BeideUnter40 (Fünfzehn, gegner) -> BeideUnter40 (Dreißig, gegner)
    | BeideUnter40 (Dreißig, gegner) -> Spieler40 (A, gegner)
    | Spieler40 (A, _) -> Gewonnen A
    | Spieler40 (B, Null) -> Spieler40 (B, Dreißig)
    | Spieler40 (B, Fünfzehn) -> Spieler40 (B, Dreißig)
    | Spieler40 (B, Dreißig) -> Einstand
    | Einstand -> Vorteil A
    | Vorteil A -> Gewonnen A
    | Vorteil B -> Einstand
    | Gewonnen spieler -> failwith (sprintf "%A hat schon gewonnen" spieler)


let gegnersicht = 
    function
    | BeideUnter40 (a,b) -> BeideUnter40 (b,a)
    | Spieler40 (spieler, gegner) -> Spieler40 (gegenSpieler spieler, gegner)
    | Einstand -> Einstand
    | Vorteil spieler -> Vorteil (gegenSpieler spieler)
    | Gewonnen spieler -> Gewonnen (gegenSpieler spieler)
 

let bPunktet =
    gegnersicht >> aPunktet >> gegnersicht


let spiele punktGewinneDurch =
    let spielerPunktet spieler = 
        function
        | A -> aPunktet spieler
        | B -> bPunktet spieler
    punktGewinneDurch
    |> List.scan spielerPunktet anfangsStand


// Test
spiele [A;A;B;A;B;B;A;B;B;B]
|> List.iter (printfn "%O")