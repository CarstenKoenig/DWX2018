
let Münzen = [200; 100; 50; 20; 10; 5; 2; 1]

let wechsle betrag =
    if betrag < 0 then failwith "nice try"
    let rec go restMünzen restBetrag =
        if restBetrag = 0 then [] else
        match restMünzen with
        | (m::ms) ->
            if m <= restBetrag then
                m :: go restMünzen (restBetrag - m)
            else
                go ms restBetrag
        | [] -> failwith "math says impossible"
    go Münzen betrag

let wechsleFold betrag =
    let fold (restBetrag, ergs) münze =
        let n = restBetrag / münze
        let rest = restBetrag % münze
        let ms = List.replicate n münze
        (rest, ergs @ ms)
    List.fold fold (betrag, []) Münzen
    |> snd

let test = wechsleFold 1337
List.sum test