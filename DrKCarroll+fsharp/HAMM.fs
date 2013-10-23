module HAMM

let HammingDistance (s1:string) (s2:string) =
    let mutable distance = 0
    for i = 0 to s1.Length - 1 do
        if s1.[i] <> s2.[i] then
            distance <- distance + 1
    distance

/// Recursive version...
let rHammingDistance (s1:string) (s2:string) =
    let rec HammingDistanceAux (s1:string) (s2:string) distance =
        match s1.Length with
        | 0 -> distance
        | _ -> HammingDistanceAux (s1.Substring(1)) (s2.Substring(1)) (distance + if s1.[0] <> s2.[0] then 1 else 0)
    HammingDistanceAux s1 s2 0


