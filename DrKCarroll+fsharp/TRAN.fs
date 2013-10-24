module TRAN

let bumpIfTransition s1 s2 =
    match s1, s2 with
    | 'A', 'G' -> 1
    | 'C', 'T' -> 1
    | 'G', 'A' -> 1
    | 'T', 'C' -> 1
    | _, _   -> 0

let bumpIfTransversion s1 s2 =
    match s1, s2 with
    | 'A', 'C' -> 1
    | 'A', 'T' -> 1
    | 'C', 'A' -> 1
    | 'C', 'G' -> 1
    | 'G', 'C' -> 1
    | 'G', 'T' -> 1
    | 'T', 'A' -> 1
    | 'T', 'G' -> 1
    | _, _   -> 0

let getTransitionTransversionRatio (s1:string) (s2:string) =
    let rec getTransitionTransversionRatioAux (s1:string) (s2:string) transitions transversions =
        match s1.Length with
        | 0 -> (float transitions) / (float transversions)
        | _ -> getTransitionTransversionRatioAux 
                    (s1.Substring(1)) 
                    (s2.Substring(1)) 
                    (transitions   + (bumpIfTransition   s1.[0] s2.[0])) 
                    (transversions + (bumpIfTransversion s1.[0] s2.[0]))
    getTransitionTransversionRatioAux s1 s2 0 0


