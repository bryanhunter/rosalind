module GC

open BioInformaticsUtils

let GC_Content strand =
    let rec compute_GC strandAsList numerator denominator =
        match strandAsList with
        | head :: tail ->
            match head with
            | 'G'
            | 'C' -> compute_GC tail (numerator + 1.0) (denominator + 1.0)
            | _ -> compute_GC tail numerator (denominator + 1.0)
        | [] -> 100.0 * numerator / denominator

    compute_GC (asList strand) 0.0 0.0

let findMaxGC_Content (fasta:fastaList) =
    let rec findMaxSoFar (fasta:fastaList) (maxSoFar:string * float) = 
        let (maxSoFarLabel, maxSoFarValue) = maxSoFar
        match fasta with
        | (currentLabel, data) :: tail ->
            let currentGC_Content = GC_Content(data)
            findMaxSoFar tail (if currentGC_Content > maxSoFarValue then (currentLabel, currentGC_Content) else maxSoFar)
        | [] -> maxSoFar

    findMaxSoFar fasta ("", 0.0)



