module GRPH

open BioInformaticsUtils

// NOT WORKING YET (according to Rosalind)
let overlapGraph (fasta: (string * string) list) k =
    let parts = fasta |> List.map(fun (label, strand) -> (label, strand, (prefix strand k), (suffix strand k)))
    let rec getOverlappingEdges (dna:(string * string * string * string)) (dnaList: (string * string * string * string) list) edges =
        match dnaList with
        | [] -> edges
        | (label, strand, _, suffix) :: tail -> 
            let (dnaLabel, dnaStrand, dnaPrefix, _) = dna
            if strand = dnaStrand || dnaPrefix <> suffix then
                getOverlappingEdges dna tail edges
            else
                (label, dnaLabel) :: edges
    
    parts |> List.map(fun dna -> getOverlappingEdges dna parts []) |> List.concat



