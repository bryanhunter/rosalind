module GRPH

open BioInformaticsUtils

let overlapGraph (fasta: fastaList) k =
    let rec getOverlappingEdges (potentialTo:(string * string * string * string)) (potentialFroms: (string * string * string * string) list) edges =
        match potentialFroms with
        | [] -> edges
        | (fromLabel, fromStrand, _, fromSuffix) :: otherPotentialFroms -> 
            let (toLabel, toStrand, toPrefix, _) = potentialTo
            getOverlappingEdges potentialTo otherPotentialFroms 
                (if fromStrand <> toStrand && fromSuffix = toPrefix then (fromLabel, toLabel) :: edges else edges)

    let potentialFroms = fasta |> List.map(fun (label, strand) -> (label, strand, (prefix strand k), (suffix strand k)))

    potentialFroms 
    |> List.map(fun potentialTo -> getOverlappingEdges potentialTo potentialFroms []) 
    |> List.concat



