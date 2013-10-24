module RNA

open BioInformaticsUtils

let DNA_to_RNA strand =
    let rec transcribe dnaAsList rnaAsList =
        match dnaAsList with
        | head :: tail ->
            match head with
            | 'T' -> 'U' :: transcribe tail rnaAsList
            | _ -> head :: transcribe tail rnaAsList
        | [] -> rnaAsList

    asString (transcribe (asList strand) [])

// This version is tail recursive, but requires the list to be reversed before returning
let DNA_to_RNA2 strand =
    let rec transcribe dnaAsList rnaAsList =
        match dnaAsList with
        | head :: tail ->
            match head with
            | 'T' -> transcribe tail ('U' :: rnaAsList)
            | _ -> transcribe tail (head :: rnaAsList)
        | [] -> rnaAsList |> List.rev

    asString (transcribe (asList strand) [])



