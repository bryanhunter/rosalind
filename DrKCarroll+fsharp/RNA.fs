module RNA

open BioInformaticsUtils

// TODO: make tail recursive
let DNA_to_RNA strand =
    let rec transcribe dnaAsList rnaAsList =
        match dnaAsList with
        | head :: tail ->
            match head with
            | 'T' -> 'U' :: transcribe tail rnaAsList
            | _ -> head :: transcribe tail rnaAsList
        | [] -> rnaAsList

    asString (transcribe (asList strand) [])


