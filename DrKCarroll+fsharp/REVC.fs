module REVC

open BioInformaticsUtils

// TODO: make tail-recursive

let reverseComplement strand =
    let rec complementList list complementedList =
        match list with
        | head :: tail ->
            match head with
            | 'T' -> 'A' :: complementList tail complementedList
            | 'A' -> 'T' :: complementList tail complementedList
            | 'C' -> 'G' :: complementList tail complementedList
            | 'G' -> 'C' :: complementList tail complementedList
            | _ -> head :: complementList tail complementedList
        | [] -> complementedList

    asString (List.rev (complementList (asList strand) []))



