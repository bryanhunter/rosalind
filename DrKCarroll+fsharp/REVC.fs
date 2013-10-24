module REVC

open BioInformaticsUtils

let reverseComplement strand =
    let rec complementList list reversedComplementedList =
        match list with
        | head :: tail ->
            match head with
            | 'T' -> complementList tail ('A' :: reversedComplementedList)
            | 'A' -> complementList tail ('T' :: reversedComplementedList)
            | 'C' -> complementList tail ('G' :: reversedComplementedList)
            | 'G' -> complementList tail ('C' :: reversedComplementedList)
            | _ -> failwith (sprintf "Illegal symbol in dna string [%c]" head)
        | [] -> reversedComplementedList

    asString (complementList (asList strand) [])


