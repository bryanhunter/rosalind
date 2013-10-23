module DNA

open BioInformaticsUtils

let countDNAsymbols (inputString : string) =
    let rec countSymbolsAux (chars : char list, A : int, C : int, G : int, T : int) =
        match chars with
        | head :: tail ->
            match head with
            | 'A' -> countSymbolsAux (tail, A + 1, C, G, T)
            | 'C' -> countSymbolsAux (tail, A, C + 1, G, T)
            | 'G' -> countSymbolsAux (tail, A, C, G + 1, T)
            | 'T' -> countSymbolsAux (tail, A, C, G, T + 1)
            | _   -> countSymbolsAux (tail, A, C, G, T)
        | [] -> sprintf "%d %d %d %d" A C G T

    countSymbolsAux (asList inputString, 0, 0, 0, 0)


