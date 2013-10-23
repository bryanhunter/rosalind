module PROT

let RNA_to_Protein (rna : string) =
    let sb = new System.Text.StringBuilder()
    let rec RNA_to_ProteinAux (rna : string) =
        if rna.Length < 3 then
            rna
        else
            let substring = rna.Substring(0, 3)
            let protein =   match substring with
                            // RNA codon table
                            | "AAA" -> "K"  | "AAC" -> "N"  | "AAG" -> "K"  | "AAU" -> "N"
                            | "ACA" -> "T"  | "ACC" -> "T"  | "ACG" -> "T"  | "ACU" -> "T"
                            | "AGA" -> "R"  | "AGC" -> "S"  | "AGG" -> "R"  | "AGU" -> "S"
                            | "AUA" -> "I"  | "AUC" -> "I"  | "AUG" -> "M"  | "AUU" -> "I"
                            | "CAA" -> "Q"  | "CAC" -> "H"  | "CAG" -> "Q"  | "CAU" -> "H"
                            | "CCA" -> "P"  | "CCC" -> "P"  | "CCG" -> "P"  | "CCU" -> "P"
                            | "CGA" -> "R"  | "CGC" -> "R"  | "CGG" -> "R"  | "CGU" -> "R"
                            | "CUA" -> "L"  | "CUC" -> "L"  | "CUG" -> "L"  | "CUU" -> "L"
                            | "GAA" -> "E"  | "GAC" -> "D"  | "GAG" -> "E"  | "GAU" -> "D"
                            | "GCA" -> "A"  | "GCC" -> "A"  | "GCG" -> "A"  | "GCU" -> "A"
                            | "GGA" -> "G"  | "GGC" -> "G"  | "GGG" -> "G"  | "GGU" -> "G"
                            | "GUA" -> "V"  | "GUC" -> "V"  | "GUG" -> "V"  | "GUU" -> "V"
                            | "UAC" -> "Y"  | "UAU" -> "Y"  | "UCA" -> "S"  | "UCC" -> "S"
                            | "UCG" -> "S"  | "UCU" -> "S"  | "UGC" -> "C"  | "UGG" -> "W"
                            | "UGU" -> "C"  | "UUA" -> "L"  | "UUC" -> "F"  | "UUG" -> "L"
                            | "UUU" -> "F"  
                            | "UAA" -> "Stop"   | "UAG" -> "Stop"   | "UGA" -> "Stop"
                            | _ -> failwith (sprintf "Illegal RNA substring provided [%s]" substring)

            if protein = "Stop" then
                sb.ToString()
            else
                sb.Append(protein) |> ignore
                RNA_to_ProteinAux (rna.Substring(3))

    RNA_to_ProteinAux rna


