module SPLC

open RNA
open PROT

let splice (fasta: (string * string) list) =
    let (label, dna) = List.head fasta
    let rec removeSubstrings (dna:string) (substrings: (string * string) list) =
        match substrings with
        | (label, substring) :: tail -> removeSubstrings (dna.Replace(substring, "")) tail
        | [] -> dna
        | _ -> failwith "huh???"

    let dnaWithoutIntrons = removeSubstrings dna (List.tail fasta)
    let rna = DNA_to_RNA dnaWithoutIntrons
    RNA_to_Protein rna

