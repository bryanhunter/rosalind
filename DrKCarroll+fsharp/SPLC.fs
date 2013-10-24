module SPLC

open BioInformaticsUtils
open RNA
open PROT

let splice (fasta:fastaList) =
    let (label, dna) = List.head fasta
    let substrings = List.tail fasta
    let rec removeSubstrings (dna:string) (substrings:fastaList) =
        match substrings with
        | (label, substring) :: tail -> removeSubstrings (dna.Replace(substring, "")) tail
        | [] -> dna

    let dnaWithoutIntrons = removeSubstrings dna substrings

    dnaWithoutIntrons
    |> DNA_to_RNA
    |> RNA_to_Protein
