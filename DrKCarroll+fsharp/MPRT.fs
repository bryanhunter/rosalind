module MPRT

open System.IO
open System.Net.Http
open System.Text
open BioInformaticsUtils

let getFASTAforId (uniprot_id : string) = async {
    let url = "http://www.uniprot.org/uniprot/" + uniprot_id.Trim() + ".fasta"

    let client = new System.Net.Http.HttpClient()
    let! raw = client.GetStringAsync(url) |> Async.AwaitTask

    let lines = raw.Trim().Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
    let fasta = toFASTAfromLines lines
    return (uniprot_id, snd (List.head fasta))
    }

let getFASTAsForIds ids = 
    Async.Parallel [ for id in ids -> getFASTAforId id ] 
    |> Async.RunSynchronously

let get_N_glycosylation_Locations (fasta : (string * string)) =
    let N_glycosylation_motif_pattern = "N[^P][ST][^P]"
    let (id, strand) = fasta
    let matches = System.Text.RegularExpressions.Regex.Matches(strand, N_glycosylation_motif_pattern)
    if matches.Count = 0 then
        None
    else
        let positions = new System.Collections.Generic.List<int>()
        for m in matches do
            positions.Add(m.Index + 1)
        Some((id, positions))
            
let get_N_glycosylation_Locations2 (fasta : (string * string)) =
    let N_glycosylation_motif_pattern = "N[^P][ST][^P]"
    let (id, strand) = fasta

    let regex = new System.Text.RegularExpressions.Regex(N_glycosylation_motif_pattern);
    let positions = new System.Collections.Generic.List<int>()
    
    for i in 0..(strand.Length - 4) do
        let substring = strand.[i .. i+3]
        if regex.IsMatch(substring) then
            positions.Add(i + 1)

    if positions.Count > 0 then
        Some((id, positions))
    else
        None

