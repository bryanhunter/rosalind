module BioInformaticsUtils

// Converts a string to a char list
let asList (s:string) =
    [for c in s -> c]

// Converts a char list to a string
let asString (l : char list) =
    let sb = new System.Text.StringBuilder()
    let rec asStringInternal (l : char list) =
        match l with
        | head :: tail -> sb.Append(head) |> ignore
                          asStringInternal tail
        | [] -> ignore()

    asStringInternal l
    sb.ToString()

let toFASTAfromLines (lines: string array) =
    let sb = new System.Text.StringBuilder()
    let fasta = new System.Collections.Generic.List<string>()

    lines 
    |> Array.iter(fun line -> 
        if line <> "" then
            if line.[0] = '>' then
                if sb.Length > 0 then
                    fasta.Add(sb.ToString())
                    sb.Clear() |> ignore

                fasta.Add(line.Trim().Substring(1))
            else
                sb.Append(line.Trim()) |> ignore
        ) 

    if sb.Length > 0 then
        fasta.Add(sb.ToString())

    [for i in 0..((fasta.Count - 1) / 2) -> (fasta.[i * 2], fasta.[i * 2 + 1])]

// Returns a list of tuples containing the label-data pairs (e.g. [ ("Label1", "Data1"); ("Label2", "Data2") .... ] )
// Assumes a "correct" file format
let getFASTAfromFile (fileName:string) =
    toFASTAfromLines (System.IO.File.ReadAllLines(fileName))

let prefix (s:string) k =
    s.Substring(0, k)

let suffix (s:string) k =
    s.Substring(s.Length - k, k)






