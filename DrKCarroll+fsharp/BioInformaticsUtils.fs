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
        if not (System.String.IsNullOrWhiteSpace(line)) then
            if line.[0] = '>' then
                if sb.Length > 0 then
                    // We have been collecting the previous entry's data. 
                    // Add it (index in the fasta List should be odd)
                    fasta.Add(sb.ToString())
                    sb.Clear() |> ignore

                // Add label (w/o '>') (index in the fasta List should be even)
                fasta.Add(line.Trim().Substring(1))
            else
                // This is data
                sb.Append(line.Trim()) |> ignore
        ) 

    // Add last data entry
    if sb.Length > 0 then
        fasta.Add(sb.ToString())

    [for i in 1..(fasta.Count / 2) -> (fasta.[(2 * i) - 2], fasta.[(2 * i) - 1])]

// Returns a list of tuples containing the label-data pairs (e.g. [ ("Label1", "Data1"); ("Label2", "Data2") .... ] )
// Assumes a "correct" file format
let getFASTAfromFile (fileName:string) =
    toFASTAfromLines (System.IO.File.ReadAllLines(fileName))

let prefix (s:string) k =
    s.Substring(0, k)

let suffix (s:string) k =
    s.Substring(s.Length - k, k)






