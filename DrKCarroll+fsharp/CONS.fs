module CONS

open BioInformaticsUtils

let mapSymbolToIndex s =
    match s with 
    | 'A' -> 0
    | 'C' -> 1
    | 'G' -> 2
    | 'T' -> 3
    | _ -> failwith (sprintf "Illegal symbol in dna string [%c]" s)

let mapIndexToSymbol i =
    match i with 
    | 0 -> 'A'
    | 1 -> 'C'
    | 2 -> 'G'
    | 3 -> 'T'
    | _ -> failwith (sprintf "Illegal index provided [%d]" i)

let getConsensusString (fasta:(string * string) list) =
    // Assume each dna string in fasta is of the same length
    let n = (snd (List.head fasta)).Length
    
    let profileMatrix = Array2D.create 4 n 0

    fasta 
    |> List.iter(fun (label, dna) -> 
        dna 
        |> asList 
        |> List.iteri(fun i s -> profileMatrix.[mapSymbolToIndex s, i] <- profileMatrix.[mapSymbolToIndex s, i] + 1))

    let sb = new System.Text.StringBuilder()
    for j in 0..(n - 1) do
        let mutable max = -1
        let mutable row = -1
        for i in 0..3 do
            if profileMatrix.[i, j] > max then 
                max <- profileMatrix.[i, j]
                row <- i
        sb.Append(mapIndexToSymbol row) |> ignore

    (sb.ToString(), profileMatrix)

let printConsensusStringAndProfileMatrixForRosalind ((consensusString:string), (profileMatrix:int[,])) =
    let n = consensusString.Length
    printfn "%s" consensusString
    for i in 0..3 do
        let sb = new System.Text.StringBuilder()
        sb.AppendFormat("{0}: ", (mapIndexToSymbol i)) |> ignore
        for j in 0..(n - 1) do
            sb.Append(profileMatrix.[i, j]).Append(' ') |> ignore
        printfn "%s" (sb.ToString())
            
let writeConsensusStringAndProfileMatrixToFile ((consensusString:string), (profileMatrix:int[,])) fileName =
    let n = consensusString.Length
    let sb = new System.Text.StringBuilder()
    sb.AppendLine(consensusString) |> ignore
    for i in 0..3 do
        sb.AppendFormat("{0}: ", (mapIndexToSymbol i)) |> ignore
        for j in 0..(n - 1) do
            sb.Append(profileMatrix.[i, j]).Append(' ') |> ignore
        sb.AppendLine() |> ignore
    
    System.IO.File.WriteAllText(fileName, sb.ToString())


