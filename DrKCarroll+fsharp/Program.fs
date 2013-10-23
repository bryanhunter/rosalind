open System
open System.IO
open System.Numerics

open BioInformaticsUtils
//open GRPH
//open GC
//open FIB
//open CONS
open SPLC

[<EntryPoint>]
let main argv = 
    let fasta = getFASTAfromFile argv.[0]
//    let fastaMap = Map.ofList fasta
//    fastaMap |> Map.iter(fun key value -> printfn "%s %s" key value)

//    let (label, maxGC) = GC.findMaxGC_Content fasta
//    printfn "%s%s%f" label Environment.NewLine maxGC

//    let result = overlapGraph fasta 3
//    result |> List.map (fun (edgeHead, edgeTail) -> printfn "%s %s" edgeHead edgeTail) |> ignore

//    let rabbits = howManyRabbits 31 4
//    printfn "Total Rabbits: %d" rabbits

//    let result = getConsensusString fasta
//    printConsensusStringAndProfileMatrixForRosalind result
//
//    writeConsensusStringAndProfileMatrixToFile result "C:\Users\kevinc\Desktop\Rosalind_CONS_Output.txt"

    let protein = splice fasta
    printfn "%s" protein

    Console.ReadKey() |> ignore
    0 // return an integer exit code
