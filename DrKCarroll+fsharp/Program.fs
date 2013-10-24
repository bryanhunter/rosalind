open System
open System.IO
open System.Numerics

open BioInformaticsUtils
open GRPH
//open GC
//open FIB
//open CONS
//open SPLC
//open FIBD
//open MPRT
open TRAN

[<EntryPoint>]
let main argv = 
//    let fasta = getFASTAfromFile argv.[0]
//    let fastaMap = Map.ofList fasta
//    fastaMap |> Map.iter(fun key value -> printfn "%s %s" key value)

//    let (label, maxGC) = GC.findMaxGC_Content fasta
//    printfn "%s%s%f" label Environment.NewLine maxGC

//    let result = overlapGraph fasta 3
//    result 
//    |> List.map (fun (edgeHead, edgeTail) -> printfn "%s %s" edgeHead edgeTail) 
//    |> ignore

//    let rabbits = howManyRabbits 31 4
//    printfn "Total Rabbits: %d" rabbits

//    let result = getConsensusString fasta
//    printConsensusStringAndProfileMatrixForRosalind result
//
//    writeConsensusStringAndProfileMatrixToFile result "C:\Users\kevinc\Desktop\Rosalind_CONS_Output.txt"

//    let protein = splice fasta
//    printfn "%s" protein

//    printfn "Living Rabbits: %d" (livingRabbits 6 3)

//    let ids = System.IO.File.ReadAllLines(argv.[0])
//    //let ids = [| "A2Z669"; "B5ZC00"; "P07204_TRBM_HUMAN"; "P20840_SAG1_YEAST" |]
//    let fastasLabeledWithIds = getFASTAsForIds ids 
//
//    fastasLabeledWithIds 
//    |> Array.map (fun fasta -> get_N_glycosylation_Locations2 fasta) 
//    |> Array.filter (fun result -> result.IsSome) 
//    |> Array.iter (fun result -> let (id, positions) = result.Value
//                                 printfn "%s" id
//                                 positions |> Seq.iter (fun position -> printf "%d " position)
//                                 printf "%s" System.Environment.NewLine)

    let fasta = getFASTAfromFile argv.[0]
    let s1 = snd (List.head fasta)
    let s2 = snd (List.head (List.tail fasta))
    let transitionTransversionRatio = getTransitionTransversionRatio s1 s2
    printfn "%f" transitionTransversionRatio

//    //printfn "%A" fasta

    Console.ReadKey() |> ignore
    0 // return an integer exit code
