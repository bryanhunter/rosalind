module PERM

let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ when n > 1 -> n * (factorial (n - 1))
    | _ -> failwith (sprintf "Factorial function must be applied to a non-negative integer. [%d] supplied" n)

let rec getPermutations n =
    match n with
    | 0 -> []
    | 1 -> [ [1] ]
    | _ when n > 1 ->
        (getPermutations (n - 1)) 
        |> List.map (fun perm -> 
            let root = (n :: perm)
            [for j in 0..(n - 1) -> (List.permute (fun index -> (j + index) % n) root)]) 
        |> List.concat
    | _ -> failwith (sprintf "N must be >= 0, [%d] provided" n)

let printPermutationsForRosalind n =
    let perms = getPermutations n
    printfn "%d" (factorial n)        
    perms |> List.map (fun perm -> let sb = new System.Text.StringBuilder()
                                   perm |> List.map (fun i -> sb.Append(i).Append(" ")) |> ignore
                                   printfn "%s" (sb.ToString()))


