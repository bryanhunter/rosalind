module FIB

let howManyRabbits n (k:int) =
    // Initialize an array to memoize previous calculations
    let F = [| for i in 1..n -> -1L |]
    F.[0] <- 1L
    F.[1] <- 1L

    let rec howManyRabbitsAux m (k:int) =

        let getRabbits j =
            if F.[j] = -1L then
                // Only compute if we don't already have it
                F.[j] <- howManyRabbitsAux j k 
            F.[j]

        match m with
        | 0 -> 1L
        | 1 -> 1L
        | 2 -> (int64 k) + 1L
        | _ -> (int64 k) * getRabbits (m - 2) + getRabbits (m - 1)

    let m = n - 1
    //howManyRabbitsAux (n - 1) k

    // below prints out each generation before returning total
    let totalRabbits = howManyRabbitsAux m k 
    F.[m] <- totalRabbits
    F |> Seq.iteri (fun i f -> printfn "Generation %d, %d" (i + 1) f)
    totalRabbits