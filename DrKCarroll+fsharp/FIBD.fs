module FIBD

// FAR FROM WORKING...

let howManyLivingRabbitsWithAllGenerations n m =
    // Initialize an array to memoize previous calculations
    let F = [| for i in 1..n -> -1L |]
    F.[0] <- 1L
    F.[1] <- 1L

    let rec howManyLivingRabbitsAux n =

        let getRabbits j =
            if j < 0 then 
                0L
            else
                if F.[j] = -1L then
                    // Only compute if we don't already have it
                    F.[j] <- howManyLivingRabbitsAux j 
                F.[j]

        match n with
        | 0 -> 1L
        | 1 -> 1L
        | _ -> getRabbits (n - 2) + getRabbits (n - 1) - getRabbits (n - m - 1)


    let m = n - 1
    //howManyRabbitsAux (n - 1) k

    // below prints out each generation before returning total
    let totalRabbits = howManyLivingRabbitsAux m 
    F.[m] <- totalRabbits
    F |> Seq.iteri (fun i f -> printfn "Generation %d, %d" (i + 1) f)
    (totalRabbits, F)

let howManyLivingRabbits n m =
    fst (howManyLivingRabbitsWithAllGenerations n m)
