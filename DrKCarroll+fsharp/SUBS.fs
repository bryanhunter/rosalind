module SUBS

let findSubstringPositions (s:string) (t:string) =
    let mutable start = 0
    let results = new System.Collections.Generic.List<int>()
    while start >= 0 do
        let position = s.IndexOf(t, start)
        if position >= 0 then
            results.Add(position + 1)
            start <- position + 1
        else
            start <- -1

    let sb = new System.Text.StringBuilder()
    results.ForEach(fun p -> sb.Append(p.ToString()).Append(' ') |> ignore)
    sb.ToString()


