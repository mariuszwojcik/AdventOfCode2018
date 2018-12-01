open System
open System.IO
open System.Drawing

let loadInput() =
    let f = System.IO.File.ReadAllLines(@"Day1/input1")
    f
    |> Array.map int

let repeat items = 
    seq { while true do yield! items }
let answer1 = 
    loadInput()
    |> Array.sum

let answer2 =
    loadInput()
    |> repeat
    |> Seq.scan(fun (freq, all, dupe) elem -> 
        let newFreq = freq + elem
        (newFreq, Set.add newFreq all, Set.contains newFreq all)
        ) (0, Set.empty<int>, false)
    |> Seq.skipWhile (fun (_,_,dupe) -> not dupe)
    |> Seq.take 1



