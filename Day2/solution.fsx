open System
open System.IO

let loadData () =
    File.ReadAllLines(@"Day2/input")

let countLetterIds lettersCount data =
    let containsMultiLetterId (s : string) =
        s
        |> Seq.groupBy(id)
        |> Seq.exists(fun (_,v) -> v |> Seq.length = lettersCount)


    data
    |> Array.filter containsMultiLetterId
    |> Array.length

let calculateChecksum data =
    let twoLetterIds = data |> countLetterIds 2
    let threeLetterIds = data |> countLetterIds 3

    twoLetterIds * threeLetterIds


let findCorrectBoxes data =
    let calculateDistance s1 s2 =
        Seq.zip s1 s2
        |> Seq.filter(fun (a,b) -> a <> b)
        |> Seq.length

    let rec find (s : string) (other : string list) = seq {
        match other with
        | [] -> yield ("","")
        | [_] -> yield ("","")
        | [a;b] -> if calculateDistance a b = 1 then yield (a,b) 
        | head::tail -> 
            let v =
                tail
                |> List.filter(fun i -> calculateDistance head i = 1)
                |> List.map(fun i -> (head, i))
            
            for n in v do yield n

            yield! find head tail
        }

    let  d' = data |> Array.toList |> List.skip 1
    let (a,b) = find data.[0] d' |> Seq.head

    Seq.zip a b
    |> Seq.filter(fun(a,b) -> a = b)
    |> Seq.map (fun (a,_) -> a)
    |> String.Concat



let data = loadData()
data |> calculateChecksum
data |> findCorrectBoxes
