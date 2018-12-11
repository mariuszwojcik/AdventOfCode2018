open System
open System.IO
open System.Text.RegularExpressions

let loadData () =
    File.ReadAllText(@"Day5/input")

(*
dabAcCaCBAcCcaDA

dabAcCaCBAcCcaDA  The first 'cC' is removed.
dabAaCBAcCcaDA    This creates 'Aa', which is removed.
dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
dabCBAcaDA        No further actions can be taken.
dabCBAcaDA
*)

let triggerReaction (polymer : string) =
    polymer
    |> Seq.fold(fun a c1 -> 
        let r = 
            if a = "" then
                c1.ToString()
            else
                let c2 = a |> Seq.last
                if Char.ToUpper(c1) = Char.ToUpper(c2) && c1 <> c2 then
                    a.Substring(0, a.Length - 1)
                else
                    sprintf "%s%O" a c1
        r            
        ) ""

let answer1 =
    loadData() |> triggerReaction

let answer2 =
    let data = loadData()

    ['a'..'z']
    |> List.map(fun letter -> (letter, Regex.Replace(data, letter.ToString(), "", RegexOptions.IgnoreCase)))
    |> List.map(fun (letter, polymer) -> (letter, polymer |> triggerReaction))
    |> List.minBy(fun (_,p) -> p.Length)
    |> snd
    |> String.length