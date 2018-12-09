open System
open System.IO
open System.Text.RegularExpressions


type LogEntry =
    {
        Date : DateTime
        State : GuardState
    }
and  GuardState =
    | BeginsShift of int
    | FallsAsleep
    | WakesUp
   
type PostState =
    | Unguarded
    | GuardAwake
    | GuardAsleep



let parse (s: string) =
    let parseState (s : string) =
        match s with
        | "falls asleep" -> FallsAsleep
        | "wakes up" -> WakesUp
        | _ ->
            let m = Regex.Match(s, "Guard #(\d*) begins shift")
            let guardId = int m.Groups.[1].Value
            BeginsShift guardId

    let m = Regex.Match(s, "^\[(\d\d\d\d-\d\d-\d\d\ \d\d:\d\d)\] (.*)$")
    {
        Date = DateTime.Parse(m.Groups.[1].Value)
        State = parseState m.Groups.[2].Value
    }

let loadData () =
    File.ReadAllLines(@"Day4/input")
    |> Array.toList
    |> List.map parse
    |> List.sortBy(fun i -> i.Date)

let getGroups<'a> (data : LogEntry list) =
    let rec group (data : LogEntry list) (state : LogEntry list) =
        seq {
            match data with
            | [] -> 
                yield state
            | [v] -> 
                match v.State with
                | BeginsShift _ -> 
                    yield state
                    yield [v]
                | _ ->                
                    yield (state @ [v])
            | head :: tail ->
                match head.State with
                | BeginsShift _ -> 
                    yield state
                    yield! group tail [head]
                | _ ->                
                    yield! group tail (state @ [head])
        }

    group (List.tail data) [(List.head data)] |> Seq.toList

let getGuardId s =
    match s with
    | BeginsShift id -> id
    | _ -> 0

let dateToDayStart (d : DateTime) =
    if d.Hour = 0 then d
    else d.Date.AddDays(1.)

let getPostState (logEntries : LogEntry list) (minute : int) =
    let date = (dateToDayStart logEntries.[0].Date).Date.AddMinutes(float minute)
    let le = logEntries |> List.tryFind(fun e -> dateToDayStart e.Date > date)
    match le with
    | None -> GuardAwake
    | Some le ->
        match le.State with
        | BeginsShift _ -> Unguarded
        | FallsAsleep -> GuardAwake
        | WakesUp -> GuardAsleep

let convertToCard (logEntries : LogEntry list) =
    let date = (logEntries.[0].Date |> dateToDayStart).ToString("MM-dd")
    [0..59]
    |> List.map(fun i -> getPostState logEntries i)


let countAsleepHours (le : LogEntry list list) =
    le
    |> List.map convertToCard
    |> List.concat
    |> List.filter (fun i -> i = GuardAsleep)
    |> List.length
    
let findIdOfheGuardSleepingMost data = 
    data
    |> getGroups
    |> List.groupBy(fun i -> getGuardId i.Head.State)
    |> List.map(fun (guardId, le) -> (guardId, countAsleepHours le))
    |> List.sortByDescending(fun (_, i) -> i)
    |> List.head
    |> fst

let findMinuteGuardIsMostAsleep guardId data =
    data
    |> getGroups
    |> List.groupBy(fun i -> getGuardId i.Head.State)
    |> List.find(fun (id, _) -> id = guardId)
    |> snd
    |> List.map convertToCard
    |> List.collect(fun i -> i |> List.mapi(fun idx i -> (idx, i)))
    |> List.groupBy fst
    |> List.map(fun (_, i) -> i |> List.map snd)
    |> List.map(fun l -> l |> List.filter(fun i-> i = GuardAsleep) |> List.length)
    |> List.mapi(fun idx i -> (idx, i))
    |> List.sortByDescending snd
    |> List.head
    |> fst


let answer1 =
    let data = loadData() 
    let guardId = data |> findIdOfheGuardSleepingMost
    let hour =  data |> findMinuteGuardIsMostAsleep guardId

    guardId * hour



let answer2 = 
    let data = loadData() 
    let guardId, minute, cnt =
        data
        |> getGroups
        |> List.groupBy(fun i -> getGuardId i.Head.State)
        |> List.map(fun (id, le) -> 
            let (minute, cnt) = 
                le
                |> List.map convertToCard
                |> List.collect(fun i -> i |> List.mapi(fun idx i -> (idx, i)))
                |> List.groupBy fst
                |> List.map(fun (_, i) -> i |> List.map snd)
                |> List.mapi(fun h l -> (h,l |> List.filter(fun i' -> i' = GuardAsleep) |> List.length))
                |> List.sortByDescending (fun (_, v) -> v)
                |> List.head
            (id, minute, cnt)
            )
        |> List.sortByDescending(fun (_, _, v) -> v)
        |> List.head
    guardId * minute

