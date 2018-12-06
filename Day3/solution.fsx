open System
open System.IO
open System.Text.RegularExpressions

let loadData () =
    File.ReadAllLines(@"Day3/input")

type Claim = 
    {
        claimId : int        
        x : int
        y : int
        w : int
        h : int
    }

let parse (s: string) =
    let m = Regex.Match(s, "^#(\d*) @ (\d*),(\d*): (\d*)x(\d*)$")
    {
        claimId = int m.Groups.[1].Value
        x = int m.Groups.[2].Value
        y = int m.Groups.[3].Value
        w = int m.Groups.[4].Value
        h = int m.Groups.[5].Value
    }


let addClaim (c : Claim) (area : Claim list[,]) : Claim list[,] =
    for y in (c.y) .. (c.y + c.h - 1) do
        for x in (c.x) .. (c.x + c.w - 1) do
            area.[y,x] <- c :: area.[y,x]

    area    

let addClaims (area : Claim list[,]) (claims : Claim list) =
    claims
    |> List.map(fun c -> addClaim c area)
    |> List.last

let calculateMultiClaimSquares (area : Claim list[,]) =
    area
    |> Seq.cast<Claim list>
    |> Seq.filter (fun i-> i.Length > 1)
    |> Seq.length

let getClaimIds area = 
    area
    |> Seq.cast<Claim list>
    |> Seq.collect(fun c -> c |> List.map(fun c' -> c'.claimId))
    |> Seq.distinct
    |> Seq.toList

let getClaimIdsWithOvelap area =
    area
    |> Seq.cast<Claim list>
    |> Seq.filter (fun i-> i.Length > 1)
    |> Seq.collect(fun c -> c |> List.map(fun c' -> c'.claimId))
    |> Seq.distinct
    |> Seq.toList



let area = Array2D.create<Claim list> 1000 1000 []
let claims = loadData() |> Array.toList |> List.map parse

let areaWithClaims = addClaims area claims
areaWithClaims |> calculateMultiClaimSquares

let claimIds = areaWithClaims |> getClaimIds
// 96569

let allClaimIds = getClaimIds areaWithClaims 
let claimsWithOverlaps = getClaimIdsWithOvelap areaWithClaims 

allClaimIds |> List.except claimsWithOverlaps
