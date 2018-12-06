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


let addClaim (c : Claim) (area : int[,]) : int[,] =
    for y in (c.y) .. (c.y + c.h - 1) do
        for x in (c.x) .. (c.x + c.w - 1) do
            area.[y,x] <- area.[y,x] + 1

    area    

let addClaims (area : int[,]) (claims : Claim list) =
    claims
    |> List.map(fun c -> addClaim c area)
    |> List.last

let calculateMultiClaimSquares (area : int[,]) =
    area
    |> Seq.cast<int>
    |> Seq.filter (fun i-> i > 1)
    |> Seq.length

let area = Array2D.zeroCreate<int> 8 8
let claims = [ "#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2" ] |> List.map parse

// let area = Array2D.zeroCreate<int> 1000 1000
// let claims = loadData() |> Array.toList |> List.map parse

let areaWithClaims = addClaims area claims
areaWithClaims |> calculateMultiClaimSquares

// 96569