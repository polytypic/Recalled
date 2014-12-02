module Tutorial

open System
open System.IO
open Recalled
open Hopac

let lastWriteTimeUtc (path: string) : Logged<Result<DateTime>> =
  log ("lastWriteTimeUtc: " + path) {
    return File.GetLastWriteTimeUtc path
  }

let readAllLines (path: string) : Update<array<string>> = update {
  let! _ = lastWriteTimeUtc path
  return File.ReadAllLines path
}

let sumLinesOfFile (path: string) : Logged<Result<int>> =
  log ("sumLinesOfFile: " + path) {
    let! intLines = readAllLines path
    let sum =
      intLines
      |> Array.sumBy (fun intLine -> int intLine)
    return sum
  }

let sumLinesOfFiles (fileListPath: string) : Logged<Result<int>> =
  log ("sumLinesOfFiles: " + fileListPath) {
    let! fileList = readAllLines fileListPath

    let! sumResults =
      fileList
      |> Seq.mapLogged sumLinesOfFile

    let! sums =
      sumResults
      |> Seq.mapUpdate read

    let total =
      sums
      |> Seq.sum

    return total
  }

[<EntryPoint>]
let main argv = 
  let sum : int = run <| recall ".recall" {
    return! sumLinesOfFiles "foo" |> wait
  }
  printfn "%A" sum
  0
