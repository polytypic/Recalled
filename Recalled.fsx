#I __SOURCE_DIRECTORY__ ;;

#r "Libs/Recalled/bin/Release/Hopac.Core.dll" ;;
#r "Libs/Recalled/bin/Release/Hopac.dll" ;;
#r "Libs/Recalled/bin/Release/Infers.dll" ;;
#r "Libs/Recalled/bin/Release/Infers.Rep.dll" ;;
#r "Libs/Recalled/bin/Release/Recalled.Internal.dll" ;;
#r "Libs/Recalled/bin/Release/Recalled.dll" ;;

open System.Security.Cryptography ;;
open System.IO ;;
open System ;;
open Hopac ;;
open Recalled ;;

////////////////////////////////////////////////////////////////////////////////

module MD5 =
  open System.Security.Cryptography
  open System.IO

  let ofStream (stream: Stream) =
    let md5 = MD5.Create ()
    md5.ComputeHash stream

  let ofFile (path: string) =
    use s = new FileStream (path, FileMode.Open, FileAccess.Read)
    ofStream s

  let toString (md5: array<byte>) =
    (Guid md5).ToString ()

////////////////////////////////////////////////////////////////////////////////

let lastWriteTimeUtc (path: string) = logAs ("lastWriteTimeUtc: " + path) {
  return File.GetLastWriteTimeUtc path
}

let md5 (path: string) = logAs ("md5: " + path) {
  let! _ = lastWriteTimeUtc path
  let md5 = MD5.ofFile path
  return md5
}

let readAllLines path = update {
  let! _ = lastWriteTimeUtc path
  return File.ReadAllLines path
}

let sumLinesOfFile path = logAs ("sumLinesOfFile: " + path) {
  let! intLines = readAllLines path
  let sum =
    intLines
    |> Seq.sumBy (fun intLine -> int intLine)
  return sum
}

let sumLinesOfFiles (filesPath: string) = logAs ("sumLinesOfFiles: " + filesPath) {
  let! filePaths = readAllLines filesPath

  let! sums =
    filePaths
    |> Seq.mapLogAs (sumLinesOfFile >> wait)

  let total = sums |> Seq.sum

  return total
}

let sumProgram () =
  let sum = run <| recall ".recall" {
    return! sumLinesOfFiles "foo" |> wait }
  printfn "Sum: %d" sum
  0

////////////////////////////////////////////////////////////////////////////////

let sumLinesOfFilesPar (filesPath: string) = logAs ("sumLinesOfFiles: " + filesPath) {
  let! filePaths = readAllLines filesPath

  let! loggedSums =
    filePaths
    |> Seq.mapLogAs sumLinesOfFile

  let! sums =
    loggedSums
    |> Seq.mapUpdate read

  let total = sums |> Seq.sum

  return total
}

////////////////////////////////////////////////////////////////////////////////
(*
let copy (source: string) (target: string) =
  logAs (sprintf "copy: %s -> %s" source target) {
    let! sourceInfo = lastWriteTimeUtc source |> wait
    let! _ = log {
      return if not (File.Exists target) ||
               File.GetLastAccessTimeUtc target <> sourceInfo then
               File.Copy (source, target, true)
    }
    return! digest
  }

let copyFiles = recall ".recall" {
  return! copy "foo" "bar"
}
*)
////////////////////////////////////////////////////////////////////////////////

let rec fib n = logAs (sprintf "fib: %d" n) {
  if n < 2L then
    return n
  else
    let! x = fib (n-2L)
    let! y = fib (n-1L)
    let! x = read x
    let! y = read y
    return x + y
}
