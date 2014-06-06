// Tip: Add --lib:<recall-root-directory> to your F# interactive options

#r "Libs\\Recall\\bin\\Release\\Hopac.Core.dll" ;;
#r "Libs\\Recall\\bin\\Release\\Hopac.dll" ;;
#r "Libs\\Recall\\bin\\Release\\Infers.dll" ;;
#r "Libs\\Recall\\bin\\Release\\Infers.Rep.dll" ;;
#r "Libs\\Recall\\bin\\Release\\PPrint.dll" ;;
#r "Libs\\Recall\\bin\\Release\\Recall.dll" ;;

open System.Security.Cryptography ;;
open System.IO ;;
open System ;;
open Hopac ;;
open Hopac.Infixes ;;
open Hopac.Job.Infixes ;;
open Hopac.Alt.Infixes ;;
open Hopac.Extensions ;;
open Recall ;;

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
    |> Seq.mapWithLog (sumLinesOfFile >> wait)

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
    |> Seq.mapWithLog sumLinesOfFile

  let! sums =
    loggedSums
    |> Seq.mapJob readAsJob

  let total = sums |> Seq.sum

  return total
}

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

let rec fib n = logAs (sprintf "fib: %d" n) {
  if n < 2L then
    return n
  else
    let! x = fib (n-2L)
    let! y = fib (n-1L) |> wait
    let! x = readAsJob x
    return x + y
}
