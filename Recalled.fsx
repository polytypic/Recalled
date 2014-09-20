#I __SOURCE_DIRECTORY__ ;;

#r "Libs/Recalled/bin/Release/Hopac.Core.dll" ;;
#r "Libs/Recalled/bin/Release/Hopac.dll" ;;
#r "Libs/Recalled/bin/Release/Infers.dll" ;;
#r "Libs/Recalled/bin/Release/Infers.Rep.dll" ;;
#r "Libs/Recalled/bin/Release/Recalled.Internal.dll" ;;
#r "Libs/Recalled/bin/Release/Recalled.dll" ;;

open System.Numerics ;;
open System.Security.Cryptography ;;
open System.IO ;;
open System ;;
open Infers ;;
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

let written (path: string) = log (sprintf "written %A" path) {
  return File.GetLastWriteTime path
}

let md5 (path: string) = log (sprintf "md5 %A" path) {
  do! dep (written path)
  let result =
    let md5 = MD5.Create ()
    use stream =
      new FileStream (path, FileMode.Open, FileAccess.Read)
    md5.ComputeHash stream
  return result
}

let allLines (path: string) = update {
  do! dep (md5 path)
  return File.ReadAllLines path
}

let md5s (listPath: string) = log (sprintf "md5s %A" listPath) {
  let! paths = allLines listPath
  let! md5Ls = paths |> Seq.mapLogged md5
  let! md5s = md5Ls |> Seq.mapUpdate read
  return md5s.ToArray ()
}



let sortedLines (path: string) = log ("sortedLines: " + path) {
  let! lines = allLines path
  return Array.sort lines
}

let sorted (xsR: Result<array<'x>>) = log (sprintf "sorted (%s)" (idOf xsR)) {
  do! req xsR
  do! watch 101
  let! xs = read xsR
  return Array.sort xs
}

let copy (src: string) (dst: string) =
  log (sprintf "copy %A %A" src dst) {
    if not (File.Exists dst) ||
       File.GetLastWriteTimeUtc dst <>
       File.GetLastWriteTimeUtc src
    then File.Copy (src, dst, true)
    return! written dst
  }

let sumLinesOfFile path = log ("sumLinesOfFile: " + path) {
  let! intLines = allLines path
  let sum =
    intLines
    |> Seq.sumBy (fun intLine -> int intLine)
  return sum
}

let sumLinesOfFiles (filesPath: string) = log ("sumLinesOfFiles: " + filesPath) {
  let! filePaths = allLines filesPath

  let! sums =
    filePaths
    |> Seq.mapLogged (sumLinesOfFile >> wait)

  let total = sums |> Seq.sum

  return total
}

let sumProgram () =
  let sum = run <| recall ".recall" {
    return! sumLinesOfFiles "foo" |> wait }
  printfn "Sum: %d" sum
  0

////////////////////////////////////////////////////////////////////////////////

let sumLinesOfFilesPar (filesPath: string) = log ("sumLinesOfFiles: " + filesPath) {
  let! filePaths = allLines filesPath

  let! loggedSums =
    filePaths
    |> Seq.mapLogged sumLinesOfFile

  let! sums =
    loggedSums
    |> Seq.mapUpdate read

  let total = sums |> Seq.sum

  return total
}

////////////////////////////////////////////////////////////////////////////////
#if FALSE
module Versioning1 =

  let sorted path =
    log (sprintf "sorted %A" path) {

      let! lines = allLines path
      return lines
             |> Array.sort

    }


  type TexInfo = {
      w: int
      h: int

      bpp: int
    }

  let texInfo path =
    log (sprintf "texInfo %A" path) {

      do! dep (md5 path)
      ...
      return {w = ...; h = ...; bpp = ...}
    }

module Versioning2 =
  let sorted path =
    log (sprintf "sorted %A" path) {
      do! watch 1
      let! lines = allLines path
      return lines
             |> Array.sort
             |> Array.distinct
    }


  type TexInfo = {
      w: int
      h: int
      hasAlpha: bool
      bpp: int
    }

  let texInfo path =
    log (sprintf "texInfo2 %A" path) {
      do! dep (md5 path)
      ...
      return {w = ...
              h = ...
              hasAlpha = ...
              bpp = ...}
    }




#endif

module Fib =
  let rec fib n =
    log ("fib " + n.ToString ()) {
      if n < 2I then
        return n
      else
        let! xL = fib (n-2I)
        let! yL = fib (n-1I)

        let! x = read xL
        let! y = read yL

        return x + y
    }

module FibTyped =
  let rec fib (n: BigInteger) : Logged<Result<BigInteger>> =
    log ("fib " + n.ToString ()) {
      if n < 2I then
        return n
      else
        let! (xL : Result<BigInteger>) = fib (n-1I)
        let! (yL : Result<BigInteger>) = fib (n-2I)

        let! (x : BigInteger) = read xL
        let! (y : BigInteger) = read yL

        return x + y
    }

