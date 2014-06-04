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

let lastWriteTimeUtc (path: string) =
  logAs ("lastWriteTimeUtc: " + path) {
    return File.GetLastWriteTimeUtc path
  }

let md5 path =
  logAs ("md5: " + path) {
    let! _ = lastWriteTimeUtc path
    return MD5.ofFile path
  }

type FileInfo = {LastWriteTimeUtc: DateTime; MD5: array<byte>}

let fileInfo (path: string) =
  logAs ("fileInfo: " + path) {
    let! ticks = lastWriteTimeUtc path
    return {LastWriteTimeUtc = ticks; MD5 = MD5.ofFile path}
  }

////////////////////////////////////////////////////////////////////////////////

let readAllLines path =
  update {
    let! _ = md5 path
    return File.ReadAllLines path
  }

let hashes fileListPath hashFilePath =
  logAs (sprintf "hashes: %s -> %s" fileListPath hashFilePath) {
    let! fileList = readAllLines fileListPath

    let! md5s = fileList |> Seq.Par.mapLogged md5

    do use outputStream =
         new FileStream (hashFilePath, FileMode.Create, FileAccess.Write)
       use outputWriter = new StreamWriter (outputStream)
       for md5 in md5s do
         outputWriter.WriteLine (MD5.toString md5)

    return! digest
  }

////////////////////////////////////////////////////////////////////////////////

let copy (source: string) (target: string) =
  logAs (sprintf "copy: %s -> %s" source target) {
    let! sourceInfo = fileInfo source
    let! () = log {
      do if not (File.Exists target) ||
           File.GetLastAccessTimeUtc target <> sourceInfo.LastWriteTimeUtc then
           File.Copy (source, target, true)
      return ()
    }
    return! digest
  }

let copyFiles =
  recall ".recall" {
    let! _ = copy "foo" "bar"
    return ()
  }

////////////////////////////////////////////////////////////////////////////////
