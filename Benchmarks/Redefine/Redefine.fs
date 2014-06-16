module Redefine

open System.IO
open System.Diagnostics
open Hopac
open Recall

// This test shows what is and what isn't being computed.

let showDigest = update {
  let! d = digest
  do printfn "  digest=%A" d
}

let constant x = logAs (sprintf "constant: %A" x) {
  do printfn " constant: %A" x
  return x
}

let stage1 = logAs "test" {
  let! x = constant 0 |> wait
  do printfn " test(1): %A" x
  do! showDigest
  return x
}

let stage2 = logAs "test" {
  let! x = constant 1 |> wait
  do printfn " test(2): %A" x
  do! showDigest
  return x
}

let stage3 = logAs "test" {
  let! x = constant 0 |> wait
  let! y = constant 2 |> wait
  do printfn " test(3): %A + %A" x y
  do! showDigest
  return x + y
}

let test (stage: LogAs<Logged<int>>) =
  printfn "\nInitial:"
  let res1 : int = run <| recall ".recall" {
    return! stage |> wait
  }
  printfn "Result: %d" res1

  printfn "\nRerun:"
  let res2 : int = run <| recall ".recall" {
    return! stage |> wait
  }
  printfn "Result: %d" res2

[<EntryPoint>]
let Start argv =
  if Directory.Exists ".recall" then
    Directory.Delete (".recall", true)
  [stage1; stage2; stage3]
  |> List.iter test
  0
