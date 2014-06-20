module Redefine

open System.IO
open System.Diagnostics
open Hopac
open Recalled

// This test shows what is and what isn't being computed.  The idea is to test a
// sequence of changes to a computation as if the computations were evolved as
// part of modifying an existing program using logged computations.

let showDigest = update {
  let! d = digest
  do printfn "  digest=%A" d
}

/// Helper for creating simple primitive constants.  Another alternative
/// would be to use the `watch` primitive.
let constant x = logAs (sprintf "constant: %A" x) {
  do printfn " constant: %A" x
  return x
}

/// The first version of the logged computation.
let stage1 = logAs "test" {
  let! x = constant 0 |> wait
  do printfn " test(1): %A" x
  do! showDigest
  return x
}

/// The second modified version of the logged computation.
let stage2 = logAs "test" {
  let! x = constant 1 |> wait
  do printfn " test(2): %A" x
  do! showDigest
  return x
}

/// The third modified version of the logged computation.
let stage3 = logAs "test" {
  let! y = constant 2 |> wait
  let! x = constant 0 |> wait
  do printfn " test(3): %A + %A" x y
  do! showDigest
  return x + y
}

/// This runs the given stage twice.  On initial run we expect the computation
/// to be run to completion as it is either new or modified.  On the rerun we
/// expect that computations are skipped after checking dependencies.
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
