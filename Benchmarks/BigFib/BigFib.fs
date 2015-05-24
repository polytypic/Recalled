module BigFib

// This benchmark measures log read time and the time it takes to construct
// logged computations.  These are both important, because both of those are
// always done.

open System
open System.Numerics
open System.Diagnostics
open Hopac
open Recalled

let rec fib n : Logged<Result<BigInteger>> = log ("fib: " + n.ToString ()) {
  if n < 2I then
    // For small `n` this code is always run, because there are no dependencies
    // to other computations.
    return n
  else
    // For larger `n` there are dependencies.
    let! xR = fib (n-1I)
    let! yR = fib (n-2I)
    // At this point dependencies will be checked and the remainder of the
    // computation is skipped unless dependencies have changed.
    let! x = read xR
    let! y = read yR
    return x + y
}

[<EntryPoint>]
let main argv =
  try
    let mutable n = 1I
    for i=1 to 16 do
      printf "fib %s" (n.ToString ())
      let timer = Stopwatch.StartNew ()
      let result =
        let n = n
        run <| recall ".recall" {
          return! fib n |> wait
        }
      let elapsed = timer.Elapsed
      printf " = %s\nTook %fs\n\n" (result.ToString ()) elapsed.TotalSeconds
      n <- n * 2I
    0
  with e ->
    printfn "%s: %s" (e.ToString ()) e.StackTrace
    1
