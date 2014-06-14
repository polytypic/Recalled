module BigFib

// This benchmark mainly measures log read time.

open System.Diagnostics
open Hopac
open Recall

let rec fib n = logAs ("fib: " + n.ToString ()) {
  if n < 2I then
    return n
  else
    let! x = fib (n-1I)
    let! y = fib (n-2I)
    let! x = read x
    let! y = read y
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
