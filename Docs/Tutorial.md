# Recall or How to Define Computations that can be Recalled

Recall is a library, an EDSL of a sort, that can be used to define persistent,
incremental, parallel computations such as build systems.  Recall isn't really a
ready made traditional build system as such.  The core of Recall is very minimal
and doesn't contain built-in functionality, like operations for checking file
modification dates, characteristics to build systems.  However, such operations
can be implemented in a straightforward manner and, in particular, *forward
build systems*, that go from inputs to outputs or sources to targets, can be
more or less directly programmed using Recall.

Let's examine how Recall can be used via a concrete toy example, meant to be
simple enough to be easily worked through without having to spend much time with
any kind of problem domain specific details

Suppose that you have a file containing a list of files with one path per line.
Each file listed in that file contains a list of integers stored with one
integer per line.  You wish to write a program that takes the name of the file
containing the list of files and computes the total sum of all the integers in
the files.

Here is a straightforward function for that purpose:

```fsharp
let sumLinesOfFiles (fileListPath: string) : int =
  let fileList = File.ReadAllLines fileListPath
  let sums =
    fileList
    |> Array.map (fun intListPath ->
       let intList = File.ReadAllLines intListPath
       intList
       |> Array.sumBy (fun intText -> int intText))
  let total =
    sums
    |> Array.sum
  total
```

There is nothing special about the above `sumLinesOfFiles` function.

The idea is that you are using this function to compute the sum whenever you
need it&mdash;without necessarily even knowing whether any file has actually
changed.  Wouldn't it be nice if your program could be modified to avoid
needless recomputation.  For example, if just one file changes or a new file is
added, it would nice if only the sum for that would be (re)computed and the
total be computed from previously known sums and so on.

As hinted in the beginning, Recall isn't really a ready made build tool, but it
allows one to straightforwardly program build tool like functionality.  So,
let's begin by creating an auxiliary definition for the purposes of the current
problem.  Basically, let's define a computation that simply gets the last write
time of a file:

```fsharp
let lastWriteTimeUtc path = logAs ("lastWriteTimeUtc: " + path) {
  return File.GetLastWriteTimeUtc path
}
```

The above definition uses the `logAs` combinator to define a persisted
computation.  The idea of the `logAs` combinator is that it defines a named
computation, whose execution is *logged*, so that that its result, and other
details, can be later identified and recovered&mdash;possibly in a another run
of the same or modified program.

The `lastWriteTimeUtc` computation does not depend on any other logged
computation.  Recall treats such computations as primitive and always reruns
such computations.  However, Recall does log the results of such primitive
computations, in this case the `DateTime` value returned by
`File.GetLastWriteTimeUtc`, and after recomputing the value, can compare the new
value with the old one, allowing Recall to avoid rerunning other computations
that depend on such primitive computations.

Let's then work to define a computation to calculate the sum of integers in a
file.  Here is the first attempt:

```fsharp
let sumLinesOfFile path = logAs ("sumLinesOfFile: " + path) {
  let sum =
    File.ReadAllLines path
    |> Array.sumBy (fun intLine -> int intLine)
  return sum
}
```

The above `sumLinesOfFile` computation works, but it doesn't buy us anything.
Can you see why?

Of course, it should be mentioned that in the context of this toy example we are
not interested robustness details like checking whether a file actually contains
integers like we expect.

The problem with the above `sumLinesOfFile` computation is that it always
computes the sum&mdash;whether or not the file has changed.  Fortunately this
is easy to fix.  Here is a modified version:

```fsharp
let sumLinesOfFile path = logAs ("sumLinesOfFile: " + path) {
  let! _ = lastWriteTimeUtc path
  let sum =
    File.ReadAllLines path
    |> Array.sumBy (fun intLine -> int intLine)
  return sum
}
```

What we changed is that now `sumLinesOfFile` computation depends on the
`lastWriteTimeUtc` computation.  Recall sees and essentially logs the result of
the `lastWriteTimeUtc` computation even though the result of `lastWriteTimeUtc`
isn't actually used by `sumLinesOfFile`.

The assumption that Recall operates upon is that programmers make sure that all
input that may have an effect on the output of a computation is logged for
Recall to see.  This may sound like a difficult requirement to fulfill, but it
is, in fact, fairly trivial, because any data that may have an effect on the
output can just be logged as part of a computation using operations like
`logAs`, which we have been using already, but also a other operations like
`log` and also the special `watch` primitive

```fsharp
do! watch (x1, x2, ..., xN)
```

that conveniently use an automatically derived identity for the logged data.

When Recall reruns the modified version of `sumLinesOfFile`, it first reruns the
`lastWriteTimeUtc` computation which is the only dependency of `sumLinesOfFile`.
Assuming the result of that hasn't changed, Recall decides that nothing has
changed so it simply recovers the previously logged result of `sumLinesOfFile`.

Let's then continue with a computation for reading all lines of a file:

```fsharp
let readAllLines path = update {
  let! _ = lastWriteTimeUtc path
  return File.ReadAllLines path
}
```

The above `readAllLines` is a partial logged computation.  It differs from the
previous computations in that it uses the `update` computation builder and the
result of the computation is not logged.  Of course, we could log the result,
but it makes little sense to log a potentially large amount of data that can be
trivially recomputed by reading the file.  We can rewrite the `sumLinesOfFile`
computation using this partially logged computation:

```fsharp
let sumLinesOfFile path = logAs ("sumLinesOfFile: " + path) {
  let! intLines = readAllLines path
  let sum =
    intLines
    |> Array.sumBy (fun intLine -> int intLine)
  return sum
}
```

We now have the building blocks to define a `sumLinesOfFiles` computation:

```fsharp
let sumLinesOfFiles (fileListPath: string) =
  logAs ("sumLinesOfFiles: " + fileListPath) {
    let! fileList = readAllLines fileListPath

    let! sums =
      fileList
      |> Array.mapLogged sumLinesOfFile

    let total =
      sums
      |> Array.sum

    return total
  }
```

The one new operation used in the above definition is the `Array.mapLogged`
function.  As its name suggests, it just runs a number of logged computations.
One can define it straightforwardly using it just basic monadic operations.

On the first run, the above computation would be performed to completion.
Assuming nothing has changed, on subsequent runs of the program the
`lastWriteTimeUtc` of the `fileListPath` file is computed and then the
`fileList` is read from disk.  Then the `lastWriteTimeUtc` computations for
the individual files are run.  Everything else is simply recreated based on
the logged data.

Now, you may wonder about having to read the `fileList` from disk.  This could
be avoided easily by logging that data.  As already said earlier, doing so
brings little benefit as it would simply move the burden of reading the file
list to Recall's log system.  If the computation of the file would be more
involved, it would make sense to log the results.

Here are a couple of questions:

* Describe what would happen if the last write time of one the files being
  summed would change and the `sumLinesOfFiles` computation would be rerun?

* Describe what would happen if the path of a new file would be added to the
  list of files and the `sumLinesOfFiles` computation would be rerun?

There is one simple improvement we can make to the `sumLinesOfFiles`
computation.  The sums of individual files do not depend on each other, so those
sums can be computed in parallel.  Recall basically starts every logged
computation as a separate lightweight thread, but when a new computation is
started the execution of the current computation normally immediately waits for
the started computation to finish.  In this case we can use the function
`Array.Parallel.mapLogged` to let `Recall` know it can produce the results in
parallel:

```fsharp
let sumLinesOfFiles (fileListPath: string) =
  logAs ("sumLinesOfFiles: " + fileListPath) {
    let! fileList = readAllLines fileListPath

    let! sums =
      fileList
      |> Array.Parallel.mapLogged sumLinesOfFile

    let total =
      sums
      |> Array.sum

    return total
  }
```
