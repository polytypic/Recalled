# Defining Computations that can be Recalled

Recalled is a library, an
[EDSL](http://en.wikipedia.org/wiki/Domain-specific_language) of a sort, that
can be used to define *persistent*, *incremental*, *parallel* computations such
as [build systems](http://en.wikipedia.org/wiki/Build_automation).  Recalled
isn't really a ready made
[traditional build system](http://en.wikipedia.org/wiki/List_of_build_automation_software)
as such.  The core of Recalled is very minimal and doesn't even contain built-in
functionality, like operations for checking file modification dates,
characteristics to build systems.  However, such operations can be implemented
in a straightforward manner and, in particular, *forward build systems*, that go
from inputs to outputs or sources to targets, can be more or less directly
programmed using Recalled.

## A toy example

Let's examine how Recalled can be used via a concrete toy example, meant to be
simple enough to be easily worked through without having to spend any time with
problem domain specific details

### Straightforward ordinary functions

Suppose that you have a file containing a list of files with one path per line.
Each file listed in that file contains a list of integers stored with one
integer per line.  You wish to write a program that takes the name of the file
containing the list of files and computes the total sum of all the integers in
the files.

Here is a straightforward pair of functions for that purpose:

```fsharp
let sumLinesOfFile (intListPath: string) : int =
  let intList = File.ReadAllLines intListPath
  intList
  |> Array.sumBy (fun intText -> int intText)
```

```fsharp
let sumLinesOfFiles (fileListPath: string) : int =
  let fileList = File.ReadAllLines fileListPath

  let sums =
    fileList
    |> Array.map sumLinesOfFile

  let total =
    sums
    |> Array.sum

  total
```

There is nothing special about the above `sumLinesOfFiles` function or the
helper `sumLinesOfFile`.

The idea is that you are using the `sumLinesOfFiles` function to compute the sum
whenever you need it&mdash;without necessarily even knowing whether any file has
actually changed.  Wouldn't it be nice if your program could be modified to
avoid needless recomputation?  For example, if just one file changes or a new
file is added, it would be nice if only the sum for that would be (re)computed
and the total would be computed from previously known sums and so on.

### Working incrementally

As hinted in the beginning, Recalled isn't really a ready made build tool, but
it allows one to straightforwardly program build tool like functionality.  So,
let's begin by creating an auxiliary definition for the purposes of the current
problem&mdash;a computation that simply gets the last write time of a file:

```fsharp
let lastWriteTimeUtc (path: string) : LogAs<Logged<DateTime>> =
  logAs ("lastWriteTimeUtc: " + path) {
    return File.GetLastWriteTimeUtc path
  }
```

The above definition uses the `logAs` combinator to define a persisted
computation.  The idea of the `logAs` combinator is that it defines a named
computation, that can be later identified, and whose execution is *logged*, so
that its result, and other details, can be later recovered&mdash;possibly in
another run of the same or modified program.  That is right.  Recalled is
designed in such a way that a programmer can easily incrementally modify a
working program so that on subsequent runs Recalled reuses whatever it can and
modified computations get rerun as necessary.

The `lastWriteTimeUtc` computation does not depend on any other logged
computation.  Recalled treats such computations as primitive and always reruns
such computations.  However, Recalled does log the results of such primitive
computations, in this case the `DateTime` value returned by
`File.GetLastWriteTimeUtc`, and after recomputing the value, can compare the new
value with the old one, allowing Recalled to avoid rerunning other computations
that depend on such primitive computations.

### Dependencies

Let's then work to define a computation to calculate the sum of integers in a
file.  Here is a first attempt:

```fsharp
let sumLinesOfFile (path: string) : LogAs<Logged<int>> =
  logAs ("sumLinesOfFile: " + path) {
    let sum =
      File.ReadAllLines path
      |> Array.sumBy (fun intLine -> int intLine)
    return sum
  }
```

The above `sumLinesOfFile` computation works, but it doesn't buy us much.  Can
you see why?

Of course, it should be mentioned that in the context of this toy example we are
not interested in robustness details such as checking whether a file actually
contains integers like we expect.  That is not the problem.

The real problem with the above `sumLinesOfFile` computation is that it always
computes the sum&mdash;whether or not the file has changed.  Fortunately this is
easy to fix.  Here is a modified version:

```fsharp
let sumLinesOfFile (path: string) : LogAs<Logged<int>> =
  logAs ("sumLinesOfFile: " + path) {
    let! _ = lastWriteTimeUtc path
    let sum =
      File.ReadAllLines path
      |> Array.sumBy (fun intLine -> int intLine)
    return sum
  }
```

What we changed is that now the `sumLinesOfFile` computation *depends on* the
`lastWriteTimeUtc` computation.  Recalled sees and essentially logs the result
of the `lastWriteTimeUtc` computation even though the result of
`lastWriteTimeUtc` isn't explicitly used by `sumLinesOfFile`.

### The essence of being Recalled

Recalled operates under the fundamental assumption that the programmer makes
sure that all input that may have an effect on the output of a computation is
logged for Recalled to see.  This may sound like a difficult requirement to
fulfill, but it is, in fact, fairly trivial, because any data that may have an
effect on the output can just be logged as part of a computation using
operations like `logAs`, which we have been using already, but also other
operations like `log` and `watch`

```fsharp
let! _ = log { ... ; return ... }
do! watch (input1, input2, ..., inputN)
```

that conveniently use an automatically derived identity, based on the
surrounding computation, for the logged data.

When Recalled reruns the modified version of `sumLinesOfFile`, it first reruns
the `lastWriteTimeUtc` computation, which is the only dependency of
`sumLinesOfFile`.  Assuming the result of that hasn't changed, Recalled decides,
based on the fundamental assumption, that nothing has changed so it simply
recovers the previously logged result of `sumLinesOfFile`.

This is also where Recalled differs from most traditional build systems and is
more like an adaptation of
[Self-Adjusting Computation](http://www.umut-acar.org/self-adjusting-computation).
Traditional build systems often have some specific built-in primitive kinds of
dependencies such as dependencies to files or they might be based on timestamps.
Upon rebuilds those systems use their built-in primitives to check dependencies
such as checking file modification dates or, say, MD5 hashes of file contents.
In Recalled, recomputations are triggered by changes in the result *values*
produced by the computations.  This makes Recalled particularly convenient for
working with ordinary host-language functions, while many existing build systems
try to primarily make it easy to work with external programs that read and write
*files*.

#### Getting sidetracked on values

Speaking of the ability to work with ordinary functions that return ordinary
values, here is an example of a computation that is surely familiar:

```fsharp
let rec fib (n: BigInteger) : LogAs<Logged<BigInteger>> =
  logAs ("fib: " + n.ToString ()) {
    if n < 2I then
      return n
    else
      let! x = fib (n-1I) |> wait
      let! y = fib (n-2I) |> wait
      return x + y
  }
```

What would be the time and space complexity of the above computation?

### Partial updates

Let's then continue with a computation for reading all lines of a file:

```fsharp
let readAllLines (path: string) : Update<array<string>> = update {
  let! _ = lastWriteTimeUtc path
  return File.ReadAllLines path
}
```

The above `readAllLines` is a *partial* logged computation.  It differs from the
previous computations in that it uses the `update` computation builder and the
final result of the computation is not logged.  An *update* is essentially a
primitive step or a sequence of steps used as part of a logged computation.  In
fact, the previous computations defined using `logAs` also consisted of updates.

Getting back to `readAllLines`, we could, of course, easily log the result if we
wanted to, but it makes little sense to log a potentially large amount of data
that can be trivially recomputed by reading the file.  Doing so would bring
little benefit as it would simply move the burden of reading the file list to
Recalled's log system.

### Readability

Using `readAllLines`, we can simplify the `sumLinesOfFile` computation:

```fsharp
let sumLinesOfFile (path: string) : LogAs<Logged<int>> =
  logAs ("sumLinesOfFile: " + path) {
    let! intLines = readAllLines path
    let sum =
      intLines
      |> Array.sumBy (fun intLine -> int intLine)
    return sum
  }
```

Written this way, `sumLinesOfFile` looks very much like just another ordinary
function.  This is also very much a design goal for Recalled.  The goal is to
make these kinds of *persistent* and *incremental* computations as readable as
possible in the sense that you can read the overall algorithm almost as easily
as you'd read ordinary code.

Speaking of readability, one potential worry might be that it might not be
obvious that there are computational effects, namely logging, going on.
Fortunately, even though the syntax of computational expressions tries to reduce
the syntactic differences, the exclamation marks conveniently point out the
places where special computational effects happen.

### Straightforward persistent and incremental computations?

Continuing with our toy, we now have the building blocks to define a
`sumLinesOfFiles` computation:

```fsharp
let sumLinesOfFiles (fileListPath: string) : LogAs<Logged<int>> =
  logAs ("sumLinesOfFiles: " + fileListPath) {
    let! fileList = readAllLines fileListPath

    let! sums =
      fileList
      |> Seq.mapLogAs (sumLinesOfFile >> wait)

    let total =
      sums
      |> Seq.sum

    return total
  }
```

The two new operations used in the above definition are the `Seq.mapLogAs` and
`wait`.  `Seq.mapLogAs` just runs a number of computations with a log.  `wait`,
on the other hand, makes a logged computation wait until its result is ready.

At this point you might want to compare this new persistent and incremental
version of `sumLinesOfFiles` to the original
[ordinary function](#straightforward-ordinary-functions).

On the first run, the above computation would be performed to completion.  On
subsequent runs, assuming nothing has changed, the `lastWriteTimeUtc` of the
`fileListPath` file would be computed and then the `fileList` would be read from
disk.  Then the `lastWriteTimeUtc` computations for the individual files would
be run one at a time.  Everything else would simply be recreated based on the
logged data.

Here are a couple of questions that the astute reader should have no trouble
answering:

* Describe what would happen if the last write time of one the files being
  summed would change and the `sumLinesOfFiles` computation would be rerun?

* Describe what would happen if the path of a new file would be added to the
  list of files and the `sumLinesOfFiles` computation would be rerun?

### Scaling to multiple cores

There is one simple improvement we can make to the `sumLinesOfFiles`
computation.  The sums of individual files do not depend on each other, so those
sums can be computed in parallel.  Recalled basically starts every logged
computation as a separate lightweight thread and you explicitly have to wait if
you need to access the result.  In this case we don't need the results
immediately.  We could just as well first start all the computations and then
read their results.

```fsharp
let sumLinesOfFiles (fileListPath: string) : LogAs<Logged<int>> =
  logAs ("sumLinesOfFiles: " + fileListPath) {
    let! fileList = readAllLines fileListPath

    let! loggedSums =
      fileList
      |> Seq.mapLogAs sumLinesOfFile

    let! sums =
      loggedSums
      |> Seq.mapUpdate read

    let total =
      sums
      |> Seq.sum

    return total
  }
```

This concludes the toy example, almost.  We haven't yet looked at how these
computation can be run.

### Running a logged computation

Logged computation are built upon the lightweight jobs of
[Hopac](https://github.com/VesaKarvonen/Hopac) and the `recall` operation of
Recalled, when given path to a directory for the log data, returns a job that
can be `run` using Hopac:

```fsharp
let sum : int = run <| recall ".recall" {
  return! sumLinesOfFiles "foo" |> wait
}
```

By the way, can you see now how the name *"Recalled"* is descriptive of Recalled
in at least two essential ways?
