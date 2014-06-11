# Recall

Recall is a library that implements a simple
[EDSL](http://en.wikipedia.org/wiki/Domain-specific_language) in F# for defining
persistent, incremental, parallel computations such as build systems.

## The elevator pitch

Imagine that you have a program that reads some *large set of input data* and
then makes some *expensive computations* based on that data to give you *some
output* that you need.  You continuously make *small incremental changes* to
that input data and then you need to *run* and wait for the program *after each
change* to compute the output that you need.

It doesn't really matter what the input data is, what those expensive
computations are or what the output is, but it matters that there are *plenty of
subcomputations whose inputs and outputs do not change* between each run of the
program.

Recall is an EDSL that allows you to conveniently write programs that take
advantage of those unchanged subcomputations to only *incrementally* compute the
parts of the whole computation that need to be computed due to the incremental
changes in the input data.

Instead of having a program whose run time is proportional to the whole input to
output translation, the Recalled version of the program has *run time
proportional to the changes*.

## Download

Experimental package [Recalled](http://www.nuget.org/packages/Recalled/) is
available via [NuGet](http://www.nuget.org/).  **Recall is not yet ready for
production use.** I do not expect radical changes to the basic programming
interface of the library, so you should feel free to play with the experimental
release, but there are several significant performance and robustness
improvements that I have in mind for the implementation and also some additions
to the programming interface.

Note that the package name differs from the current library name.  For some
reason NuGet didn't tell me there already was a package by the name Recall when
I searched for it a couple of months ago.  I will likely rename this whole
repository and library.

## Documentation

The document [Defining Computations that can be Recalled](Docs/Tutorial.md)
provides a tutorial for programming with Recall.  The programming interface of
Recall is very simple, compromising only a few central types and associated
computation builders and a handful of combinators.  See the
[Recall Library Reference](http://vesakarvonen.github.io/Recall/Recall.html) for
details.

The implementation of Recall is based on two infrastructure libraries:

* The [Hopac](https://github.com/VesaKarvonen/Hopac) library makes it easy to
  implement the necessary concurrent, parallel, and asynchronous programming
  patterns and also makes it practical to spawn each computation as a separate
  lightweight thread.

* The [Infers](https://github.com/VesaKarvonen/Infers) library makes it easy to
  implement the necessary datatype generic operations, such as serialization
  (pickling), and with practical performance.

Abstractions from these libraries are also present in the inferface of Recall.

Recall is inspired by, but is significantly different from

* Umut Acar's and other's work on
  [Self-Adjusting Computation](http://www.umut-acar.org/self-adjusting-computation),
  and

* Neil Mitchell's work on [Shake](http://community.haskell.org/~ndm/shake/) also
  on [GitHub](https://github.com/ndmitchell/shake).  Anton Tayanovskyy has also
  created [FShake](https://github.com/intellifactory/fshake), which is inspired
  by Shake.

Understanding those works is not at all necessary to use Recall or to understand
its implementation, but might be of general interest.
