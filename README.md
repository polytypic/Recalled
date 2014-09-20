> [![Build status](https://ci.appveyor.com/api/projects/status/aav8lotwvhvo43gr)](https://ci.appveyor.com/project/VesaKarvonen/recalled) &#xb7; [![Build Status](https://travis-ci.org/VesaKarvonen/Recalled.svg?branch=master)](https://travis-ci.org/VesaKarvonen/Recalled)

# Recalled

Recalled is a library that implements a simple
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

Recalled is an EDSL that allows you to conveniently write programs that take
advantage of those unchanged subcomputations to only *incrementally* compute the
parts of the whole computation that need to be computed due to the incremental
changes in the input data.

Instead of having a program whose run time is proportional to the whole input to
output translation, the Recalled version of the program has *run time
proportional to the changes*.

## Download

Experimental package [Recalled](http://www.nuget.org/packages/Recalled/) is
available via [NuGet](http://www.nuget.org/).

I do not expect radical changes to the basic programming interface of the
library, but there are still a few important robustness improvements that I have
in mind for the implementation.  See the end of this page for a description of
planned improvements.

## Documentation

The document [Defining Computations that can be Recalled](Docs/Tutorial.md)
provides a tutorial for programming with Recalled.  The programming interface of
Recalled is very simple, compromising only a few central types and associated
computation builders and a handful of combinators.  See the
[Recalled Library Reference](http://vesakarvonen.github.io/Recalled/Recalled.html)
for details.

The document [The design of the persistent storage of Recalled](Docs/LogArch.md)
discusses the design of the persistent storage mechanism of Recalled.  You do
not need to read that document to use the Recalled library.

## Presentations

Here are slides I've prepared for talks on Recalled:

* [6.8.2014](http://vesakarvonen.github.io/Recalled/Recalled-060814.pdf) in
  Helsinki. *Note: The missing diagram from the "Picture" slide will be added
  later.*

* [25.6.2014](http://vesakarvonen.github.io/Recalled/Recalled-250614.pdf) in New
  York.

## Background material

The implementation of Recalled is based on two infrastructure libraries:

* The [Hopac](https://github.com/VesaKarvonen/Hopac) library makes it easy to
  implement the necessary concurrent, parallel, and asynchronous programming
  patterns and also makes it practical to spawn each computation as a separate
  lightweight thread.

* The [Infers](https://github.com/VesaKarvonen/Infers) library makes it easy to
  implement the necessary datatype generic operations, such as serialization
  (pickling), and with practical performance.

Abstractions from these libraries are also present in the inferface of Recalled.

Recalled is inspired by, but is significantly different from

* Umut Acar's and other's work on
  [Self-Adjusting Computation](http://www.umut-acar.org/self-adjusting-computation),
  and

* Neil Mitchell's work on [Shake](http://community.haskell.org/~ndm/shake/) also
  on [GitHub](https://github.com/ndmitchell/shake).  Anton Tayanovskyy has also
  created [FShake](https://github.com/intellifactory/fshake), which is inspired
  by Shake.

Understanding those works is not at all necessary to use Recalled or to
understand its implementation, but might be of general interest.

## Planned improvements

Robustness improvements:

* Currently Recalled does not implicitly guard against changes in the log
  format.  Recalled should be improved to check a log format revision number and
  if the format has changed, Recalled should simply clear the log and recompute
  everything.

* Currently Recalled does not clean up the computation log.  There is a simple
  and efficient way to compact the log using a linear pass over the log and
  Recalled should be improved to perform such a clean up pass whenever the log
  is holding a large percentage of deleted items.

Interface or flexibility improvements:

* Currently Recalled implicitly uses the `PU` inference rules (see the reference
  docs) to generate serialization capabilities for arbitrary types.  For clarity
  and flexibility, the interface of Recalled should also provide ways for user
  code to explicitly provide specialized serialization capabilities for Recalled
  to use.

After the above improvements, Recalled should basically "just work" for carefree
persisted, incremental, parallel computations and would be ready to be tagged as
version "1.0.0".

Scalability improvements:

* Recalled has been designed with the idea in mind that the results of
  computations can be shared over a network of workstations.  This is currently
  enabled in the form of the `digest` combinator and user code should be able to
  implement the necessary network stuff on top of Recalled.  Sharing of results
  over a network of workstations could and should also be just directly
  supported by Recalled.

* Recalled can currently be used to write programs that perform incremental
  computations when (re)run.  It would also be possible to extend the internals
  of Recalled to support *change propagation* like in Self-Adjusting
  Computation.  This way Recalled could also be used to implement build "agents"
  that monitor sources for changes and then perform incremental builds after
  changes.
