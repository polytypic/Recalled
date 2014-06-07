# Recall

Recall is a library that implements a simple EDSL in F# for defining persistent,
incremental, parallel computations such as build systems.

## Download

Experimental package [Recalled](http://www.nuget.org/packages/Recalled/) is
available via [NuGet](http://www.nuget.org/).

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
  on [GitHub](https://github.com/ndmitchell/shake).

Understanding those works is not at all necessary to use Recall or to understand
its implementation, but might be of general interest.
