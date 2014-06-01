# Recall

Recall is a library that implements a simple EDSL in F# for defining persistent,
incremental, parallel computations such as build systems easily.  Recall is
inspired by, but does not directly copy

* Umut Acar's and other's work on
  [Self-Adjusting Computation](http://www.umut-acar.org/self-adjusting-computation),
  and

* Neil Mitchell's work on [Shake](http://community.haskell.org/~ndm/shake/) also
  on [GitHub](https://github.com/ndmitchell/shake).

The implementation of Recall is based on two libraries:

* The [Hopac](https://github.com/VesaKarvonen/Hopac) library makes it easy to
  implement the necessary concurrent, parallel, and asynchronous programming
  patterns and also makes it practical to spawn each build computation as a
  separate lightweight thread.

* The [Infers](https://github.com/VesaKarvonen/Infers) library makes it easy to
  implement the necessary datatype generic operations, such as serialization
  (pickling), and with practical performance.

## Documentation

The document
[Recall or How to Define Computations that can be Recalled](Docs/Tutorial.md)
provides a tutorial for programming with Recall.  The programming interface of
Recall is very simple, compromising only a few central types and associated
computation builders and a handful of combinators.  See the
[Recall Library Reference](http://vesakarvonen.github.io/Recall/Recall.html) for
details.

## Download

Recall will be available via [NuGet](http://www.nuget.org/).
