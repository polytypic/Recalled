# The design of the persistent storage of Recalled

Recalled is based on idea of persistently storing the results and dependencies
of computations so that when the computations are being rerun, dependencies can
be checked for changes and recomputation can be avoided when dependencies have
not changed.  The raw speed at which the persistent storage can be read from
largely determines the *overhead time* when running programs written with
Recalled and the speed at which the persistent storage can be written to is also
crucial when new computations are being persisted.  This document describes the
design of the persistent storage architecture used by Recalled.

## What needs to be stored?

Before diving into the design of the persistent storage, it is important to
understand what actually needs to be stored by Recalled and what needs to be
retrieved by Recalled when rerunning previously stored computations.

Consider the following sketch of a computation:

```fsharp
let computation : LogAs<Logged<ResultType>> =
  logAs "identity of computation" {
    let! dependencyA = computationA
    let! dependencyB = computationB
    let result = expensiveProcessing dependencyA dependencyB
    return result
  }
```

The first thing to note is that the identity of a computation is an arbitrary
string, namely `"identity of computation"` in the above sketch.  That also goes
for the dependencies, namely `computationA` and `computationB` in the above
sketch.  Storing and comparing arbitrary strings would be fairly
expensive&mdash;particularly for storing the identities of dependencies, because
a specific computation may need to be referenced as a dependency many times.  To
reduce the amount of data to be stored and retrieved, Recalled only stores
*digests*, specifically 128-bit hashes, computed from the arbitrary identity
strings and matches those digests to the arbitrary strings computed when the
program is run.

The result of a computation can be an arbitrary F# type.  For example, a
computation that compresses a texture image might return a large array of bytes
that contains the compressed texture data, and a computation that preprocesses a
3D mesh for rendering might produce a large data structure containing various
kinds of data, such as arrays of vertices containing points, normals and
material data.  Potentially large amounts of non-trivial data needs to be stored
efficiently, but it must also be possible to avoid reading that data when it is
not needed, because reading all of the data would be very expensive when only a
fraction of it is really needed to process incremental changes.  Therefore
Recalled not only stores the result data, but also stores a digest of the result
data, so that Recalled only needs to retrieve the digests and compare them to
determine whether some result has changed.  For checking dependencies it is
sufficient to store just a single combined digest of the results of all the
dependencies.
