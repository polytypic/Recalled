# The design of the persistent storage of Recalled

Recalled is based on the idea of persistently storing the results and
dependencies of computations so that when the computations are being rerun,
dependencies can be checked for changes and recomputation can be avoided when
dependencies have not changed.  The raw speed at which the persistent storage
can be read from largely determines the *overhead time* when running programs
written with Recalled and the speed at which the persistent storage can be
written to is also crucial when new computations are being persisted.  This
document describes the design of the persistent storage architecture used by
Recalled.

## What needs to be stored and retrieved?

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
sketch.  Storing, retrieving and comparing arbitrary length strings would be
fairly expensive&mdash;particularly for storing the identities of dependencies,
because a specific computation may need to be referenced as a dependency many
times.  To reduce the complexity of the data to be stored and retrieved,
Recalled only stores *digests*, specifically 128-bit hashes, computed from the
arbitrary identity strings and matches those digests to the arbitrary strings
computed when the program is run.  Unlike with arbitrary strings, and aside from
computing a digest from arbitrary data, operations on digests can be performed
in *constant time* and typically using only a few machine instructions.

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

## The basic idea: Log structured storage

Consider the data stored by a typical build system.  Most of the data remains
untouched from one run of the build system to the next.  It obviously makes
sense to avoid having to rewrite or move data that has not changed.  On the
other hand, some tiny part of the data is likely to be under active development
and changes from one build to the next.  Once the initial development is
finished, the data becomes part of the majority that only changes infrequently.
It therefore seems reasonable to store changes simply by appending them to the
storage, because that is unlikely to immediately waste large amounts of storage
space, because most of the live data remains untouched.

This is exactly the main approach employed by the persistent storage system of
Recalled to store computations.  Specifically, records of new computations are
simply appended to the end of a file.  When a previously known computation
changes, the new data is also appended to the end of a file, but it is also
recorded that the previous version of the computation has been removed.  (As a
special case, Recalled reuses old storage space when the size of the new entry
exactly matches the size of the old entry.)  When Recalled later reads an
existing *log* of computations, it effectively redoes all the operations, both
*add* and *remove* operations, to reconstruct the last persisted state of the
storage.

Some of the main benefits of a
[log structured storage](http://blog.notdot.net/2009/12/Damn-Cool-Algorithms-Log-structured-storage)
mechanism are that it avoids the need to move data around and can be very easy
to implement.  Another benefit is that a single log file can store a large
number of entries.  Traditional file systems are not particularly efficient when
there is a need to store potentially hundreds of thousands of individual files.

## Avoiding the need to wait for the whole log to be read

When a Recalled program starts, the persistent log is read and computations
created at runtime are matched to the entries in the log.  As the number of
entries stored in the log increases, the time it takes to reconstruct the last
persisted state of the storage also increases.  Should a Recalled program need
to wait until the whole log has been read, the overhead time might become a
major limiting factor.  The storage system used by Recalled uses two techniques
to minimize the log reconstruction overhead time.

The first technique used is to store add and remove entries in separate log
files.  The add entries are stored sequentially in the add log file.  Every add
entry can therefore be implicitly given a sequence number.  The remove log then
simply contains integer sequence numbers referring to removed add entries.  When
Recalled reads an existing log, the log file containing remove entries is first
sorted, and when the add entries are then processed sequentially, the sorted
list of removed indices can be consulted very efficiently.

Even with this organization it may still take a lot of time to process all the
entries.  Therefore Recalled performs reconstruction in a separate thread and
allows other threads to make queries for log entries during reconstruction.  As
soon as a particular entry has been read from the storage, queries for that
entry can be satisfied.  Queries for entries that do not exist in the log are
answered negatively as soon as the entire log has been read.  This approach
allows a Recalled program to effectively start processing computations before
the log has been read completely.

## Separate storage for binary objects or BOBs

When a Recalled program runs, an attempt is made to match all created
computations to previously logged computations and generally the digests of all
results are needed.  However only a subset of the result values need to be
reconstructed or deserialized from the log.  At the limit, only the last final
result of a Recalled computation may need to be deserialized from the log.  To
avoid slowing down the process of reading the add entries containing the
digests, the less frequently needed result data of computations is stored in yet
another log and accessed separately only when needed.

## Memory mapped buffers

To make operations on the various log files as efficient as possible, Recalled
uses memory mapped files to implement the dynamically grown log buffers.
Serialization and deserialization operations directly read and write the memory
mapped files without need for intermediate abstractions such as streams.  This
approach admits *nearly optimal serialization and
deserialization*&mdash;theoretically even approaching memory bandwidth limits.
To deserialize a 64-bit integer, for example, a single aligned 64-bit read
operation is sufficient and to deserialize an array of 64-bit integers, one can
simply make a sequence of aligned reads from the memory mapped buffer.  The sort
of the remove entries is performed in-place and the sorted result becomes
persisted.  Memory mapped buffers also make it convenient to perform
serialization and deserialization operations in parallel.  The operating system
takes care of managing the necessary IO operations and can make effective use of
RAM to cache the most frequently needed regions of the memory mapped files.

## Compacting the log structured storage

As the set of data being computed with Recalled evolves, the log storage will
not only store the live entries, but will also contain entries that have been
removed and replaced by new entries and entries that are simply no longer used.
Once the percentage of dead data becomes high enough, the log storage can be
compacted in linear time by going through the entries and copying or shifting
live data towards the beginning of the log files.  This results in the log files
being first read and then written once from beginning to end.  It should not be
difficult for the OS to perform this sort of linear IO access pattern at nearly
maximum IO bandwidth.

## A picture is worth a thousand words

Here is a picture of a possible log state:

<img src="http://polytypic.github.io/Recalled/LogDiagram.svg"/>

At the top are the *remove* entries.  In the middle are the *add* entries.  In
the bottom are the *bob* entries.  For every add there is a corresponding bob.
Two entries have been removed and are shown in darker color.

## Summary

The efficiency of the Recalled library depends crucially on the efficiency of
the persistent storage mechanism.  For this reason Recalled implements a
relatively simple, yet highly IO efficient, log structured storage mechanism
designed for the needs of the Recalled library.
