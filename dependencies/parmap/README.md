# Parmap in a nutshell

Parmap is a minimalistic library allowing  to exploit multicore architecture for
OCaml programs with minimal modifications: if you want to use your many cores to
accelerate an   operation  which  happens   to be   a  map,  fold    or map/fold
(map-reduce),  just use  Parmap's  `parmap`, `parfold`  and `parmapfold` primitives in
place  of the standard   `List.map`   and friends,  and  specify  the  number   of
subprocesses to use by the optional parameter `~ncores`.

See the `example` directory for a couple of running programs.

## DO'S and DONT'S

Parmap is *not*  meant to be a replacement  for a full fledged implementation of
parallelism skeletons  (map, reduce, pipe, and the  many others described in the
scientific literature   since the end   of the  1980's,   much earlier  than the
specific   implementation by Google   engineers  that popularised them).  It  is
meant,  instead, to allow you to  quickly leverage the  idle processing power of
your extra cores, when handling some heavy computational load.

The principle of parmap is very simple: when you call one of the three available
primitives, map, fold, and  mapfold , your OCaml  sequential program forks  in n
subprocesses (you choose the n), and each subprocess performs the computation on
the 1/n of the data, in chunks  of a size you  can choose, returning the results
through a shared memory area to the  parent process, that resumes execution once
all the children have terminated, and the data has been recollected.

You *need*  to run your  program *on a single multicore  machine*;  repeat after me:
`Parmap`  _is   not meant_ to   run  on a cluster,  see   one of the  many available
(re)implementations of the map-reduce schema for that.

By forking the parent process  on a single machine, the children get access, for
free, to all the data structures already built, even the imperative ones, and as
far as your computation  inside the map/fold  does not produce side effects that
need  to be  preserved, the  final result will   be the same  as  performing the
sequential operation, the only difference is that you might get it faster.

The OCaml  code is reasonably  simple and  only marginally relies  on external C
libraries: most of the magic is done by your operating  system's fork and memory
mapping   mechanisms.    One    could gain  some      speed  by implementing   a
marshal/unmarshal operation directly on bigarrays, but we did not do this yet.

Of course, if you happen  to have open  channels, or files, or other connections
that should only be  used by the parent  process, your program  may behave in  a
very wierd way: as an example, *do  not* open a  graphic window before calling a
Parmap primitive, and   *do   not*  use  this  library   if  your  program    is
multi-threaded!

## Pinning processes to physical CPUs

To obtain maximum  speed,  Parmap tries to  pin  the worker processes to  a CPU,
using  the scheduler  affinity  interface  that is  available   in recent  Linux
kernels.   Similar functionality may be  obtained  on different platforms  using
slightly different API. Contributions  are welcome to  support those other APIs,
just make sure that you use autoconf properly.

## Using Parmap with Ocamlnat

You can use Parmap in a native toplevel  (it may be quite useful  if you use the
native toplevel to perform fast interactive computations), but remember that you
need to load the `.cmxs` modules in it; an example is given in `example/topnat.ml`

## Preservation of output order in Parmap

If the number of chunks is equal to the number of cores, it is easy to preserve
the order of the elements of the sequence passed to the map/fold operations, so
the result will be a list with the same order as if the sequential function would
be applied to the input. This is what the `parmap`, `parmapfold` and `parfold` functions
do when the chunksize argument is not used.

If the user specifies a chunksize that is different from the number of cores,
the current implementation for `parmap`, `parmapi`, `array_parmap` and
`array_parmapi` by default does not guarantee the preservation of the order
of the results. If the `keeporder` parameter is set to true, an alternative
implementation is used, that tags the chunks and reorders them at the end, so the result of
calling `Parmap.parmap f l` is the same as `List.map f l`. Depending on the
nature of your workload (in particular, number of chunks and size of the results),
this may be way more efficient than implementing a sorting mechanism yourself, but
may also end up using up to twice the space and time of the default implementation:
there is a tradeoff, and it is up to the user to choose the solution that better suits him/her.

No reordering logic is implemented for `parmapfold`, `parfold` and their
variants, as performing these operations in parallel only make sense if the
order is irrelevant.

In general, using little chunksize helps in balancing the load among the
workers, and provides better speed, but incurs a little overhead for tagging and
reordering the chunks: there is a tradeoff, and it is up to the user to choose
the solution that better suits him/her.

## Fast map on arrays and on float arrays

Visiting an array is much faster than visiting a list, and conversion of an array
to and from a list is expensive, on large data structures, so we provide a specialised
version of map on arrays, that beaves exactly like parmap.

We also provide a highly optimised specialised parmap version that is targeted
to float arrays, `array_float_parmap`, that allows you to perform parallel
computation on very large float arrays efficiently, without the boxing/unboxing
overhead introduced by the other primitives, including `array_parmap`.

To understand the efficiency issues involved in the case of large arrays of float,
here is a short summary of the steps that any implementation of a parallel map
function must perform.

 1. create a float array to hold the result of the computation.
    This operation is expensive: on an Intel i7, creating a 10M float array
    takes 50 milliseconds
    ```ocaml
        ocamlnat
             Objective Caml version 3.12.0 - native toplevel

        # #load "unix.cmxs";;
        # let d = Unix.gettimeofday() in ignore(Array.create 10000000 0.); Unix.gettimeofday() -. d;;
        - : float = 0.0501301288604736328
    ```

 2. create a shared memory area,

 3. possibly copy the result array to the shared memory area,

 4. perform the computation in the children writing the result in the shared memory area,

 5. possibly copy the result back to the OCaml array.

All implementations need to do 1, 2 and 4; steps 3 and/or 5 may be omitted depending on
what the user wants to do with the result.

The `array_float_parmap` performs steps 1, 2, 4 and 5. It is possible to share steps
1 and 2 among subsequent calls to the parallel function by preallocating the result
array and the shared memory buffer, and passing them as optional parameters to the
`array_float_parmap` function: this may save a significant amount of time if the
array is very large.

## Install 

### With opam

```
opam install parmap
```

### From source

```
make
make install
make test
```
