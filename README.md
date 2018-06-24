## Dynamizer ##

Dynamizer is a command line tool that produces all valid less precisely-typed
variants of a valid explicitly-typed program in
[Grift](https://github.com/Gradual-Typing/Grift). The output is valid Grift
source code files commented with how much they are dynamically-typed.  Dynamizer
is primarily used for benchmarking the different implementations of gradual
typing in the Grift project.

I am still looking into extending the dynamizer to other gradually typed
languages such as reticulated python.

## Compiling

```
stack build
```

## Usage
The dynamizer has several modes of execution, and will be described by example. However, it writes the generated configurations to disk as valid grift source files in a directory of the same name as the input program and located right beside that program.

```bash
dynamizer --help
```
>Dynamizer - lattice generator for gradual typing
>
>Usage: dynamizer FILE [--samples SAMPLES] [--bins BINS] [--coarse]
>  Generates gradually-typed configurations of a statically-typed Grift program
>
>Available options:
>  FILE                     File path of a grift program
>  --samples SAMPLES        Number of samples
>  --bins BINS              Number of bins (default: 1.0)
>  --coarse                 Enable coarse grained lattice
>  -h,--help                Show this help text

Let's grab a Grift program for demo:
```bash
cp https://github.com/Gradual-Typing/Grift/blob/master/benchmark/suite/macro/src/static/matmult.grift .
```

All configurations will be written to disk, 859963392 in this case, if no optional parameter is specified
```bash
dynamizer matmult.grift
> There are 859963392 less precisely typed programs and 33 type constructors
```

To generate 1000 samples, 100 from 0-10% statically typed bin, another 100 from 10-20%, and so on.
```bash
dynamizer matmult.grift --samples 100 --bins 10
```

To generate 2^n samples where n is the number of top level functions and each one is either fully typed or untyped
```bash
dynamizer matmult.grift --coarse
```
