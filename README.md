nAnnotizer produces all valid gradually-typed versions of a valid
explicitly-typed program in Simply Typed Lambda Calculus. It is
implemented in Haskell. The output is valid
[Schml](https://github.com/akuhlens/schml) source code files commented
with how much they are statically-typed.

The result files are written in a directory that has the same name of
the source file.

nAnnotizer is primarily used for benchmarking the different semantics of
gradual typing implemented in the Schml project.
