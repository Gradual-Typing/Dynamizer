Dynamizer produces all valid less precisely-typed versions of a valid
explicitly-typed program in Schml. The output is valid
[Schml](https://github.com/deyaaeldeen/schml) source code files
commented with how much they are dynamically-typed.
Dynamizer is primarily used for benchmarking the different implementations of
gradual typing in the Schml project.

## Compile

        $ stack clean; stack build

## Run

        $ stack exec dynamizer <path to the schml source code file without file extension>

will generate all less precisely-typed programs for that program and write them to disk in a directory of the same name of the original file beside it.

        $ stack exec dynamizer <path to the schml source code file without file extension> n

will randomly pick n programs from the lattice and write those in a directory with the same name of the original source code file beside that file.

<!--         $ <stack bin directory>/dynamizer <path to the completely-annotated schml source code file without file extension> <n1> <n2> -->

<!-- will generate all possible gradually-typed programs and partition them into n1 bins, sampling n2 programs from each bin and write them as specified before. -->

<!--         $ <stack bin directory>/dynamizer <path to the completely-annotated schml source code file without file extension> <n1> <n2> <n3> -->

<!-- will generate all possible gradually-typed programs and sample n3 programs from programs with percentage of dynamic typing between n1 and n2. -->

<!-- ## Issues -->

<!-- It is known that it consumes huge memory. For instance a typical quick sort program can easily rsult in billions of different gradually-typed versions, hence sampling is always preferred so you do not run out of inodes and/or RAM ;) -->
