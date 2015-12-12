nAnnotizer produces all valid gradually-typed versions of a valid
explicitly-typed program in Schml. The output is valid
[Schml](https://github.com/deyaaeldeen/schml) source code files
commented with how much they are dynamically-typed.
nAnnotizer is primarily used for benchmarking the different semantics of
gradual typing implemented by the Schml project.

## Compile

        $ stack clean; stack build

## Run

        $ <stack bin directory>/nAnnotizer <path to the completely-annotated schml source code file without file extension> dry

will print the number of all possible gradually-typed programs for this specific program, and the number of type nodes in it.

        $ <stack bin directory>/nAnnotizer <path to the completely-annotated schml source code file without file extension>

will generate all possible gradually-typed programs and write them in a directory with the same name of the original source code file beside that file.

        $ <stack bin directory>/nAnnotizer <path to the completely-annotated schml source code file without file extension> <n1> <n2>

will generate all possible gradually-typed programs and partition them into n1 bins, sampling n2 programs from each bin and write them as specified before.

        $ <stack bin directory>/nAnnotizer <path to the completely-annotated schml source code file without file extension> <n1> <n2> <n3>

will generate all possible gradually-typed programs and sample n3 programs from programs with percentage of dynamic typing between n1 and n2.

## Issues

It is known that it consumes huge memory. For instance a typical quick sort program can easily result in more than 26 million different gradually-typed versions, hence sampling is always preferred so you do not run out of inodes ;)
