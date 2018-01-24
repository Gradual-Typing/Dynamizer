## Dynamizer ##

Dynamizer is a command line tool that produces all valid less precisely-typed
variants of a valid explicitly-typed program in
[Grift](https://github.com/Gradual-Typing/Grift). The output is valid Grift
source code files commented with how much they are dynamically-typed.  Dynamizer
is primarily used for benchmarking the different implementations of gradual
typing in the Grift project.

I am still looking into extending the dynamizer to other gradually typed
languages such as reticulated python.

## Usage

Dynamizer takes the path to the Grift source file as input. It also has two additional optional parameters: `--samples` and `--bins`, where the former specifies how many samples to generate, and the latter specifies how many bins to sample from.
