Course project for B629 Topics in Programming Languages.

nAnnotizer produces all valid gradually-typed versions of a valid
implicitly-typed program in Simply Typed Lambda Calculus. It is
implemented in Haskell and Racket. The output is valid
[Schml](https://github.com/akuhlens/schml) source code files.

# An Example

## Haskell

I did not implement a parser, so the examples are encoded using data constructors.

(nAnnotize (Lam1 "x" (App1 (Var1 "x") (N1 42))))

## Racket

The examples are encoded as syntax objects.

(nAnnotize #'(lambda (x) (x 42)))



#### The result would be:

(lambda (x) : Int
(x 42))

(lambda (x) : Dyn
(x 42))

(lambda ([x : (-> Int Int)]) : Int
(x 42))

(lambda ([x : (-> Int Int)]) : Dyn
(x 42))

(lambda ([x : (-> Int Dyn)]) : Int
(x 42))

(lambda ([x : (-> Int Dyn)]) : Dyn
(x 42))

(lambda ([x : (-> Dyn Int)]) : Int
(x 42))

(lambda ([x : (-> Dyn Int)]) : Dyn
(x 42))

(lambda ([x : (-> Dyn Dyn)]) : Int
(x 42))

(lambda ([x : (-> Dyn Dyn)]) : Dyn
(x 42))
