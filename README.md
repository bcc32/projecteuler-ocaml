# projecteuler-ocaml
My solutions to ProjectEuler problems in OCaml.  Don't spoil yourself, please!

# How to run

```
dune build -p euler
dune exec -p euler -- euler PROBLEM_NUMBER [-time]
```

If passed the `-time` flag, the program will report the time taken to execute
the solution code.

The `EULER_DEBUG` environment variable, when set to a non-empty string, will
enable debug/progress printing in some solutions.
