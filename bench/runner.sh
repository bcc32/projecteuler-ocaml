export BENCHMARKS_RUNNER=TRUE
export BENCH_LIB=euler_bench
exec dune exec -- ./main.exe -fork -run-without-cross-library-inlining "$@"
