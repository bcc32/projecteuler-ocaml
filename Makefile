.PHONY: all check clean fmt test

all:
	dune build @check @runtest @fmt --auto-promote

check:
	dune build @check

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote

test:
	dune runtest --auto-promote
