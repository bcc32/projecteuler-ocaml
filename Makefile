.PHONY: all check clean fmt test

all: check test fmt

check:
	dune build @check

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote

test:
	dune runtest --auto-promote
