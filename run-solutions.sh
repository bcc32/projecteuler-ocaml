#!/bin/sh

set -euxo pipefail

function list_solutions {
    dune exec -p euler -- euler 2>&1 | awk '/Problem/ { print $1 }' | grep -v naive
}

echo "building..."
dune build -p euler
echo "done building"

if which parallel >/dev/null 2>&1; then
    parallel -k -- echo {1} '&&' dune exec --no-build -p euler -- euler {1} -time ::: $(list_solutions)
else
    for i in $(list_solutions); do
        dune exec --no-build -p euler -- euler $i -time
    done
fi
