#!/bin/sh

set -euxo pipefail

list_solutions() {
    dune exec -p euler -- euler 2>&1 | awk '/Problem/ { print $1 }' | grep -v naive
}

run_solution() {
    sol=$1
    echo "$sol"
    dune exec -p euler --no-build -- euler "$sol" -time
}
export -f run_solution

echo "building..."
dune build -p euler
echo "done building"

if command -v parallel >/dev/null 2>&1; then
    list_solutions \
        | parallel --keep-order run_solution
else
    for i in $(list_solutions); do
        run_solution "$i"
    done
fi
