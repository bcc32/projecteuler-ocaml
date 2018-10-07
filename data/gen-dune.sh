#!/bin/sh

set -eu

for file in *.txt; do
    num="$(basename "$file" .txt)"
    cat <<EOF
(rule
 (targets problem_$num.ml)
 (deps "$file")
 (action (with-stdout-to %{targets} (run ./gen-problem.sh $num))))

EOF
done
