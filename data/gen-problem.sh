#!/bin/sh

set -eu

num=$1

if grep -F '|}' "$num.txt" >/dev/null 2>&1; then
    >&2 echo "$num.txt contains delimited quote, aborting"
    exit 1
fi

cat <<EOF
let data = {|$(cat "$num.txt")|}
EOF
