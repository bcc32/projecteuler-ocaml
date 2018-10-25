#!/bin/sh

echo "module Parse = Parse"
for file in *.txt; do
    num="$(basename "$file" .txt)"
    echo "module Problem_$num = Problem_$num"
done
