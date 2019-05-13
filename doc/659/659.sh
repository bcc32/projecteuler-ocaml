#!/bin/bash

do_for_k() {
    awk '{print $1*$1*4+1}' | factor | awk '{print $NF}' | ./sum
}
export -f do_for_k

seq 1 10000000 |
    parallel --pipe do_for_k |
    ./sum
