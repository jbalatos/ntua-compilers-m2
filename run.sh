#!/usr/bin/env bash

./main < $1
llc-16 output.ll -o output.s
clang -fno-pie -no-pie -o a.out output.s base_lib.a
