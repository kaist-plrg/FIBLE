#!/bin/bash
args=("$@")
pname=$1
GHIDRA_PATH=/home/j31d0/ghidra_10.3_PUBLIC exec ~/PCodeCert/_build/default/ocaml-core/src/tool/interpreter.exe -i $pname -func-path $pname.funcs -log-path $pname.log -log-feature float
