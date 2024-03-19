#!/bin/bash
args=("$@")
pname=$1
GHIDRA_PATH=/home/j31d0/ghidra_10.3_PUBLIC exec ~/PCodeCert/_build/default/ocaml-core/src/tool/sim_checker.exe -i $pname -log-path $pname.log
