#!/bin/bash
args=("$@")
pname=$1
exec $FIBLE_PATH/_build/default/ocaml-core/src/tool/sim_checker.exe $pname -log-path $pname.log
