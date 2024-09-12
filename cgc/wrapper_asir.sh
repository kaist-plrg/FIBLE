#!/bin/bash
args=("$@")
pname=$1
exec $FIBLE_PATH/_build/default/ocaml-core/src/tool/interpreter.exe $pname.asir_dump -log-path $pname.log
