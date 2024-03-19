#!/bin/bash
args=("$@")
pname=$1
exec ~/PCodeCert/_build/default/ocaml-core/src/tool/interpreter.exe -i $pname.fgir_dump -log-path $pname.log
