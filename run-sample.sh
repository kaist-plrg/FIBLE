#!/bin/bash

# check $GHIDRA_PATH exists
if [ -z "$GHIDRA_PATH" ]; then
    echo "GHIDRA_PATH is not set"
    exit 1
fi

# check sample directory exists
if [ ! -d "sample" ]; then
    echo "sample directory does not exist"
    exit 1
fi

CWD=$(pwd)

# ./_build/default/ocaml-core/main.exe -i $CWD/sample/add -func-path "$CWD/sample/add.funcs" -dump-path "$CWD/sample/" -dump-cfa -dump-spfa -dump-l1 -dump-basic-block -dump-l2 -g $GHIDRA_PATH


$GHIDRA_PATH/support/analyzeHeadless $CWD/tmp tmptmptmp -import $CWD/sample/add -postScript "GenGround.java" "$CWD/sample/add.funcs" "$CWD/sample" -scriptPath $CWD -deleteProject
