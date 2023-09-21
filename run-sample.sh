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

./_build/default/executor/main.exe -i $CWD/sample/add -func-path "$CWD/sample/add.funcs" -dump-cfa-path "$CWD/sample/" -g $GHIDRA_PATH