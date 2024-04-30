#!/bin/bash

# Check Ghdira is installed
if [ ! -d "ghidra_11.0.3_PUBLIC" ]; then
  echo "Ghidra is not installed. Download from github."
  curl -OL https://github.com/NationalSecurityAgency/ghidra/releases/download/Ghidra_11.0.3_build/ghidra_11.0.3_PUBLIC_20240410.zip && \
  unzip -q ghidra_11.0.3_PUBLIC_20240410.zip && \
  rm ghidra_11.0.3_PUBLIC_20240410.zip
else
  echo "Ghidra is already installed. No further action needed."
fi

# Check if opam is installed
if command -v opam &> /dev/null
then
    echo "opam is already installed."
        if [ -d "$HOME/.opam" ]; then
        echo "opam has been initialized. No further action needed."
    else
        echo "opam is not initialized."
        echo "To initialize opam, please run 'opam init'."
        exit 1
    fi
else
    echo "opam is not installed."
    echo "To install opam, please run:"
    echo "sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
    exit 1
fi

# check if opam local switch already exists
if [ ! -d "_opam" ]; then
  opam switch -y create . --deps-only --packages=ocaml-base-compiler.5.1.0 && \
  opam install -y --deps-only .
else
  opam install -y --deps-only .
fi

echo "Initialization done."