#!/bin/bash

# Check if opam is installed
if command -v opam &> /dev/null
then
    echo "opam is installed."
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
