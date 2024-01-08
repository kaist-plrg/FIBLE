#!/bin/bash

# check if opam local switch already exists
if [ ! -d "_opam" ]; then
  opam switch -y create . --deps-only --packages=ocaml-base-compiler.5.1.0 && \
  opam install --deps-only .
else
  opam install --deps-only .
fi
