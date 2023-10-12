(lang dune 3.7)
(using coq 0.7)
(maintainers "Jihee Park <jiheepark@kaist.ac.kr>")
(package
  (name pcode-decomp)
  (synopsis "Cerfitied decompilation for PCode")
  (depends zarith ocaml-lsp-server ocamlformat)
  (authors
    "Jihee Park")
)
 
 (generate_opam_files true)
