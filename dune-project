(lang dune 3.7)
(maintainers "Jihee Park <jiheepark@kaist.ac.kr>")
(package
  (name multi-step-decomp)
  (synopsis "Multi-step decompilation for PCode")
  (depends ppxlib zarith ocamlgraph ocaml-lsp-server ocamlformat ctypes ctypes-foreign angstrom sexplib ppx_sexp_conv xml-light)
  (authors
    "Jihee Park")
)
 
 (generate_opam_files true)
