(lang dune 3.7)
(using coq 0.7)
(maintainers "Jihee Park <jiheepark@kaist.ac.kr>")
(package
  (name coq-pcode)
  (synopsis "A Coq description for pcode")
  (depends (coq-ext-lib (>= 0.11.1)) zarith)
  (authors
    "Jihee Park")
)
 
 (generate_opam_files true)