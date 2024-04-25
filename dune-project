(lang dune 3.14)

(generate_opam_files)

(package
 (name textmate)
 (depends
  (ocaml
   (>= 5.1.0))
  (reason
   (>= 3.11.0))
  yojson
  markup
  ppx_let
  ppx_deriving
  (ocaml-lsp-server :with-test)
  (ocamlformat :with-test)))
