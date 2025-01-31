(lang dune 3.16)

(name MGSLang)

(generate_opam_files true)

(source
 (github MgShepherd/MGS-Lang))

(authors "Michael Shepherd")
(maintainers "Michael Shepherd")

(package
 (name MGSLang)
 (synopsis "The MGS Programming language")
 (depends ocaml dune (alcotest :with-test)))
