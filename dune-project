(lang dune 3.7)

(name remora)

(generate_opam_files true)

(source
 (github liam923/remora))

(authors "Liam Stevenson")

(maintainers "Liam Stevenson")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name remora)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  base
  menhir
  re2
  ppx_jane
  ppx_deriving
  (core_unix :with-test)
  (ocamlformat
   (and
    :with-test
    (= 0.26.0))))
 (tags
  (topics "to describe" your project)))

(map_workspace_root false)
