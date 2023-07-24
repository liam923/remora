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
 (depends ocaml dune base menhir ppx_jane ppx_deriving (ocamlformat (and :with-test (= 0.26.0))))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
