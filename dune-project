(lang dune 3.7)

(name ticketsystem)

(generate_opam_files true)

(source
 (github edustocchero/ticketsystem))

(authors "Eduardo Stocchero")

(maintainers "Eduardo Stocchero")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ticketsystem)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune opium uuidm ppx_deriving_yojson yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
