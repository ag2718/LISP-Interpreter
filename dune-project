(lang dune 3.20)

(name lisp_interpreter)

(generate_opam_files true)

(source
 (github ag2718/lisp_interpreter))

(authors "Aarush Gupta <aarush.g75@gmail.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lisp_interpreter)
 (synopsis "A LISP Interpreter")
 (description "A LISP Interpreter written in OCaml")
 (depends ocaml core)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
