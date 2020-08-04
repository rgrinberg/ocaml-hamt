(lang dune 1.11)
(name hamt)
(generate_opam_files true)
(maintainers "rudi.grinberg@gmail.com")
(authors
  "Thibault Suzanne"
  "Gabriel Scherer"
  "Rudi Grinberg")
(source (github rgrinberg/ocaml-hamt))
(bug_reports "https://github.com/rgrinberg/ocaml-hamt/issues")
(documentation "https://rgrinberg.github.io/ocaml0hamt/")

(package
  (name hamt)
  (depends
    stdlib-shims
    (ocaml (>= 4.01.0))
    (ounit (and :test (>=" 1.0.2"))))
  (synopsis "Hash Array Mapped Tries")
  (description "HAMT data structure implemented in OCaml

http://gallium.inria.fr/blog/implementing-hamt-for-ocaml/"))
