(lang dune 2.9)
(name windows-version)

(generate_opam_files true)

(source (github ocurrent/windows-version))
(authors
 "Antonin Décimo <antonin@tarides.com>"
 "David Allsopp <david.allsopp@metastack.com>")
(maintainers
 "Antonin Décimo <antonin@tarides.com>"
 "David Allsopp <david.allsopp@metastack.com>")
(license ISC)
(package
 (name windows-version)
 (synopsis "List Windows 10 versions, LCUs, and their status")
 (description
  "List Windows 10 versions, LCUs, and their status.")
 (depends
  astring
  fmt
  ppx_deriving
  ppx_sexp_conv
  sexplib0))
