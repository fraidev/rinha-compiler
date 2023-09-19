(lang dune 3.2)

(name rinha)

(version 0.1)

(maintainers "fraifelipe@gmail.com")

(generate_opam_files true)

(package
 (name rinha)
 (synopsis "rinha")
 (description "rinha")
 (depends
  (alcotest :with-test)
  (dune
   (and
    :build
    (>= 3.1.1)))
  progress
  eio
  eio_main
  bencode
  stdint
  uri
  ppx_deriving))
