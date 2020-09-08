(lang dune 1.11)
(using fmt 1.2)
(name ocamlpaxos)
(source (github cjen1/OcamlPaxos_api))
(authors "Chris Jensen")

(allow_approximate_merlin)

(generate_opam_files true)

(package
  (name ocamlpaxos_api)
  (synopsis "Capnp api for ocamlpaxos")
  (depends capnp))

