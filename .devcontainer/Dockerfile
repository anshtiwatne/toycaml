FROM ocaml/opam:latest

RUN opam install -y dune ocaml-lsp-server ocamlformat
RUN echo 'eval $(opam env)' >> ~/.bashrc
