{ pkgs, rinha }:

with pkgs;
with ocamlPackages;
mkShell {
  OCAMLRUNPARAM = "b";
  inputsFrom = [ rinha ];
  packages = [ nixfmt ocamlformat ocaml findlib dune odoc ocaml-lsp ];
}
