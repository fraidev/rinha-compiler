{ pkgs, doCheck ? true, static ? false, nix-filter }:

let inherit (pkgs) lib stdenv ocamlPackages;

in with ocamlPackages;
buildDunePackage rec {
  pname = "rinha";
  version = "0.0.0-dev";

  src = with nix-filter.lib;
    filter {
      root = ../.;
      include = [ "bin" "lib" "dune-project" "rinha.opam" ];
    };

  checkInputs = [ alcotest ];

  propagatedBuildInputs = [
    # Put dependencies here if you're creating a library
  ];

  buildInputs =
    [ ppx_deriving ];

  inherit doCheck;

  # Remove every directory which could have links to other store paths.
  # This makes the result much smaller
  isLibrary = false;
}
