inal: prev:
let
  disableCheck = package: package.overrideAttrs (o: { doCheck = false; });
  addCheckInputs = package:
    package.overrideAttrs ({ buildInputs ? [ ], checkInputs, ... }: {
      buildInputs = buildInputs ++ checkInputs;
    });
in {
  ocaml-ng = builtins.mapAttrs (_: ocamlVersion:
  ocamlVersion.overrideScope' (oself: osuper: {
    # Override here:
    })) prev.ocaml-ng;
}
