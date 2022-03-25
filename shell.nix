with (import <nixpkgs> {});
mkShell {
    nativeBuildInputs = [
        ocaml
        ocamlPackages.ocamlbuild
    ];
}
