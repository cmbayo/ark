with (import <nixpkgs> {});
mkShell {
    nativeBuildInputs = [
        ocaml
    ];
}