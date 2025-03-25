# CamlBlast - Installation Guide
We assume that the OCaml package installer, opam, is already installed for this program.

1. Begin by installing the necessary OPAM packages for this project.
    ```shell
    opam update
    opam upgrade
    opam install raylib raygui batteries qcheck ounit2
    ```
2. Run the program
    ```shell
    dune build
    dune exec bin/main.exe
    ```