# JavaScript Interpreter

## System Requirements
* Install OCaml
  * [Unix and macOS](https://ocaml.org/docs/installing-ocaml#installation-on-unix-and-macos)
  * [Windows](https://ocaml.org/docs/installing-ocaml#installation-on-windows)
* Install `dune`
  * `opam install dune`
* Install `acorn`
  * `npm install -g acorn` (`-g` implies that it will be installed globally,
    excluding `-g` will install it locally)


## Type checking

The program reads from standard input. The input can be piped into the following command:

```bash
dune exec check
```