# Remora

> Remora is a higher-order, rank-polymorphic array-processing program- ming language, in the same general class of languages as APL and J. It is intended for writing programs to be executed on parallel hardware.

Source: [Introduction to Rank-polymorphic Programming in Remora](https://arxiv.org/abs/1912.13451)

Remora was designed as the [PhD thesis of Justin Slepak](https://ccs.neu.edu/~jrslepak/Dissertation.pdf), and its semantics and type system are laid out in the paper [The Semantics of Rank Polymorphism](https://arxiv.org/abs/1907.00509).

This project, which is currently in-progress, aims to compile Remora code into efficient CUDA C code for GPU execution.

An interim report is available in this repository at [./interim-report.pdf](./interim-report.pdf).

## Usage

Make sure you have the build system [Dune](https://github.com/ocaml/dune) installed, as well as all dependencies, which can be installed via opam:

```bash
opam install .
```

### Compile a program

The following command will print out an intermediate representation of a program that is the product of what has been implement so far. 

```bash
dune exec --display=quiet -- remora /path/to/program.remora
```

### Run Tests

```bash
dune runtest
```

### Build

```bash
dune build @all
```
