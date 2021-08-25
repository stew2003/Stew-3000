# lang
This folder contains programs for doing language-related things. It houses:

- the compiler, which converts 3000 source programs to 3000 asm
- the assembler, which converts 3000 asm to binary
- the emulator, which emulates the hardware and allows us to run 3000 asm programs in software

## Installing

If you'd just like to install the Stew 3000 compiler/assembler/emulator
on your machine, you'll have to build it from the source. To do this, 
you'll need OCaml, Opam, Dune, and a few dependencies.

### Dependencies

> Note: This project expects OCaml >= 4.11.1 and dune >= 2.8.

[Install Opam](https://opam.ocaml.org/doc/Install.html)

[Install OCaml](https://ocaml.org/docs/install.html)

Once these are installed, you can use `opam` to install a few dependencies:
```bash
opam install dune
eval $(opam env)
opam install core ounit2 ppx_blob ppx_let menhir
```

### Building the language tools

Once you've installed the dependencies, you can run the following from 
the project root:
```bash
./scripts/build-release
```
This builds the compiler, assembler, and emulator, and copies them into
your `/usr/local/bin` folder. Because this folder is usually owned by 
root, you may have to enter your root password for this script to complete
successfully.

Once this finishes, restart your shell and you should be able to use `stew3c` 
(compiler), `stew3s` (assembler), and `stew3e` (emulator) all directly from
your shell. 

> If these commands aren't recognized, make sure that `/usr/local/bin` exists and is added to your `$PATH`.

## Working on language tools

If you'd like to make changes to the language tools yourself, you'll find the following commands useful:

```bash
# builds everything
dune build
```

```bash
# builds and runs the compiler
dune exec bin/compile.exe <source-file> <target-file>
```

```bash
# builds and runs the assembler
dune exec bin/assemble.exe <asm-file> <binary-file>
```

```bash
# builds and runs the emulator
dune exec bin/emulate.exe <asm-file>
```

These will rebuild the changed OCaml files if necessary and invoke the corresponding tool. You may also want to check out the scripts in the `scripts/` directory for abbreviating these commands.

### Tests
To run all tests, run 
```bash
dune runtest -f
```