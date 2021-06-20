# lang
This folder contains programs for doing language-related things. It houses:

- the compiler, which converts 3000 source programs to 3000 asm
- the assembler, which converts 3000 asm to binary
- the emulator, which emulates the hardware and allows us to run 3000 asm programs in software

Each of these exposes a command-line interface, as specified below.

### Compiler
To invoke the compiler, run
```bash
dune exec bin/compile.exe <source-file> <target-file>
```

### Assembler
To invoke the assembler, run
```bash
dune exec bin/assemble.exe <asm-file> <binary-file>
```

### Emulator
To invoke the emulator, run
```bash
dune exec bin/emulate.exe <asm-file>
```

### Tests
To run all tests, run 
```bash
dune runtest -f
```

### Requirements

This project expects OCaml >= 4.11.1 and dune >= 2.8.
