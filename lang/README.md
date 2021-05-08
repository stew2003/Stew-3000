Language tools.

## Assembler
To invoke the assembler, run
```bash
dune exec bin/assemble.exe <asm-file> <binary-file>
```

## Emulator
To invoke the emulator, run
```bash
dune exec bin/emulate.exe <asm-file>
```

## Tests
To run all tests, run 
```bash
dune runtest -f
```

## Requirements

This project expects OCaml >= 4.11.1 and dune >= 2.8.
