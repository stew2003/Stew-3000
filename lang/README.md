

### Install
First, [install `opam`.](https://opam.ocaml.org/doc/Install.html)

Then [install OCaml.](https://ocaml.org/docs/install.html)

Make sure necessary packages are installed
```bash
opam install dune
opam install ocamlformat
opam install merlin
```

Get the following VSCode extensions:
- [OCaml and Reason IDE](https://marketplace.visualstudio.com/items?itemName=freebroccolo.reasonml)
- [OCaml Formatter](https://marketplace.visualstudio.com/items?itemName=badochov.ocaml-formatter)

Add these to your VSCode settings: (replace paths with your own)
```
"reason.path.ocamlmerlin": "/home/tcastleman/.opam/4.11.1/bin/ocamlmerlin",
"reason.diagnostics.tools": ["merlin"],
"reason.codelens.enabled": true,
"ocaml-formatter.ocamlformat-path": "/home/tcastleman/.opam/4.11.1/bin/ocamlformat",
"[ocaml]": {
    "editor.defaultFormatter": "badochov.ocaml-formatter",
    "editor.formatOnSave": true,
}
```

### Build & Run
To see if it worked, run in this directory:
```bash
dune exec bin/main.exe
```

You should see
```
hello!
```