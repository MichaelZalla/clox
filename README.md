# clox

C17 implementation of the Lox programming language from Robert Nystrom's _"[Crafting Interpreters](https://github.com/munificent/craftinginterpreters)"_; `clox` compiles Lox source code into a bytecode format that is executed by a portable virtual machine.

## Building

```bash
# Builds clox.
make clox

# Generates assembly (main.c only).
make clox.o:

# Generates disassembly (main.c only).
make clox.s
```
