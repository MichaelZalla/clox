# clox

C17 implementation of the Lox programming language from Robert Nystrom's _"[Crafting Interpreters](https://github.com/munificent/craftinginterpreters)"_; `clox` compiles Lox source code into a bytecode format that is executed by a portable virtual machine.

## Building

```bash
# Build binary.
/usr/bin/gcc \
    -Wall `# Report all warnings.` \
    -fcolor-diagnostics `# Enable colors in diagnostics.` \
    -g `# Generate source-level debug information.` \
	src/main.c `# Report all warnings.` \
    -o ./build/debug/arm64-apple-darwin/main `# Output`

# Generate assembly.
gcc \
	-g `# Generate source-level debug information.` \
	-c `# Only run preprocess, compile, and assemble steps.` \
	-o build/debug/arm64-apple-darwin/main.o `# Output` \
	src/main.c

# Generate disassembly.
objdump \
	-d `# Disassemble.` \
	-M intel `# Emit Intel-style disassembly.` \
	-S build/debug/arm64-apple-darwin/main.o `# Output` \
		> build/debug/arm64-apple-darwin/main.s
```
