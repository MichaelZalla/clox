src := \
	src/memory.c \
	src/chunk.c \
	src/value.c \
	src/object.c \
	src/table.c \
	src/compiler.c \
	src/scanner.c \
	src/vm.c \
	src/debug.c \
	src/main.c \

bin := clox

# Builds clox.
clox:
	gcc \
		-Wall `# All warnings.` \
		-O0 `# No optimizations.` \
		-fcolor-diagnostics `# Enables colors in diagnostic output.`\
		-g $(src) \
		-o ./build/debug/arm64-apple-darwin/$(bin)

# Generates assembly (main.c only).
clox.o:
	gcc \
		-g `# Generate source-level debug information.` \
		-c `# Only run preprocess, compile, and assemble steps.` \
		-o build/debug/arm64-apple-darwin/$(bin).o `# Output` \
		src/main.c

# Generates disassembly (main.c only).
clox.s: clox.o
	objdump \
		-d `# Disassemble.` \
		-M intel `# Emit Intel-style disassembly.` \
		-S build/debug/arm64-apple-darwin/$(bin).o `# Output` \
			> build/debug/arm64-apple-darwin/$(bin).s

# Cleans our build folder.
clean:
	find ./build -type f | xargs -n1 rm
