#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "debug.h"

static void repl()
{
	char line[1024];

	for (;;)
	{
		printf("> ");

		if (!fgets(line, sizeof(line), stdin))
		{
			printf("\n");
			break;
		}

		interpret(line);
	}
}

static char *readFile(const char *path)
{
	FILE *file = fopen(path, "rb"); // binary for reading

	if (file == NULL)
	{
		fprintf(stderr, "Failed to read file \"%s\".\n", path);
		exit(74);
	}

	fseek(file, 0L, SEEK_END);	   // Seeks to the end of the file.
	size_t fileSize = ftell(file); // Gets current file position indicator
								   // (as a byte offset).
	rewind(file);				   // Resets the file position indicator.

	// Allocates heap memory to store a null-terminated (C-style) string.
	char *buffer = (char *)malloc(fileSize + 1);

	if (buffer == NULL)
	{
		fprintf(stderr, "Not enough memory to read file \"%s\".\n", path);
		exit(74);
	}

	// Copy bytes from our open file description into our buffer.
	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);

	if (bytesRead < fileSize)
	{
		fprintf(stderr, "Could not read entirety of file \"%s\".\n", path);
		exit(74);
	}

	// Null terminator
	buffer[bytesRead] = '\0';

	// Release the file resource.
	fclose(file);

	return buffer;
}

static void runFile(const char *path)
{
	char *source = readFile(path);

	InterpretResult result = interpret(source);

	free(source);

	if (result == INTERPRET_COMPILER_ERROR)
		exit(65);
	if (result == INTERPRET_RUNTIME_ERROR)
		exit(70);
}

int main(int argc, const char *argv[])
{
	initVM();

	if (argc == 1)
	{
		repl();
	}
	else if (argc == 2)
	{
		runFile(argv[1]);
	}
	else
	{
		fprintf(stderr, "Usage: clox [path]\n");

		exit(64); // man sysexits
	}

	freeVM();

	return 0;
}
