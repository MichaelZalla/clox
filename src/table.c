#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table)
{
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void freeTable(Table *table)
{
	FREE_ARRAY(Entry, table->entries, table->capacity);

	initTable(table);
}

static Entry *findEntry(Entry *entries, int capacity, ObjString *key)
{
	uint32_t index = key->hash & (capacity - 1);

	Entry *tombstone = NULL;

	// Open addressing.

	// Note: Because we enforce a maximum load factor, we don't run any risk here
	// of looping infinitely and never finding an open index (bucket).
	for (;;)
	{
		Entry *entry = &entries[index];

		if (entry->key == NULL)
		{
			// If we encounter a completely empty entry, and we haven't returned yet,
			// then we know that `key` is not present in the hash table.
			if (IS_NIL(entry->value))
			{
				// Now that we know that `key` is not present, we can return (reuse) the
				// tombstone entry, if one was found; otherwise, we return (use) this
				// entry.
				return tombstone != NULL ? tombstone : entry;
			}
			else
			{
				// Encountered a tombstone; remember this entry in case we determine
				// later that we can re-use it.
				if (tombstone == NULL)
				{
					tombstone = entry;
				}
			}
		}
		// (Compares pointers).
		else if (entry->key == key)
		{
			// The entry (key) exists!
			return entry;
		}

		// Resolves collision with linear probing.
		index = (index + 1) & (capacity - 1);
	}
}

static void adjustCapacity(Table *table, int newCapacity)
{
	Entry *entries = ALLOCATE(Entry, newCapacity);

	for (int i = 0; i < newCapacity; i++)
	{
		entries[i].key = NULL;
		entries[i].value = NIL_VAL;
	}

	// We're re-accumulate a new entry count (ignores old tombstones).
	table->count = 0;

	for (int i = 0; i < table->capacity; i++)
	{
		Entry *oldEntry = &table->entries[i];

		if (oldEntry->key == NULL)
		{
			// Empty entry or tombstone entry.
			continue;
		}

		Entry *newEntry = findEntry(entries, newCapacity, oldEntry->key);

		newEntry->key = oldEntry->key;
		newEntry->value = oldEntry->value;

		table->count += 1;
	}

	FREE_ARRAY(Entry, table->entries, table->capacity);

	table->entries = entries;
	table->capacity = newCapacity;
}

bool tableGet(Table *table, ObjString *key, Value *value)
{
	if (table->count == 0)
	{
		return false;
	}

	Entry *entry = findEntry(table->entries, table->capacity, key);

	if (entry->key == NULL)
	{
		return false;
	}

	*value = entry->value;

	return true;
}

bool tableSet(Table *table, ObjString *key, Value value)
{
	// Grows our table, if necessary, to honor a maximum load factor.
	//
	//   loadFactor = (count / capacity)
	//
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD)
	{
		int newCapacity = NEXT_CAPACITY(table->capacity);

		adjustCapacity(table, newCapacity);
	}

	// Determines at which index this key exists, or would exist, in our table.
	Entry *entry = findEntry(table->entries, table->capacity, key);

	// Checks if the key was already present.
	bool isNewKey = entry->key == NULL;

	if (isNewKey && IS_NIL(entry->value))
	{
		// Preserves the original count if we overwrote a value for an existing key,
		// or if we overwrote a tombstone entry.

		table->count += 1;
	}

	entry->key = key;
	entry->value = value;

	return isNewKey;
}

bool tableDelete(Table *table, ObjString *key)
{
	if (table->count == 0)
	{
		return false;
	}

	// Checks if the key is present.

	Entry *entry = findEntry(table->entries, table->capacity, key);

	if (entry->key == NULL)
	{
		return false;
	}

	// Replace the entry with a tombstone.
	entry->key = NULL;
	entry->value = BOOL_VAL(true);

	// Note: We don't decrement the table's `count` field.

	return true;
}

void tableAddAll(Table *from, Table *to)
{
	for (int i = 0; i < from->capacity; i++)
	{
		Entry *fromEntry = &from->entries[i];

		if (fromEntry->key != NULL)
		{
			tableSet(to, fromEntry->key, fromEntry->value);
		}
	}
}

ObjString *tableFindString(Table *table, const char *chars, int length, uint32_t hash)
{
	if (table->count == 0)
	{
		return NULL;
	}

	uint32_t index = hash & (table->capacity - 1);

	for (;;)
	{
		Entry *entry = &table->entries[index];

		if (entry->key == NULL && IS_NIL(entry->value))
		{
			// We've found an empty (non-tombstone) entry.
			return NULL;
		}
		else if (entry->key->length == length && entry->key->hash == hash)
		{
			// We've found an entry with a matching length and hash.
			if (memcmp(entry->key->chars, chars, length) == 0)
			{
				// We've found an exact string match.
				return entry->key;
			}
		}

		index = (index + 1) & (table->capacity - 1);
	}
}

void tableRemoveWhite(Table *table)
{
	for (int i = 0; i < table->count; i++)
	{
		Entry *entry = &table->entries[i];

		if (entry->key != NULL && !entry->key->obj.isMarked)
		{
			// Note that `tableDelete()` leaves the table's `count` unchanged.
			tableDelete(table, entry->key);
		}
	}
}

void markTable(Table *table)
{
	// Iterates through entries and marks their key and value.
	for (int i = 0; i < table->count; i++)
	{
		Entry *entry = &table->entries[i];

		markObject((Obj *)entry->key);
		markValue(entry->value);
	}
}
