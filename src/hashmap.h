/*H*
 * 
 * FILENAME: hashmap.h
 * DESCRIPTION: 
 * AUTHORS: José Antonio Riaza Valverde
 * DATE: 26.01.2019
 * 
 *H*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define HASHSIZE 256



#ifndef LOGIC_HASHMAP_H
#define LOGIC_HASHMAP_H

typedef struct Hashmap {
    struct Hashmap *next;
    unsigned char *key;
    int value;
} Hashmap;

#endif



/** Calculate the hash for lookup an element. */
unsigned long hashmap_hash(unsigned char *key);

/** Look an element in a hashmap. */
int hashmap_lookup(Hashmap **h, unsigned char *key);

/** Insert an element into a hashmap. */
void hashmap_append(Hashmap **h, unsigned char *key, int value);

/** Allocate a new hashmap in memory. */
Hashmap **hashmap_alloc();

/** Deallocate a hashmap in memory. */
void hashmap_free(Hashmap **h);
