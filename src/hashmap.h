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



#ifndef LOGIC_HASHMAP_H
#define LOGIC_HASHMAP_H

typedef struct HashmapRegister {
    struct HashmapRegister *next;
    unsigned char *key;
    int value;
} HashmapRegister;

typedef struct Hashmap {
    HashmapRegister **map;
    int nb_registers;
} Hashmap;

#endif



/**
  *
  * This function calculates the hash for lookup an element
  * into a map.
  *
  **/
unsigned long hashmap_hash(Hashmap *map, unsigned char *key);

/**
  *
  * This function looks up an element into a map.
  *
  **/
int hashmap_lookup(Hashmap *map, unsigned char *key);

/**
  *
  * This function adds an element into a map.
  *
  **/
void hashmap_append(Hashmap *map, unsigned char *key, int value);

/**
  * 
  * This function creates a map, returning a pointer
  * to a newly initialized Hashmap struct.
  * 
  **/
Hashmap *hashmap_alloc(int nb_register);

/**
  * 
  * This function frees a previously allocated map.
  * The strings and register underlying the map will 
  * also be deallocated.
  * 
  **/
void hashmap_free(Hashmap *map);

/**
  * 
  * This function prints for the standard output
  * the whole map.
  * 
  **/
void hashmap_print(Hashmap *map);