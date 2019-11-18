/*H*
 * 
 * FILENAME: hashmap.h
 * DESCRIPTION: 
 * AUTHORS: José Antonio Riaza Valverde
 * DATE: 18.11.2019
 * 
 *H*/

#ifndef HERBRAND_HASHMAP_H
#define HERBRAND_HASHMAP_H

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

typedef struct HashmapRegister {
    struct HashmapRegister *next;
    wchar_t *key;
    int value;
} HashmapRegister;

typedef struct Hashmap {
    HashmapRegister **map;
    int nb_registers;
} Hashmap;

/**
  *
  * This function calculates the hash for lookup an element
  * into a map of certain size.
  *
  **/
unsigned long hashmap_function(int size, wchar_t *key);

/**
  *
  * This function calculates the hash for lookup an element
  * into a map.
  *
  **/
unsigned long hashmap_hash(Hashmap *map, wchar_t *key);

/**
  *
  * This function looks up an element into a map.
  *
  **/
int hashmap_lookup(Hashmap *map, wchar_t *key);

/**
  *
  * This function adds an element into a map.
  *
  **/
void hashmap_append(Hashmap *map, wchar_t *key, int value);

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

#endif