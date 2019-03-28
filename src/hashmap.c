/*H*
 * 
 * FILENAME: hashmap.c
 * DESCRIPTION: 
 * AUTHORS: José Antonio Riaza Valverde
 * DATE: 27.03.2019
 * 
 *H*/



#include "hashmap.h"



/**
  *
  * This function calculates the hash for lookup an element
  * into a map.
  *
  **/
unsigned long hashmap_hash(Hashmap *map, unsigned char *key) {
	int i, j;
	unsigned int byte, crc, mask;
	i = 0;
	crc = 0xFFFFFFFF;
	while(key[i] != '\0') {
		byte = key[i];
		crc = crc^byte;
		for(j = 7; j >= 0; j--) {
			mask = -(crc & 1);
			crc = (crc >> 1)^(0xEDB88320 & mask);
		}
		i++;
	}
	return ~crc % map->nb_registers;
}

/**
  *
  * This function looks up an element into a map.
  *
  **/
int hashmap_lookup(Hashmap *map, unsigned char *key) {
    HashmapRegister *p;
	for(p = map->map[hashmap_hash(map, key)]; p != NULL; p = p->next)
		if(strcmp(p->key, key) == 0)
			return p->value;
	return -1;
}

/**
  *
  * This function adds an element into a map.
  *
  **/
void hashmap_append(Hashmap *map, unsigned char *key, int value) {
	int hash = hashmap_hash(map, key);
	HashmapRegister *p = map->map[hash], *q = malloc(sizeof(HashmapRegister)), *r;
	q->next = NULL;
	q->key = malloc(sizeof(char)*(strlen(key)+1));
	strcpy(q->key, key);
	q->value = value;
	if(p == NULL) {
		map->map[hash] = q;
		return;
	}
	while(p != NULL) {
		if(strcmp(p->key, key) == 0) {
			p->value = value;
			free(q);
			return;
		}
		r = p;
		p = p->next;
	}
	r->next = q;
}

/**
  * 
  * This function creates a map, returning a pointer
  * to a newly initialized Hashmap struct.
  * 
  **/
Hashmap *hashmap_alloc(int nb_registers) {
	int i;
	if(nb_registers == 0)
		nb_registers++;
	Hashmap *map = malloc(sizeof(Hashmap));
	if(map != NULL) {
		map->nb_registers = nb_registers;
		map->map = malloc(sizeof(HashmapRegister*)*nb_registers);
		for(i = 0; i < nb_registers; i++)
			map->map[i] = NULL;
	}
	return map;
}

/**
  * 
  * This function frees a previously allocated map.
  * The strings and register underlying the map will 
  * also be deallocated.
  * 
  **/
void hashmap_free(Hashmap *map) {
	int i;
	HashmapRegister *p, *q;
	for(i = 0; i < map->nb_registers; i++) {
		p = map->map[i];
		while(p != NULL) {
			q = p->next;
			free(p);
			p = q;
		}
	}
	free(map);
}

/**
  * 
  * This function prints for the standard output
  * the whole map.
  * 
  **/
void hashmap_print(Hashmap *map) {
	int i;
	HashmapRegister *p;
	printf("(hashmap (size %d)", map->nb_registers);
	for(i = 0; i < map->nb_registers; i++) {
		p = map->map[i];
		if(p != NULL) {
			printf("\n\t(hash %d", i);
			while(p != NULL) {
				printf("\n\t\t((key \"%s\") (value %d))", p->key, p->value);
				p = p->next;
			}
		}
	}
	printf(")\n");
}