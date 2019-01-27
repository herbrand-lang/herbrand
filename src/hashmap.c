/*H*
 * 
 * FILENAME: hashmap.c
 * DESCRIPTION: 
 * AUTHORS: José Antonio Riaza Valverde
 * DATE: 26.01.2019
 * 
 *H*/



#include "hashmap.h"



/** Calculate the hash for an element. */
unsigned long hashmap_hash(unsigned char *key) {
    unsigned long hash = 5381;
    int c;
    while(c = *key++)
        hash = ((hash << 5) + hash) + c;
    return hash % HASHSIZE;
}

/** Look an element in a hashmap. */
int hashmap_lookup(Hashmap **h, unsigned char *key) {
    Hashmap *p;
	for(p = h[hashmap_hash(key)]; p != NULL; p = p->next)
		if(strcmp(p->key, key) == 0)
			return p->value;
	return -1;
}

/** Insert an element into a hashmap. */
void hashmap_append(Hashmap **h, unsigned char *key, int value) {
	int hash = hashmap_hash(key);
	Hashmap *p = h[hash], *q = malloc(sizeof(Hashmap)), *r;
	q->next = NULL;
	q->key = key;
	q->value = value;
	if(p == NULL) {
		h[hash] = q;
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

/** Allocate a new hashmap in memory. */
Hashmap **hashmap_alloc()
{
	int i;
	Hashmap **h = malloc(HASHSIZE * sizeof(Hashmap));
	if(h == NULL)
		return NULL;
	for(i = 0; i < HASHSIZE; i++)
		h[i] = NULL;
	return h;
}

/** Deallocate a hashmap in memory. */
void hashmap_free(Hashmap **h) {
	int i;
	for(i = 0; i < HASHSIZE; i++)
		if(h[i] != NULL)
			free(h[i]);
	free(h);
}