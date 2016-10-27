#include <stdio.h>
#include <stdlib.h>

// Dummy GC that allocates memory in 1G chunks and never frees.

void* start;
void* last;

#define CHUNK (1024*1024*1024)

void scalanative_init() {
    start = malloc(CHUNK);
    last = start;
}

void* scalanative_alloc_raw(size_t size) {
    if (last + size < start + CHUNK) {
        void* alloc = last;
        last += size;
        return alloc;
    } else {
        scalanative_init();
        return scalanative_alloc_raw(size);
    }
}

void* scalanative_alloc_raw_atomic(size_t size) {
    return scalanative_alloc_raw(size);
}

void* scalanative_alloc(void* info, size_t size) {
    void** alloc = (void**) scalanative_alloc_raw(size);
    *alloc = info;
    return (void*) alloc;
}


