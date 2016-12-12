#include <unordered_map>
#include <stdio.h>
#include "CountingMap.hpp"

using namespace std;

void CountingMap::insert_occ(int key, int value) {
    this->backing[key][value] += 1L;
}

unsigned long CountingMap::occurrences(int key, int value) {
    return this->backing[key][value];
}

void CountingMap::print(FILE* out) {
    typedef unordered_map<int, unordered_map<int, unsigned long> >::iterator outer_it;
    typedef unordered_map<int, unsigned long>::iterator inner_it;

    for (outer_it m = this->backing.begin(); m != this->backing.end(); ++m) {
        fprintf(out, "= %d:\n", m->first);
        for (inner_it n = m->second.begin(); n != m->second.end(); ++n) {
            fprintf(out, "\t%d (%lu)\n", n->first, n->second);
        }
    }
}
