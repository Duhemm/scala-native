#ifndef CountingMap_hpp
#define CountingMap_hpp

#include <unordered_map>
#include <stdio.h>

using namespace std;

class CountingMap {
private:
    unordered_map<int, unordered_map<int, unsigned long> > backing;

public:
    void insert_occ(int key, int value);
    unsigned long occurrences(int key, int value);
    void print(FILE* out);
};

#endif /* CountingMap_hpp */
