#ifndef CountingMap_hpp
#define CountingMap_hpp

#include <map>
#include <stdio.h>

using namespace std;

class CountingMap {
private:
    map<char*, map<int, unsigned long> > backing;

public:
    void insert_occ(char* key, int value);
    bool contains(char* key);
    bool contains_pair(char* key, int value);
    unsigned long occurrences(char* key, int value);
    void print(FILE* out);
};

#endif /* CountingMap_hpp */
