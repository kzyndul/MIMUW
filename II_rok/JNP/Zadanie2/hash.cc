#include <unordered_map>
#include <unordered_set>
#include <functional>
#include <iostream>
#include <cstdarg>
#include "hash.h"

using namespace std;

namespace 
{
    #ifdef NDEBUG
        bool const debug = false;
    #else
        bool const debug = true;
    #endif

    /**
     * Struktura spełniająca wymagania unordered seta.
     */
    struct vectorHash 
    {
        jnp1::hash_function_t vector_hash_function;
        vectorHash(jnp1::hash_function_t f) 
        {
            vector_hash_function = f;
        }
        size_t operator()(const vector <uint64_t>& v1) const noexcept 
        {
        return (*vector_hash_function)(&v1[0], v1.size());
        }
    };

    struct vectorComparison 
    {
        vectorComparison(){}
        bool operator()( const vector <uint64_t>& v1, const vector <uint64_t>& v2) const 
        {
            return equal(v1.begin(), v1.end(), v2.begin());
        };
    };
    
    /**
     * Funkcja pomocnicza rozwiązująca problem static initialization order fiasco.
     */
    unordered_map <unsigned long, unordered_set <vector <uint64_t>, vectorHash, vectorComparison >>& hashmap() 
    {
        static unordered_map <unsigned long, unordered_set <vector <uint64_t>, vectorHash, vectorComparison >> hash_map;
        return hash_map;
    }

    /**
     * Funkcja pomocnicza rozwiązująca problem static initialization order fiasco.
     */
    unsigned long& next_free_id() 
    {
        static unsigned long next_free_id = 0;
        return next_free_id;
    }

    vector <uint64_t> series_to_vector(uint64_t const * seq, size_t size) 
    {
        vector <uint64_t> vec;
        for (size_t i = 0; i < size; i++)
            vec.push_back(seq[i]);
        return vec;
    }

    /**
     * Funkcje pomocnicze służące do wypisywania komunikatów diagnostycznych.
     */
    void print_seq(uint64_t const * seq, size_t size) 
    {
        if (seq != NULL) 
        {
            cerr << "\"";
            if (size != 0)
                cerr << seq[0];
            for (size_t i = 1; i < size; i++)
                cerr << " " << seq[i];
            cerr << "\"";
        } 
        else 
        {
            cerr << "NULL";
        }
    }


    void print_function(int which) 
    {
        switch (which) 
        {
            case 0:
                cerr << "hash_create";
                break;
            case 1:
                cerr << "hash_delete";
                break;
            case 2:
                cerr << "hash_size";
                break;
            case 3:
                cerr << "hash_insert";
                break;
            case 4:
                cerr << "hash_remove";
                break;
            case 5:
                cerr << "hash_clear";
                break;
            case 6:
                cerr << "hash_test";
            default:
                break;
        }
    }

    void print_arg(int which, jnp1::hash_function_t hash_function) 
    {
        print_function(which);
        cerr << "(" << &hash_function << ")" << endl;
    }

    void print_arg(int which, unsigned long id) 
    {
        print_function(which);
        cerr << "(" << id << ")" << endl;
    }

    void print_arg(int which, unsigned long id, uint64_t const * seq, size_t size) 
    {
        print_function(which);
        cerr << "(" << id << ", ";
        print_seq(seq, size);
        cerr << ", " << size << ")" << endl;
    }

    void print_create(int which, unsigned long id)
    {
        print_function(which);
        cerr << ": hash table #" << id << " created" << endl;
    }

    void print_delete(int which, unsigned long id, int state)
    {
        print_function(which);
        switch (state) 
        {
            case 1:
                cerr << ": hash table #" << id << " deleted" << endl;
                break;
            case 0:
                cerr << ": hash table #" << id << " does not exist" << endl;
            default:
                break;
        }
    }

    void print_size(int which, unsigned long id, int state) 
    {
        print_function(which);
        if (state == -1) 
        {
            cerr << ": hash table #" << id << " does not exist" << endl;   
        } 
        else 
        {
            cerr << ": hash table #" << id << " contains " << state << " element(s)" << endl;
        }
    }

    void print_clear(int which, unsigned long id, int state) 
    {
        print_function(which);
        switch (state) 
        {
            case 1:
                cerr << ": hash table #" << id << " cleared" << endl;
                break;
            case 0:
                cerr << ": hash table #" << id << " does not exist" << endl;
                break;
            case 2:
                cerr << ": hash table #" << id << " was empty" << endl;
                break;
            default:
                break;
        }
    }

    void print_form(int which, int state)
    {
        switch (state) 
        {
            case 1:
                switch (which) 
                {
                    case 3:
                        cerr<< " was present" << endl;
                        break;
                    case 4:
                        cerr << " was not present" << endl;
                        break;
                    case 6:
                        cerr << " is not present" << endl;
                        break;
                    default:
                        break;
                }
                break;

            case 2:
                switch (which) 
                {
                    case 3:
                        cerr << " inserted" << endl;
                        break;
                    case 4:
                        cerr << " removed" << endl;
                        break;
                    case 6:
                        cerr << " is present" << endl;
                        break;
                    default:
                        break;
                }
                break;
        
            default:
                break;
        }
    }

    void print_pom(int which, unsigned long id, uint64_t const * seq, size_t size, int state)
    {
        if (state == 0 || (seq != NULL && size))
            print_function(which);
        switch (state) 
        {
            case 0:
                cerr << ": hash table #" << id << " does not exist" << endl;
                break;
            case 1:
                cerr << ": hash table #" << id << ", sequence ";
                print_seq(seq, size);
                print_form(which, state);
                break;
            case 2:
                cerr << ": hash table #" << id << ", sequence ";
                print_seq(seq, size);
                print_form(which, state);
                break;
            default:
                break;
        }

        if (seq == NULL) 
        {
            print_function(which);
            cerr << ": invalid pointer (NULL)" << endl;
        }

        if (!size) 
        {
            print_function(which);
            cerr << ": invalid size (0)" << endl;
        }
    }

}

namespace jnp1
{

unsigned long hash_create(hash_function_t hash_function) 
{
    next_free_id()++;

    if (debug) 
    {
        print_arg(0, hash_function);
        print_create(0, next_free_id());
    }

    hashmap().insert({next_free_id(), unordered_set<vector <uint64_t>, vectorHash, vectorComparison>
                    (0, vectorHash(hash_function), vectorComparison())});
    return next_free_id();
}

void hash_delete(unsigned long id) 
{
    auto temp = hashmap().erase(id);

    if (debug) 
    {
        print_arg(1, id);
        print_delete(1, id, temp);
    }
    return;
}

size_t hash_size(unsigned long id) 
{

    if (debug)
        print_arg(2, id);

    if (hashmap().find(id) == hashmap().end()) 
    {
        if (debug)
            print_size(2, id, -1);
        return 0;
    }
    auto temp = (hashmap().find(id)->second).size();
    if (debug)
        print_size(2, id, temp);
    return temp;
}

bool hash_insert(unsigned long id, uint64_t const * seq, size_t size) 
{

    if (debug)
        print_arg(3, id, seq, size);

    if (seq == nullptr || !size) 
    {
        if (debug)
            print_pom(3, id, seq, size, -1);
        return false;
    }

    auto id_hashmap = hashmap().find(id);

    if (id_hashmap == hashmap().end()) 
    {
        if (debug)
            print_pom(3, id, seq, size, 0);
        return false;
    }

    // Sprawdzam czy takiego ciągu nie ma już w tablicy.
    if ((id_hashmap->second).find(series_to_vector(seq, size)) != (id_hashmap->second).end()) 
    {
        if (debug)
            print_pom(3, id, seq, size, 1);
        return false;
    }

    (id_hashmap->second).insert(series_to_vector(seq, size));

    if (debug)
        print_pom(3, id, seq, size, 2);

    return true;
}

bool hash_remove(unsigned long id, uint64_t const * seq, size_t size) 
{
    if (debug)
        print_arg(4, id, seq, size);

    if (seq == nullptr || !size) 
    {
        if (debug)
            print_pom(4, id, seq, size, -1);
        return false;
    }

    auto id_hashmap = hashmap().find(id);    

    if (id_hashmap == hashmap().end()) 
    {
        if (debug)
            print_pom(4, id, seq, size, 0);
        return false;
    }

    // Sprawdzam czy taki ciąg jest w tablicy.
    if ((id_hashmap->second).find(series_to_vector(seq, size)) == (id_hashmap->second).end()) 
    {
        if (debug)
            print_pom(4, id, seq, size, 1);
        return false;
    }
    id_hashmap->second.erase(series_to_vector(seq, size));
    if (debug)
            print_pom(4, id, seq, size, 2);
    return true;    
}

void hash_clear(unsigned long id) 
{

    if (debug)
        print_arg(5, id);

    auto id_hashmap = hashmap().find(id);
    if (id_hashmap == hashmap().end())
    {
        if (debug)
            print_clear(5, id, 0);
        return;
    }
    if (debug) 
    {
        size_t pom = id_hashmap->second.size();
        if (!pom)
            print_clear(5, id, 2);
        else
            print_clear(5, id, 1);
    }
    id_hashmap->second.clear();
    return;
}

bool hash_test(unsigned long id, uint64_t const * seq, size_t size) 
{
    if (debug)
        print_arg(6, id, seq, size);

    if (seq == nullptr || !size) 
    {
        if (debug)
            print_pom(6, id, seq, size, -1);
        return false;
    }

    auto id_hashmap = hashmap().find(id);

    if (id_hashmap == hashmap().end()) 
    {
        if (debug)
            print_pom(6, id, seq, size, 0);
        return false;
    }

    // Sprawdzam czy taki ciag jest w tablicy.
    if (id_hashmap->second.find(series_to_vector(seq, size)) == id_hashmap->second.end()) 
    {
        if (debug)
            print_pom(6, id, seq, size, 1);
        return false;
    }
    if (debug)
        print_pom(6, id, seq, size, 2);
    return true;
}

}