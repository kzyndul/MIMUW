#ifndef FIFO_H
#define FIFO_H


#include <cstdint>
#include <mutex>
#include "utility.h"
#include <cstring>
#include <condition_variable>
#include <deque>

/**
 * Klasa przechowuje ostatnie FSIZE paczek, które zostały wysłane.
 */
class Fifo {
    std::size_t pack_size;
    std::size_t max_size;
    uint64_t byte_num = 0;

    std::deque<std::vector<char>> queue = std::deque<std::vector<char>>();

    std::mutex mutex;
    uint8_t off_set = 16;
    uint64_t session_id;


    void erese()
    {
        byte_num += pack_size;
        queue.pop_front();
    }

    void add(char *pack)
    {
        std::vector<char> tmp;
        for (size_t i = 0; i < pack_size; ++i)
        {
            tmp.push_back(pack[i + off_set]);
        }
        queue.push_back(tmp);
    }


public:

    /**
     * Dodaje paczkę. Zapisuje ją na końcu kolejki, jeżeli nie ma miejsca to usuwa pierwszą wartość w kolejce.
     *
     * @param pack      paczka do zapisania.
     */
    void save(char *pack)
    {
        mutex.lock();
        if (queue.size() >= max_size)
        {
            erese();
        }

        auto curentSize = queue.size();
        try
        {
            add(pack);
        }
        catch (std::exception &e)
        {
            queue.resize(curentSize);
            max_size = curentSize;
            erese();
            add(pack);
        }
        mutex.unlock();
    }

    /**
     * Kopiuje paczkę o numerze @param byte_num na @param pack.
     * @param pack      buffor który przetrzymuje nową paczkę
     * @param byte_num  numer paczki ktorą należy skopiować
     * @return          zwraca true jeżeli paczka była w kolejce wpp false.
     */
    bool copy(char *pack, uint64_t pack_num)
    {
        mutex.lock();
        if (byte_num <= pack_num)
        {
            auto current_num = byte_num;
            for (size_t i = 0; i < queue.size(); ++i)
            {

                if (current_num == pack_num)
                {

                    pack_num = htobe64(pack_num);
                    memcpy(pack + sizeof(uint64_t), &pack_num, sizeof(uint64_t));
                    memcpy(pack, &session_id, sizeof(uint64_t));
                    for (size_t j = 0; j < pack_size; ++j)
                    {
                        pack[j + off_set] = queue[i][j];
                    }
                    mutex.unlock();
                    return true;
                }
                current_num += pack_size;
            }
        }
        mutex.unlock();
        return false;
    }


    size_t get_pack_size()
    {
        return pack_size + off_set;
    }


    Fifo &operator=(const Fifo &other)
    {
        pack_size = other.pack_size;
        max_size = other.max_size;
        session_id = other.session_id;
        return *this;
    }


    Fifo(Fifo const &other) : pack_size(other.pack_size), max_size(other.max_size), session_id(other.session_id)
    {

    }

    Fifo(std::size_t pack_size, std::size_t max_size, uint64_t session_id) :
            pack_size(pack_size),
            max_size(max_size),
            session_id(session_id)
    {}


};


#endif
