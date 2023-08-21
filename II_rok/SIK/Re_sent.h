#ifndef RE_SENT_H
#define RE_SENT_H


#include <cstdint>
#include <mutex>
#include "utility.h"
#include <cstring>
#include <condition_variable>
#include <vector>
#include <queue>
#include <condition_variable>

/**
 * Klasa przechowuje paczki które mają zostać wysłane ponownie.
 *
 * @param to_copy       numery paczek które należy przkopiować z kolejki.
 * @param working       tablica do ktorej sa zapisywane paczki.
 * @param to_sent       tablica z ktorej paczki sa wysyłane.
 */
class Re_sent {
    std::size_t pack_size;
    std::size_t working_end = 0;
    std::size_t working_size = 0;

    std::vector<char> working;
    std::vector<char> to_sent;

    std::queue<uint64_t> to_copy;


    std::mutex mutex_queue;
    std::mutex mutex_to_copy;
    std::condition_variable wait_until_not_empty;


    size_t get_new_size()
    {
        return 2 * working_size + pack_size;
    }

public:

    /**
     * Zapisuje paczkę do struktury, zawsze na ostatnia pozycję. Jeżeli nie ma już miejsca to usuwa ostatnia paczkę.
     *
     * @param pack
     */
    void save(char *pack)
    {
        mutex_queue.lock();
        if (working_end == working_size)
        {
            try
            {
                size_t tmp = get_new_size();
                working.resize(tmp);
                working_size = tmp;
            }
            catch (std::exception &e)
            {
                working_end = working_end - pack_size;
            }
        }

        for (size_t i = 0; i < pack_size; ++i)
        {
            working[working_end + i] = pack[i];
        }
        working_end += pack_size;
        mutex_queue.unlock();
    }

    std::vector<char> &swap()
    {
        mutex_queue.lock();

        working.resize(working_end);
        std::swap(working, to_sent);
        working_size = 0;
        working_end = 0;

        mutex_queue.unlock();

        return to_sent;
    }

    /**
     * Dodaje numery paczek do ponownego przesłania.
     *
     * @param numbers
     */
    void add_to_copy(std::vector<uint64_t> &numbers)
    {
        mutex_to_copy.lock();

        for (auto x: numbers)
        {
            to_copy.push(x);
        }

        mutex_to_copy.unlock();
        wait_until_not_empty.notify_one();
    }

    /**
     * @param end       informacja czy program się zakończył.
     * @return          numer paczki do przkopiowania
     */
    uint64_t pop_to_copy(bool &end)
    {

        std::unique_lock<std::mutex> lk(mutex_to_copy);
        wait_until_not_empty.wait(lk, [this, &end]
        { return !to_copy.empty() || end; });
        if (end)
        {
            return 0;
        }
        auto result = to_copy.front();
        to_copy.pop();

        lk.unlock();
        return result;
    }

    void wake_me()
    {
        wait_until_not_empty.notify_one();
    }


    explicit Re_sent(std::size_t pack_size) : pack_size(pack_size)
    {}


    Re_sent &operator=(const Re_sent &other)
    {
        pack_size = other.pack_size;
        return *this;
    }


    Re_sent(Re_sent const &other) : pack_size(other.pack_size)
    {

    }


};


#endif
